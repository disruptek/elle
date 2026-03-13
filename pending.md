# IO Subsystem: Pending Assessment

Full audit of `src/io/` on `feat/http-module` branch. Covers correctness bugs,
structural debt, and the path to making this subsystem correct, elegant, and
performant. Subsumes issue #548.

## Critical Bugs

### 1. `submit_uring_sendto`: sockaddr dangling pointer

**File:** `uring.rs:303-331`
**Severity:** Memory safety violation (use-after-free)

The sockaddr (`sin` / `sin6`) is stack-allocated inside the `match` arm. A raw
pointer to it is passed into the SQE via `dest_addr()`. But io_uring SQEs are
queued — the kernel reads `dest_addr` *after* `submit_uring_sendto` returns and
the stack frame is gone. The pointer dangles.

This is the same class of bug as the connect sockaddr bug already fixed at line
233 (`alloc(0) + extend_from_slice`). The connect path correctly stashes the
sockaddr bytes in the buffer pool. The sendto path does not.

**Fix:** Stash the sockaddr in a buffer pool allocation, same pattern as
`submit_uring_connect`. The payload is already stashed (lines 324-326); the
sockaddr needs the same treatment, and the SQE must reference the pool buffer's
pointer instead of the stack local.

### 2. `submit_uring_recvfrom`: doesn't capture sender address

**File:** `uring.rs:368-416`
**Severity:** Silent data corruption

This submits `opcode::Recv`, not `opcode::RecvMsg`. A plain `Recv` does not
capture the sender's address. The thread pool path correctly uses
`libc::recvfrom` and encodes the sender address as
`addr_len(4 LE) + sockaddr_storage + payload` in the result data.

The io_uring path returns raw payload bytes with no address metadata. The
completion handler in `completion.rs:128-165` expects the
`addr_len + sockaddr_storage + payload` format. It will either:
- Fail the length check at line 130 and return an error, or
- Parse payload bytes as a sockaddr and return garbage address/port.

Neither outcome is acceptable. The caller gets either a spurious error or a
fabricated source address.

**Fix:** Use `opcode::RecvMsg` with a properly constructed `msghdr` that
includes a sockaddr buffer. Alternatively, after `Recv` completes, call
`getpeername()` on connected UDP sockets — but for unconnected UDP (the common
case for recvfrom), there is no peer. `RecvMsg` is the correct approach.

This also explains the dead overallocation at line 379:
`alloc(count + sizeof(sockaddr_storage) + 4)` followed by `resize(count, 0)`.
Someone intended to receive the sockaddr in the same buffer but never finished
the implementation.

### 3. Connect sockaddr buffer handle leak

**File:** `uring.rs:233` creates a handle; `aio.rs:494` creates another
**Severity:** Unbounded memory leak under sustained connect workload

`submit_uring_connect` allocates `buf_handle` at line 233 for the sockaddr.
This handle is never stored anywhere reachable on completion. `submit_connect`
in `aio.rs:494` creates its *own* `buf_handle = inner.buffer_pool.alloc(0)` and
stores that in `PendingOp.buffer_handle`. On completion, only
`PendingOp.buffer_handle` is released. The sockaddr buffer is orphaned.

Every `tcp/connect` via io_uring leaks one buffer pool slot permanently.

**Fix:** Either:
- (a) Pass the `buf_handle` from `submit_connect` down into
  `submit_uring_connect` so it uses the same handle, or
- (b) Add a `sockaddr_buffer_handle: Option<BufferHandle>` field to
  `PendingOp` and release it on completion, or
- (c) Restructure so `submit_uring_connect` returns the handle it allocated and
  `submit_connect` stores that as the primary `buffer_handle` (don't allocate
  a second one).

Option (c) is cleanest — one allocation, one handle, one release path.

## Structural Problems

### 4. `PendingOp` is a god-struct

**File:** `aio.rs:50-68`

8 fields, most `None`/meaningless for most operations:

| Field | Accept | Connect | Stream R/W | Sleep | Shutdown |
|-------|--------|---------|------------|-------|----------|
| `op` | ✓ | ✓ | ✓ | ✓ | ✓ |
| `port_key` | ✓ | dummy(-1) | ✓ | dummy(-1) | ✓ |
| `port` | ✓ | NIL | ✓ | NIL | ✓ |
| `buffer_handle` | dummy | dummy | ✓ | dummy | dummy |
| `listener_kind` | ✓ | None | None | None | None |
| `connect_addr` | None | ✓ | None | None | None |
| `timeout` | ✓ | ✓ | ✓ | None | ✓ |
| `connect_fd` | None | ✓ (uring) | None | None | None |

Five of eight fields are dummies or None for most operation types. The
connect bug already fixed (forgetting to set `connect_fd` on the thread
pool path in `drain_network_completions`) is a direct consequence — the
compiler can't catch a missing field when everything is `Option`.

**Fix:** Make `PendingOp` an enum:

```rust
enum PendingOp {
    Stream {
        op: StreamOp,        // ReadLine | Read { count } | ReadAll | Write | Flush
        port_key: PortKey,
        port: Value,
        buffer_handle: BufferHandle,
        timeout: Option<Duration>,
    },
    Accept {
        port_key: PortKey,
        port: Value,
        listener_kind: PortKind,
        buffer_handle: BufferHandle,
        timeout: Option<Duration>,
    },
    Connect {
        addr: ConnectAddr,
        fd: RawFd,                       // uring: pre-created socket; thread pool: set on completion
        sockaddr_handle: BufferHandle,   // uring: pool slot for sockaddr bytes
        timeout: Option<Duration>,
    },
    SendTo {
        port_key: PortKey,
        port: Value,
        buffer_handle: BufferHandle,
        timeout: Option<Duration>,
    },
    RecvFrom {
        port_key: PortKey,
        port: Value,
        count: usize,
        buffer_handle: BufferHandle,
        timeout: Option<Duration>,
    },
    Shutdown {
        port_key: PortKey,
        port: Value,
        buffer_handle: BufferHandle,
        timeout: Option<Duration>,
    },
    Sleep {
        // no port, no buffer, no timeout (duration is in the SQE)
    },
}
```

Each variant carries exactly what the completion handler needs. The compiler
enforces field presence. The Connect variant's `sockaddr_handle` eliminates
bug #3 by construction.

### 5. Duplicate sockaddr formatting

**Files:** `completion.rs:188-282` and `backend/network.rs:368-432`

Four functions doing essentially the same thing:

| Function | File | Purpose |
|----------|------|---------|
| `format_sockaddr` | `completion.rs` | Format sockaddr for async completions |
| `parse_sockaddr` | `completion.rs` | Parse sockaddr to (addr, port) for async |
| `format_sockaddr` | `backend/network.rs` | Format sockaddr for sync completions |
| `parse_sockaddr_ip` | `backend/network.rs` | Parse sockaddr to (addr, port) for sync |

The implementations differ subtly. `completion.rs` uses `ip.to_le_bytes()` for
IPv4 formatting; `backend/network.rs` uses `Ipv4Addr::from(u32::from_be(...))`.
Same result, different code paths, divergence waiting to become a bug.

**Fix:** One `sockaddr` module in `src/io/` with `format()` and `parse()`
functions. Both backends import from there.

### 6. Two completion processing paths with different semantics

**Files:** `completion.rs:process_raw_completion` and `aio.rs:process_with_buffer`

`process_raw_completion` is the io_uring-aware path — handles Accept (result =
new fd), Connect (result = 0), Sleep (-ETIME), and stream ops.

`process_with_buffer` is the thread-pool-aware path — wraps
`process_raw_completion` but adds per-fd ReadLine buffering (buffer-and-split
on `\n`).

This means ReadLine behavior differs between backends:

- **Thread pool:** Raw `libc::read` returns N bytes. `process_with_buffer`
  appends to per-fd buffer, scans for `\n`, returns one line, stores remainder.
  Next ReadLine serves from buffer. Correct.

- **io_uring:** `opcode::Read` returns N bytes. `drain_cqes` reads
  `buf[..result_code]`. `process_raw_completion` trims trailing `\n`/`\r` and
  returns the whole thing as one string. If io_uring returned two lines in one
  read (common with TCP), the second line is lost. If it returned a partial
  line (no newline), the caller gets a truncated result with no way to know.

The io_uring ReadLine path is subtly broken for any scenario where a single
`read()` doesn't return exactly one complete line — which is most of TCP.

**Fix:** Unify into one completion processor that always does per-fd buffering
for ReadLine, regardless of backend. The buffer-and-split logic belongs in the
shared path, not in a thread-pool-specific wrapper.

### 7. Thread pool completion is `(u64, i32, Vec<u8>)` — untyped

**File:** `threadpool.rs` (the channel type), consumed in `aio.rs`

`result_code` is semantically overloaded:
- Positive for Read: byte count
- Positive for Accept: new fd
- Positive for Connect TCP: fd from `into_raw_fd()`
- Zero for Flush/Shutdown/Sleep: success
- Negative: errno

The consumer in `aio.rs` must re-derive semantics from the `PendingOp`'s `op`
field. `drain_network_completions` special-cases Connect to stash `result_code`
as `connect_fd`. This is fragile — adding a new op that returns a meaningful
positive value requires updating every consumer.

This is exactly what issue #548 identifies for the submission side
(`op_kind: u8` + loose parameters). The completion side has the same problem.

**Fix (subsumes #548):** Replace both:
- Submission: `(id, fd, op_kind: u8, data: Vec<u8>, size: usize)` →
  `SubmitOp` enum per #548
- Completion: `(id, result_code: i32, data: Vec<u8>)` →
  `ThreadPoolResult` enum with variants that carry typed payloads

```rust
enum ThreadPoolResult {
    Read { id: u64, data: Result<Vec<u8>, i32> },
    Write { id: u64, bytes_written: Result<usize, i32> },
    Accept { id: u64, fd: Result<RawFd, i32> },
    Connect { id: u64, fd: Result<RawFd, i32> },
    Flush { id: u64, result: Result<(), i32> },
    Shutdown { id: u64, result: Result<(), i32> },
    Sleep { id: u64 },
    SendTo { id: u64, bytes_sent: Result<usize, i32> },
    RecvFrom { id: u64, data: Result<(Vec<u8>, libc::sockaddr_storage, libc::socklen_t), i32> },
}
```

## Execution Plan

### Phase 1: Correctness (must-fix before commit)

1. **Fix sendto sockaddr dangling pointer** — stash in buffer pool
2. **Fix recvfrom address capture** — use `RecvMsg` or equivalent
3. **Fix connect buffer handle leak** — single allocation path
4. **Unify ReadLine completion processing** — per-fd buffering for all backends

### Phase 2: Structure (should-do before merging to main)

5. **Extract sockaddr module** — deduplicate format/parse functions
6. **Make `PendingOp` an enum** — compiler-enforced field presence
7. **Type thread pool submission** — `SubmitOp` enum (closes #548)
8. **Type thread pool completion** — `ThreadPoolResult` enum

### Phase 3: Polish

9. **Clean up `submit_uring` signature** — follows naturally from #7
10. **Review timeout handling consistency** across backends
11. **Update `io/AGENTS.md`** to reflect new structure
