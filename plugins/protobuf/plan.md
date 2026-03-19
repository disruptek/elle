# elle-protobuf plugin plan

## Approach: dynamic descriptors

Users load `.proto` files (or binary `FileDescriptorSet`) at runtime.
The plugin parses them into a descriptor pool — an opaque `External`
value. Encode and decode operate against message descriptors from that
pool. No code generation. No compile-time schemas.

This is the right choice for a scripting language. The alternatives:

- **Schema-as-struct** (Elle DSL for field numbers/types) — forces users
  to hand-translate every `.proto` into Elle. Defeats the point of using
  protobuf as an interchange format. No ecosystem compatibility.
- **Raw wire format** (field-number maps) — useful for inspection but
  not for application-level encode/decode. Loses type information.
- **Hybrid** — complexity for questionable gain. Start with descriptors;
  raw wire can be added later if needed.

## Crates

| Crate | Version | Purpose |
|-------|---------|---------|
| `prost-reflect` | latest | `DescriptorPool`, `DynamicMessage`, `Value` enum |
| `prost` | (transitive) | Protobuf encoding/decoding primitives |
| `prost-types` | (transitive) | `FileDescriptorSet`, `FileDescriptorProto` |
| `protobuf-parse` | latest | Parse `.proto` source text → `FileDescriptorSet` (pure Rust, no `protoc`) |
| `protobuf` | 3.x | (transitive via `protobuf-parse`) |

### Why prost-reflect

- Active maintenance, recent releases
- Clean `Value` enum maps directly to Elle types
- `DynamicMessage` supports construction by field name, encode, decode
- Handles all protobuf features: scalars, nested messages, repeated,
  maps, enums, oneofs

### Why protobuf-parse

- `prost-reflect` only accepts binary `FileDescriptorSet`, not `.proto`
  text
- `protobuf-parse` has a pure-Rust parser — no `protoc` binary needed
- Parses `.proto` source at runtime → `FileDescriptorSet` → feed to
  `prost-reflect`
- Without this, users must pre-compile `.proto` files with `protoc
  --descriptor_set_out`. Unacceptable for a scripting language.

### Dual-ecosystem dependency

This plugin depends on two protobuf implementations: the `protobuf`
crate (via `protobuf-parse` for `.proto` text parsing) and the `prost`
ecosystem (via `prost-reflect` for dynamic messages). These are
different Rust types for the same wire format. The bridge is:
serialize `FileDescriptorSet` to bytes with `protobuf`, deserialize
with `prost-reflect`. This is a one-time cost at schema load time.

The alternative — using only `prost-reflect` — would require users to
pre-compile `.proto` files with `protoc`, which is unacceptable for a
scripting language. The binary size increase from carrying both
implementations is the cost of a good UX.

### Conversion path

```
.proto text → protobuf-parse → FileDescriptorSet (protobuf crate)
                                       ↓ serialize to bytes
                                FileDescriptorSet (prost-types)
                                       ↓
                                DescriptorPool (prost-reflect)
                                       ↓
                                 MessageDescriptor
                                  ↓            ↓
                            encode          decode
                         (Elle → bytes)  (bytes → Elle)
```

## Value mapping

### Protobuf → Elle (decode)

| Protobuf type | Elle type | Notes |
|---------------|-----------|-------|
| `int32`, `sint32`, `sfixed32` | `int` | Direct |
| `int64`, `sint64`, `sfixed64` | `int` | Clamped to Elle 48-bit range; error if out of range |
| `uint32`, `fixed32` | `int` | Direct (fits in 48-bit signed) |
| `uint64`, `fixed64` | `int` | Error if > 2^47-1 |
| `float` | `float` | Widened to f64 |
| `double` | `float` | Direct |
| `bool` | `bool` | Direct |
| `string` | `string` | Immutable |
| `bytes` | `bytes` | Immutable |
| `enum` | `keyword` | Enum value name as keyword (e.g., `:FOO_BAR`) |
| message | `struct` | Immutable struct with keyword keys, recursive |
| `repeated T` | `array` | Immutable array of decoded elements |
| `map<K, V>` | `struct` | Immutable struct; keys become keyword keys (string maps) or int keys |
| unset field | absent | Field omitted from struct (not `nil`) |

### Elle → Protobuf (encode)

| Elle type | Protobuf type | Notes |
|-----------|---------------|-------|
| `int` | `int32`/`int64`/`uint32`/`uint64`/etc. | Range-checked against field descriptor |
| `float` | `float`/`double` | Narrowed to f32 if field is `float` |
| `bool` | `bool` | Direct |
| `string` / `@string` | `string` | Borrowed or copied |
| `bytes` / `@bytes` | `bytes` | Borrowed or copied |
| `keyword` | `enum` | Keyword name matched against enum descriptor |
| `struct` / `@struct` | message | Recursive; keyword keys matched to field names |
| `array` / `@array` | `repeated` | Each element encoded per field type |
| `nil` | (field omitted) | Nil value means field not set |

**uint64/fixed64 map keys on encode:** Elle `int` values are accepted
for keys ≤ 2⁶³−1. For keys > 2⁶³−1, Elle `string` values containing
the decimal representation are accepted and parsed. See map key section
above for the TODO.

### Enum representation

Enums decode as keywords: the enum value _name_ (not number) becomes a
keyword. `STATUS_OK` → `:STATUS_OK`. Proto case is preserved — no
lowercasing. This is idiomatic Elle — keywords are the natural
representation for named constants.

On encode, keywords are matched against the enum descriptor by name.
Unknown enum names produce a clear error. Integer values are also
accepted (for forward compatibility with unknown enum values).

On decode, unknown enum values (not in the descriptor) are returned as
integers. This handles forward-compatibility: a newer `.proto` may
define enum values the current descriptor doesn't know about.

### Map field keys

Protobuf map keys can be string, integer, or bool. The full set of
allowed protobuf map key types (from the spec):

```
keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
          "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
```

Mapping to Elle:

| Protobuf map key type | Elle struct key type | Notes |
|-----------------------|----------------------|-------|
| `string` | keyword | String value used as keyword name (tokens are the natural Elle representation) |
| `int32`, `sint32`, `sfixed32` | int | Fits in Elle i64 |
| `int64`, `sint64`, `sfixed64` | int | Fits in Elle i64 |
| `uint32`, `fixed32` | int | Max 4,294,967,295 — fits in Elle i64 |
| **`uint64`, `fixed64`** | **int** (or **string** if > 2⁶³−1) | **See note below** |
| `bool` | bool | Direct |

**uint64/fixed64 map keys — known limitation:**
`uint64` and `fixed64` map keys have range 0 to 2⁶⁴−1
(18,446,744,073,709,551,615). Elle's `int` is a 64-bit *signed* integer
(−2⁶³ to 2⁶³−1). Values above 2⁶³−1 cannot be represented without a
new `u64` Value variant.

For now, `uint64`/`fixed64` map keys are represented as **strings** in
Elle (decimal representation, e.g., `"18446744073709551615"`). This is
lossy for round-trips when keys exceed 2⁶³−1, but avoids adding a new
Value type. All encode/decode code for these key types is marked with
`// TODO(uint64): Elle has no u64 Value type; using string representation
// for uint64/fixed64 map keys. When u64 is added to Value, change this
// to use int keys directly.`

String-keyed maps use keyword keys because that's idiomatic Elle.
The string value is used as-is as the keyword name — no case conversion.
Elle keywords are arbitrary strings internally.

### Absent vs. default

Protobuf 3 does not distinguish "field set to default" from "field
absent" at the wire level (except for `optional` fields). The plugin
delegates to `prost-reflect`'s behavior, which follows the proto3 spec:

- **Decode**: `prost-reflect`'s `DynamicMessage` tracks which fields
  have been explicitly set. The plugin includes a field in the output
  struct if and only if `DynamicMessage::has_field()` returns true.
  For proto3 non-optional fields, this means default values are omitted.
  For `optional` fields, explicitly-set defaults are included.
- **Encode**: `nil` values or missing struct keys mean "don't set field".
  The plugin calls `set_field` only for keys present in the Elle struct
  with non-nil values. `prost-reflect` handles wire encoding per proto3
  rules (default values omitted for non-optional fields).

### Nested messages

Nested messages are represented as nested structs. The nesting
structure is preserved in the struct hierarchy — field names are not
flattened or qualified.

```lisp
# Given: message Outer { Inner inner = 1; }
#        message Inner { string value = 1; }
(protobuf/encode pool "Outer" {:inner {:value "hello"}})
(protobuf/decode pool "Outer" buf)  # => {:inner {:value "hello"}}
```

### Oneof fields

Oneof fields are flattened into the message struct. Only the active
field of a oneof group appears in the decoded struct. On encode, if
multiple fields from the same oneof group are set in the Elle struct,
the last one wins (following `prost-reflect`'s set_field semantics —
each set_field call for a oneof field clears the previous one).

```lisp
# Given: message Event {
#   oneof payload { string text = 1; int32 code = 2; }
# }
(protobuf/decode pool "Event" buf)  # => {:text "hello"}  (only active field)
(protobuf/encode pool "Event" {:text "hello"})  # sets text, code is unset
```

### Struct key handling on encode

On encode, only keyword keys in the Elle struct are processed. This
is the normal case — Elle structs created from protobuf decoding
always have keyword keys. Non-keyword keys (int, string, bool) are
silently ignored. This avoids errors when a struct has extra metadata
keys of non-keyword type.

## Primitives

### protobuf/schema

Parse `.proto` source text into a descriptor pool.

```
(protobuf/schema proto-string)
(protobuf/schema proto-string {:path "foo.proto" :includes ["dir1" "dir2"]})
```

- `proto-string`: string containing `.proto` source
- Optional second arg: struct with:
  - `:path` — virtual filename for the proto (for import resolution)
  - `:includes` — array of include paths for resolving `import` statements

Returns an `External` value of type `"protobuf/pool"` wrapping a
`DescriptorPool`.

| Error | Kind | Message |
|-------|------|---------|
| Not a string | `type-error` | `"protobuf/schema: expected string, got {type}"` |
| Parse failure | `protobuf-error` | `"protobuf/schema: {parse error details}"` |
| Invalid option struct | `type-error` | `"protobuf/schema: expected struct for options, got {type}"` |

```lisp
(def pool (protobuf/schema "
  syntax = \"proto3\";
  message Person {
    string name = 1;
    int32 age = 2;
    repeated string tags = 3;
  }
"))
```

### protobuf/schema-bytes

Load a pre-compiled `FileDescriptorSet` (binary) into a descriptor pool.

```
(protobuf/schema-bytes fds-bytes)
```

- `fds-bytes`: `bytes` or `@bytes` containing a serialized
  `FileDescriptorSet`

Returns an `External` value of type `"protobuf/pool"`.

| Error | Kind | Message |
|-------|------|---------|
| Not bytes | `type-error` | `"protobuf/schema-bytes: expected bytes, got {type}"` |
| Decode failure | `protobuf-error` | `"protobuf/schema-bytes: {decode error}"` |

This exists for users who have pre-compiled descriptors (e.g., from
`protoc --descriptor_set_out`). The common path is `protobuf/schema`.

### protobuf/encode

Encode an Elle struct to protobuf bytes.

```
(protobuf/encode pool "MessageName" value)
```

- `pool`: `External("protobuf/pool")` from `protobuf/schema` or
  `protobuf/schema-bytes`
- `"MessageName"`: string, fully qualified message name (e.g.,
  `"Person"` or `"mypackage.Person"`)
- `value`: Elle struct to encode

Returns `bytes` (immutable).

| Error | Kind | Message |
|-------|------|---------|
| Wrong pool type | `type-error` | `"protobuf/encode: expected protobuf/pool, got {type}"` |
| Message not found | `protobuf-error` | `"protobuf/encode: message 'Foo' not found in pool"` |
| Not a struct | `type-error` | `"protobuf/encode: expected struct, got {type}"` (both struct and @struct accepted) |
| Field type mismatch | `protobuf-error` | `"protobuf/encode: field 'name': expected string, got int"` |
| Unknown field | `protobuf-error` | `"protobuf/encode: unknown field 'xyz' in message 'Person'"` |
| Int out of range | `protobuf-error` | `"protobuf/encode: field 'age': value 999999999999 out of int32 range"` |
| Unknown enum keyword | `protobuf-error` | `"protobuf/encode: field 'status': unknown enum value :INVALID"` |

```lisp
(def buf (protobuf/encode pool "Person"
           {:name "Alice" :age 30 :tags ["dev" "lisp"]}))
# buf is bytes
```

### protobuf/decode

Decode protobuf bytes to an Elle struct.

```
(protobuf/decode pool "MessageName" buf)
```

- `pool`: `External("protobuf/pool")`
- `"MessageName"`: string, fully qualified message name
- `buf`: `bytes` or `@bytes` to decode

Returns an immutable `struct` with keyword keys.

| Error | Kind | Message |
|-------|------|---------|
| Wrong pool type | `type-error` | `"protobuf/decode: expected protobuf/pool, got {type}"` |
| Message not found | `protobuf-error` | `"protobuf/decode: message 'Foo' not found in pool"` |
| Not bytes | `type-error` | `"protobuf/decode: expected bytes, got {type}"` |
| Decode failure | `protobuf-error` | `"protobuf/decode: {wire format error}"` |
| Int overflow | `protobuf-error` | `"protobuf/decode: field 'id': int64 value out of Elle 48-bit range"` |

```lisp
(def person (protobuf/decode pool "Person" buf))
(get person :name)  # => "Alice"
(get person :age)   # => 30
(get person :tags)  # => ["dev" "lisp"]
```

### protobuf/messages

List message names in a descriptor pool.

```
(protobuf/messages pool)
```

Returns an immutable array of strings (fully qualified message names).

| Error | Kind | Message |
|-------|------|---------|
| Wrong pool type | `type-error` | `"protobuf/messages: expected protobuf/pool, got {type}"` |

```lisp
(protobuf/messages pool)  # => ["Person"]
```

### protobuf/fields

List fields of a message in a descriptor pool.

```
(protobuf/fields pool "MessageName")
```

Returns an immutable array of structs, one per field:

```lisp
(protobuf/fields pool "Person")
# => [{:name "name" :number 1 :type :string :label :optional}
#     {:name "age"  :number 2 :type :int32  :label :optional}
#     {:name "tags" :number 3 :type :string :label :repeated}]
```

Each field struct contains:
- `:name` — string, field name
- `:number` — int, field number
- `:type` — keyword, protobuf type (`:int32`, `:string`, `:message`,
  `:enum`, `:map`, etc.)
- `:label` — keyword, `:optional`, `:repeated`, or `:required`
- `:message-type` — string, fully qualified name of the message or
  enum type. Present only for message, enum, and map-value fields.
  For map fields, this is the auto-generated map entry message name.

| Error | Kind | Message |
|-------|------|---------|
| Wrong pool type | `type-error` | `"protobuf/fields: expected protobuf/pool, got {type}"` |
| Message not found | `protobuf-error` | `"protobuf/fields: message 'Foo' not found in pool"` |

### protobuf/enums

List enum types in a descriptor pool.

```
(protobuf/enums pool)
```

Returns an immutable array of structs:

```lisp
(protobuf/enums pool)
# => [{:name "Status" :values [{:name "OK" :number 0}
#                               {:name "ERROR" :number 1}]}]
```

| Error | Kind | Message |
|-------|------|---------|
| Wrong pool type | `type-error` | `"protobuf/enums: expected protobuf/pool, got {type}"` |

All enums in the pool are returned, including nested enums. Enum names
are fully qualified (e.g., `"mypackage.Outer.Status"`).

## Primitive summary

| Primitive | Arity | Signal | Purpose |
|-----------|-------|--------|---------|
| `protobuf/schema` | 1–2 | errors | Parse `.proto` text → pool |
| `protobuf/schema-bytes` | 1 | errors | Load binary `FileDescriptorSet` → pool |
| `protobuf/encode` | 3 | errors | Elle struct → protobuf bytes |
| `protobuf/decode` | 3 | errors | Protobuf bytes → Elle struct |
| `protobuf/messages` | 1 | errors | List message names in pool |
| `protobuf/fields` | 2 | errors | List fields of a message |
| `protobuf/enums` | 1 | errors | List enum types in pool |

## What is deliberately excluded

- **`protobuf/valid?`** — not useful. Protobuf bytes without a schema
  are just varints and length-delimited blobs. Validation requires a
  schema, and `protobuf/decode` already validates.
- **Raw wire format access** — no need yet. Can be added later.
- **gRPC** — out of scope. gRPC is a transport protocol, not a
  serialization format.
- **Proto2 `required` fields** — `prost-reflect` handles these. No
  special treatment needed.
- **Extensions** — rare in proto3. Can be added later if needed.
- **`protobuf/merge-pools`** — combining pools. Start without it; add
  if import-heavy workflows demand it.

## Implementation

### External value

The descriptor pool is stored as:

```rust
Value::external("protobuf/pool", pool)
```

where `pool` is a `prost_reflect::DescriptorPool`. Extracted with:

```rust
args[0].as_external::<prost_reflect::DescriptorPool>()
```

### protobuf/schema internals

1. Extract proto string from args
2. Optionally extract `:path` and `:includes` from options struct
3. Write proto string to a temp file. If `:path` is provided, use it
   as the filename (for import resolution context). Default to
   `"input.proto"`.
4. Configure `protobuf_parse::Parser::new().pure()`:
   - `.input(&temp_path)` — the temp file
   - `.includes(&include_dirs)` — from `:includes` (defaults to
     the temp file's parent directory)
5. Call `.file_descriptor_set()` to get `FileDescriptorSet`
6. Serialize to bytes with `protobuf::Message::write_to_bytes()`
7. Call `DescriptorPool::decode(fds_bytes)` (prost-reflect)
8. Clean up temp file
9. Return `Value::external("protobuf/pool", pool)`

**Serialization bridge**: See "Dual-ecosystem dependency" above.
Verified in Chunk 0 spike.

**Temp file note**: `protobuf-parse` may require actual files on disk
for its include resolution mechanism. If the Chunk 0 spike reveals an
in-memory API, prefer that. Otherwise, use `tempfile` crate for safe
temp file management.

### protobuf/encode internals

1. Extract pool, message name, Elle struct from args
2. Look up `MessageDescriptor` by name in pool
3. Create `DynamicMessage::new(desc)`
4. For each key-value in the Elle struct:
   a. Strip `:` prefix from keyword key to get field name
   b. Look up `FieldDescriptor` by name
   c. Convert Elle value to `prost_reflect::Value` per the type mapping
   d. Set field on the dynamic message
5. Encode with `msg.encode_to_vec()`
6. Return `Value::bytes(encoded)`

### protobuf/decode internals

1. Extract pool, message name, bytes from args
2. Look up `MessageDescriptor` by name in pool
3. Call `DynamicMessage::decode(desc, bytes)`
4. For each field in the decoded message:
   a. Convert `prost_reflect::Value` to Elle value
   b. Use field name as keyword key
5. Build immutable struct from fields
6. Return the struct

### Elle ↔ prost_reflect::Value conversion

Two functions at the core:

```rust
fn elle_to_pb(val: Value, field: &FieldDescriptor) -> Result<prost_reflect::Value, String>
fn pb_to_elle(val: &prost_reflect::Value, field: &FieldDescriptor) -> Result<Value, String>
```

The `FieldDescriptor` parameter is required because protobuf values
need type context (e.g., to know whether an int is int32 or int64, or
whether to decode an enum by name).

## File structure

```
plugins/protobuf/
  Cargo.toml
  src/
    lib.rs        # Plugin entry point, primitive registration
    convert.rs    # Elle ↔ prost_reflect::Value conversion
    schema.rs     # Schema loading (proto text and binary FDS)
    inspect.rs    # messages, fields, enums introspection
```

Split by responsibility. ~4 files, each under 300 lines.

## Cargo.toml

```toml
[package]
name = "elle-protobuf"
version = "1.0.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
elle = { path = "../.." }
prost = "0.13"
prost-reflect = "0.14"
prost-types = "0.13"
protobuf = "3"
protobuf-parse = "3"
```

(Version numbers are approximate — use latest compatible versions.)

## Implementation chunks

### Chunk 0: Pre-implementation spike

Before any plugin code, verify the two critical assumptions in a
standalone Rust binary (not a cdylib):

1. **protobuf-parse in-memory input**: Does `protobuf_parse::Parser`
   support parsing a `.proto` from a string in memory? Try
   `Parser::new().pure()` with whatever API is available. If it
   requires files on disk, test the temp-file approach.

2. **Serialization bridge**: Parse a `.proto` with `protobuf-parse`,
   serialize the resulting `FileDescriptorSet` to bytes using the
   `protobuf` crate's `Message::write_to_bytes()`, then decode those
   bytes with `prost_reflect::DescriptorPool::decode()`. Verify it
   produces a valid pool with the expected message descriptors.

3. **Dependency compatibility**: Verify that `protobuf-parse` 3.x and
   `prost-reflect` latest can coexist in the same `Cargo.toml` without
   version conflicts.

4. **Import resolution**: Test a `.proto` file that `import`s another
   `.proto` file. Verify that `protobuf-parse`'s include path
   mechanism works and the resulting `DescriptorPool` contains both
   message types. This validates the `:includes` option design.

If any of (1)–(3) fail, the architecture must be revised before
proceeding. Possible fallback: drop `protobuf/schema` (text parsing)
and only support `protobuf/schema-bytes` (pre-compiled descriptors).
If (4) fails, drop the `:includes` option and document the limitation.

### Chunk 1: Scaffold + schema loading

- Create `Cargo.toml`, `src/lib.rs`
- Implement `protobuf/schema` and `protobuf/schema-bytes`
- Plugin init, registration table
- Verify: load a `.proto` string, get a pool back

### Chunk 2: Encode

- Implement `elle_to_pb` conversion
- Implement `protobuf/encode`
- Test: encode a simple message, verify bytes match expected

### Chunk 3: Decode

- Implement `pb_to_elle` conversion
- Implement `protobuf/decode`
- Test: roundtrip encode→decode, verify struct equality

### Chunk 4: Introspection

- Implement `protobuf/messages`, `protobuf/fields`, `protobuf/enums`
- Test: verify introspection output against known schemas

### Chunk 5: Edge cases and complex types

- Nested messages
- Map fields
- Enum fields (keyword mapping)
- Oneof fields
- Repeated fields with nested messages
- Integer overflow detection (int64/uint64 vs Elle 48-bit)
- Test all of the above

### Chunk 6: Integration test

- Write an Elle script that:
  1. Loads a `.proto` schema
  2. Encodes a struct
  3. Decodes it back
  4. Verifies round-trip fidelity
- Add to `tests/` or `examples/`

## Open questions

All three questions below are resolved by the Chunk 0 spike. Do not
proceed past Chunk 0 until they have empirical answers.

1. **protobuf-parse input API**: Does `protobuf-parse` support parsing
   from a string in memory, or does it require files on disk? If
   files-only, the temp file strategy must handle cleanup and
   concurrency. Fallback: drop `protobuf/schema`, keep only
   `protobuf/schema-bytes`.

2. **Version compatibility**: `protobuf-parse` 3.x uses `protobuf` 3.x
   types. `prost-reflect` uses `prost-types`. The serialization bridge
   (serialize with `protobuf` → deserialize with `prost`) should work
   since they implement the same proto format, but must be verified.

3. **Enum case convention**: Resolved. Preserve proto case. `:STATUS_OK`
   not `:status-ok`. No information loss, no ambiguity.
