//! Mock I/O backend for testing and benchmarking.
//!
//! Fulfills `IoRequest`s from in-memory state. No OS resources needed.
//! Completions are produced instantly on submit (zero-latency by default).

use crate::io::request::{IoOp, IoRequest};
use crate::io::Completion;
use crate::value::{error_val, Value};

use std::cell::RefCell;
use std::collections::VecDeque;

/// In-memory I/O backend.
///
/// Configurable via:
/// - `seed_read(data)` — pre-seed data returned by ReadLine/Read/ReadAll
/// - `inject_error(errno)` — make the next N operations fail
/// - Call log available via `take_log()`
pub(crate) struct MockBackend {
    inner: RefCell<MockInner>,
}

struct MockInner {
    next_id: u64,
    completions: VecDeque<Completion>,
    read_data: VecDeque<Vec<u8>>,
    error_queue: VecDeque<i32>,
    log: Vec<String>,
}

impl MockBackend {
    pub(crate) fn new() -> Self {
        MockBackend {
            inner: RefCell::new(MockInner {
                next_id: 1,
                completions: VecDeque::new(),
                read_data: VecDeque::new(),
                error_queue: VecDeque::new(),
                log: Vec::new(),
            }),
        }
    }

    /// Pre-seed read data. Each call adds one chunk that will be returned
    /// by the next ReadLine/Read/ReadAll operation.
    #[cfg(test)]
    pub(crate) fn seed_read(&self, data: Vec<u8>) {
        self.inner.borrow_mut().read_data.push_back(data);
    }

    /// Queue an error. The next `n` operations will fail with errno.
    #[cfg(test)]
    pub(crate) fn inject_error(&self, errno: i32) {
        self.inner.borrow_mut().error_queue.push_back(errno);
    }

    /// Take the call log (clears it).
    #[cfg(test)]
    pub(crate) fn take_log(&self) -> Vec<String> {
        std::mem::take(&mut self.inner.borrow_mut().log)
    }
}

impl crate::io::IoBackend for MockBackend {
    fn submit(&self, request: &IoRequest) -> Result<u64, String> {
        let mut inner = self.inner.borrow_mut();
        let id = inner.next_id;
        inner.next_id += 1;

        let op_name = match &request.op {
            IoOp::ReadLine => "read-line",
            IoOp::Read { .. } => "read",
            IoOp::ReadAll => "read-all",
            IoOp::Write { .. } => "write",
            IoOp::Flush => "flush",
            IoOp::Accept => "accept",
            IoOp::Connect { .. } => "connect",
            IoOp::SendTo { .. } => "send-to",
            IoOp::RecvFrom { .. } => "recv-from",
            IoOp::Shutdown { .. } => "shutdown",
            IoOp::Sleep { .. } => "sleep",
        };
        inner.log.push(op_name.to_string());

        // Check for injected error
        if let Some(errno) = inner.error_queue.pop_front() {
            inner.completions.push_back(Completion {
                id,
                result: Err(error_val(
                    "io-error",
                    format!("mock error: errno {}", errno),
                )),
            });
            return Ok(id);
        }

        let result = match &request.op {
            IoOp::ReadLine | IoOp::Read { .. } | IoOp::ReadAll => {
                if let Some(data) = inner.read_data.pop_front() {
                    if data.is_empty() {
                        Ok(Value::NIL) // EOF
                    } else {
                        Ok(Value::string(String::from_utf8_lossy(&data).as_ref()))
                    }
                } else {
                    Ok(Value::NIL) // EOF — no data seeded
                }
            }
            IoOp::Write { data } => {
                let len = data
                    .with_string(|s| s.len())
                    .or_else(|| data.as_bytes().map(|b| b.len()))
                    .unwrap_or(0);
                Ok(Value::int(len as i64))
            }
            IoOp::Flush | IoOp::Shutdown { .. } | IoOp::Sleep { .. } => Ok(Value::NIL),
            IoOp::Accept => {
                // Mock accept: no real fd to return.
                Err(error_val(
                    "io-error",
                    "mock: accept not supported (seed connections via mock/configure)",
                ))
            }
            IoOp::Connect { .. } => {
                // Mock connect: no real socket.
                Err(error_val(
                    "io-error",
                    "mock: connect not supported (seed connections via mock/configure)",
                ))
            }
            IoOp::SendTo { data, .. } => {
                let len = data
                    .with_string(|s| s.len())
                    .or_else(|| data.as_bytes().map(|b| b.len()))
                    .unwrap_or(0);
                Ok(Value::int(len as i64))
            }
            IoOp::RecvFrom { .. } => {
                if let Some(data) = inner.read_data.pop_front() {
                    use crate::value::heap::TableKey;
                    let mut fields = std::collections::BTreeMap::new();
                    fields.insert(TableKey::Keyword("data".into()), Value::bytes(data));
                    fields.insert(TableKey::Keyword("addr".into()), Value::string("127.0.0.1"));
                    fields.insert(TableKey::Keyword("port".into()), Value::int(0));
                    Ok(Value::struct_from(fields))
                } else {
                    Ok(Value::NIL)
                }
            }
        };

        inner.completions.push_back(Completion { id, result });
        Ok(id)
    }

    fn poll(&self) -> Vec<Completion> {
        let mut inner = self.inner.borrow_mut();
        inner.completions.drain(..).collect()
    }

    fn wait(&self, _timeout_ms: i64) -> Result<Vec<Completion>, String> {
        Ok(self.poll())
    }

    fn cancel(&self, _id: u64) -> Result<(), String> {
        // Mock: cancel is a no-op (completions are instant)
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::IoBackend;

    #[test]
    fn test_mock_read() {
        let mock = MockBackend::new();
        mock.seed_read(b"hello world".to_vec());

        let req = IoRequest {
            op: IoOp::ReadAll,
            port: Value::NIL,
            timeout: None,
        };
        let id = mock.submit(&req).unwrap();
        assert_eq!(id, 1);

        let completions = mock.poll();
        assert_eq!(completions.len(), 1);
        assert_eq!(completions[0].id, 1);
        assert!(completions[0].result.is_ok());
    }

    #[test]
    fn test_mock_write() {
        let mock = MockBackend::new();
        let req = IoRequest {
            op: IoOp::Write {
                data: Value::string("test data"),
            },
            port: Value::NIL,
            timeout: None,
        };
        let id = mock.submit(&req).unwrap();
        let completions = mock.poll();
        assert_eq!(completions.len(), 1);
        assert_eq!(completions[0].id, id);
        // Write returns byte count
        let val = completions[0].result.as_ref().unwrap();
        assert_eq!(val.as_int(), Some(9));
    }

    #[test]
    fn test_mock_error_injection() {
        let mock = MockBackend::new();
        mock.inject_error(5); // EIO

        let req = IoRequest {
            op: IoOp::ReadAll,
            port: Value::NIL,
            timeout: None,
        };
        mock.submit(&req).unwrap();

        let completions = mock.poll();
        assert_eq!(completions.len(), 1);
        assert!(completions[0].result.is_err());
    }

    #[test]
    fn test_mock_call_log() {
        let mock = MockBackend::new();
        mock.seed_read(b"data".to_vec());

        let _ = mock.submit(&IoRequest {
            op: IoOp::ReadAll,
            port: Value::NIL,
            timeout: None,
        });
        let _ = mock.submit(&IoRequest {
            op: IoOp::Flush,
            port: Value::NIL,
            timeout: None,
        });

        let log = mock.take_log();
        assert_eq!(log, vec!["read-all", "flush"]);
    }

    #[test]
    fn test_mock_eof_no_data() {
        let mock = MockBackend::new();
        let req = IoRequest {
            op: IoOp::ReadLine,
            port: Value::NIL,
            timeout: None,
        };
        mock.submit(&req).unwrap();
        let completions = mock.poll();
        assert_eq!(completions.len(), 1);
        // No seeded data → EOF (NIL)
        assert_eq!(*completions[0].result.as_ref().unwrap(), Value::NIL);
    }

    #[test]
    fn test_mock_monotonic_ids() {
        let mock = MockBackend::new();
        let id1 = mock
            .submit(&IoRequest {
                op: IoOp::Flush,
                port: Value::NIL,
                timeout: None,
            })
            .unwrap();
        let id2 = mock
            .submit(&IoRequest {
                op: IoOp::Flush,
                port: Value::NIL,
                timeout: None,
            })
            .unwrap();
        assert!(id2 > id1);
    }
}
