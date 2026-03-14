//! I/O subsystem: request types and backends.

pub mod aio;
pub mod backend;
pub(crate) mod completion;
pub(crate) mod pool;
pub mod request;
pub(crate) mod sockaddr;
pub(crate) mod threadpool;
pub(crate) mod types;
#[cfg(target_os = "linux")]
pub(crate) mod uring;
