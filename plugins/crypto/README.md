# elle-crypto

A crypto plugin for Elle, providing SHA-256 and HMAC-SHA256 via the Rust `sha2` and `hmac` crates.

## Building

Built as part of the workspace:

```sh
cargo build --workspace
```

Produces `target/debug/libelle_crypto.so` (or `target/release/libelle_crypto.so`).

## Usage

```lisp
(import-file "path/to/libelle_crypto.so")

(bytes->hex (crypto/sha256 "hello"))
;; => "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"

(bytes->hex (crypto/hmac-sha256 "key" "message"))
;; => "6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a"
```

## Primitives

| Name | Args | Returns |
|------|------|---------|
| `crypto/sha256` | data | 32 bytes (SHA-256 hash) |
| `crypto/hmac-sha256` | key, message | 32 bytes (HMAC-SHA256) |

Both accept string, bytes, or blob inputs.
