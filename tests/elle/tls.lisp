## TLS library integration tests — Chunk 4: handshake only
##
## Requires network access (connects to example.com:443).
## Requires the elle-tls plugin to be built:
##   cargo build -p elle-tls          (debug)
##   cargo build --release -p elle-tls (release)
##
## Run with:
##   ./target/release/elle tests/elle/tls.lisp
##
## Note: Plugin primitives are not resolvable by name at compile time.
## They are accessed via the struct returned by import-file and closed
## over in lib/tls.lisp.

## Try release build first, fall back to debug.
(def [ok? tls-plugin]
  (let [[[ok? r] (protect (import-file "target/release/libelle_tls.so"))]]
    (if ok?
      [ok? r]
      (protect (import-file "target/debug/libelle_tls.so")))))

(when (not ok?)
  (display "SKIP: elle-tls plugin not built (run: cargo build -p elle-tls)\n")
  (exit 0))

## Extract the handshake-complete? primitive for use in assertions.
## (Must be accessed via plugin struct — not resolvable as a global name.)
(def handshake-complete? (get tls-plugin :handshake-complete?))

## Load the TLS stdlib, passing the plugin struct so it can close over
## the plugin primitives.
(def tls ((import-file "lib/tls.lisp") tls-plugin))

## ── Chunk 4: handshake test ─────────────────────────────────────────────────

(ev/run (fn []
  (let [[[ok? result] (protect
                        (tls:connect "example.com" 443))]]
    (assert ok? (concat "tls: connect to example.com:443 failed: " (string result)))
    (let [[conn result]]
      # Verify the tls-conn shape.
      (assert (not (nil? conn:tcp)) "tls: conn:tcp must be a port")
      (assert (not (nil? conn:tls)) "tls: conn:tls must be a tls-state")
      (assert (handshake-complete? conn:tls) "tls: handshake must be complete")
      # Clean up without trying to send data.
      (port/close conn:tcp)))))

(print "tls chunk 4: handshake test PASSED\n")
