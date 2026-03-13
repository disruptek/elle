#!/usr/bin/env elle

## HTTP module example
##
## Demonstrates:
##   parse-url    — URL parsing
##   http:respond — build response structs
##   http:get     — make a GET request
##   http:post    — make a POST request with a body
##   Integration  — live server+client round-trip using ev/run

(def http ((import "./lib/http.lisp")))

# ── URL parsing ──────────────────────────────────────────────────────────────

(let* [[u (http:parse-url "http://example.com:8080/api?q=test")]]
  (assert (= u:host  "example.com") "host")
  (assert (= u:port  8080)          "port")
  (assert (= u:path  "/api")        "path")
  (assert (= u:query "q=test")      "query"))

# ── http:respond ─────────────────────────────────────────────────────────────

(let* [[r (http:respond 200 "Hello, World!")]]
  (assert (= r:status 200)             "status 200")
  (assert (= r:body   "Hello, World!") "body"))

# ── http:serve demonstration ────────────────────────────────────────────────
#
# http:serve(port handler) starts an HTTP server that runs forever.
# The handler receives a request struct and returns a response struct.
# Request: {:method :path :version :headers :body}
# Response: {:status :headers :body}
#
# Example handler (not executed here, as http:serve blocks):
#
#   (defn my-handler [request]
#     (match request:method
#       ("GET" (http:respond 200 (string/format "GET {}" request:path)))
#       ("POST" (http:respond 201 (string/format "Posted: {}" request:body)))
#       (_ (http:respond 405 "Method Not Allowed"))))
#
#   (http:serve 8080 my-handler)  ; runs forever
#
# To stop the server, use (cancel server-fiber) from outside the handler.

(print "all http examples passed.")
