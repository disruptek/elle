# HTTP module tests

(def {:assert-eq assert-eq :assert-true assert-true :assert-false assert-false
      :assert-err assert-err :assert-err-kind assert-err-kind
      :assert-not-nil assert-not-nil :assert-string-eq assert-string-eq}
  ((import-file "tests/elle/assert.lisp")))

(def http ((import-file "lib/http.lisp")))
(def parse-url (get http :parse-url))

# ============================================================================
# Chunk 1: URL parsing
# ============================================================================

# Full URL with all components
(let ((u (parse-url "http://example.com:8080/api/users?page=2")))
  (assert-eq (get u :scheme) "http"         "full url: scheme")
  (assert-eq (get u :host)   "example.com"  "full url: host")
  (assert-eq (get u :port)   8080           "full url: port")
  (assert-eq (get u :path)   "/api/users"   "full url: path")
  (assert-eq (get u :query)  "page=2"        "full url: query"))

# Default port (80)
(let ((u (parse-url "http://example.com/index.html")))
  (assert-eq (get u :port) 80 "default port is 80")
  (assert-eq (get u :path) "/index.html" "path with default port"))

# No path defaults to "/"
(let ((u (parse-url "http://example.com")))
  (assert-eq (get u :path) "/" "no path defaults to /")
  (assert-true (nil? (get u :query)) "no query is nil"))

# Query string present, no path
(let ((u (parse-url "http://example.com/?q=hello")))
  (assert-eq (get u :path)  "/"       "path is / with query")
  (assert-eq (get u :query) "q=hello" "query parsed"))

# Trailing slash, no query
(let ((u (parse-url "http://localhost:3000/")))
  (assert-eq (get u :host) "localhost" "localhost host")
  (assert-eq (get u :port) 3000        "port 3000")
  (assert-eq (get u :path) "/"         "path /")
  (assert-true (nil? (get u :query))   "no query"))

# Error: non-http scheme
(assert-err-kind
  (fn () (parse-url "ftp://example.com/"))
  :http-error
  "ftp scheme signals :http-error")

# Error: malformed (no scheme)
(assert-err-kind
  (fn () (parse-url "example.com/foo"))
  :http-error
  "bare hostname signals :http-error")

# Error: https not supported
(assert-err-kind
  (fn () (parse-url "https://example.com/"))
  :http-error
  "https signals :http-error")

# ============================================================================
# Chunk 2: Header parsing and serialization
# ============================================================================

(def header-name->keyword (get http :header-name->keyword))
(def keyword->header-name (get http :keyword->header-name))
(def read-headers  (get http :read-headers))
(def write-headers (get http :write-headers))

# header-name->keyword
(assert-eq (header-name->keyword "Content-Type") :content-type
  "Content-Type -> :content-type")
(assert-eq (header-name->keyword "Host") :host
  "Host -> :host")
(assert-eq (header-name->keyword "X-Custom-Header") :x-custom-header
  "X-Custom-Header -> :x-custom-header")
(assert-eq (header-name->keyword "content-type") :content-type
  "already lowercase: content-type -> :content-type")

# keyword->header-name
(assert-eq (keyword->header-name :content-type) "Content-Type"
  ":content-type -> Content-Type")
(assert-eq (keyword->header-name :host) "Host"
  ":host -> Host")
(assert-eq (keyword->header-name :x-custom-header) "X-Custom-Header"
  ":x-custom-header -> X-Custom-Header")
(assert-eq (keyword->header-name :content-length) "Content-Length"
  ":content-length -> Content-Length")

# Round-trip: header-name->keyword -> keyword->header-name
(assert-eq (keyword->header-name (header-name->keyword "Content-Type")) "Content-Type"
  "round-trip Content-Type")
(assert-eq (keyword->header-name (header-name->keyword "Host")) "Host"
  "round-trip Host")

# read-headers: parse from a file used as port
(spit "/tmp/elle-http-test-headers"
  "Content-Type: text/plain\r\nHost: example.com\r\nContent-Length: 42\r\n\r\n")
(let ((p (port/open "/tmp/elle-http-test-headers" :read)))
  (let ((h (ev/spawn (fn [] (read-headers p)))))
    (port/close p)
    (assert-eq (get h :content-type)   "text/plain"   "content-type header")
    (assert-eq (get h :host)           "example.com"  "host header")
    (assert-eq (get h :content-length) "42"            "content-length header")))

# read-headers: whitespace trimmed from values
(spit "/tmp/elle-http-test-headers-ws"
  "X-Foo:   bar baz   \r\n\r\n")
(let ((p (port/open "/tmp/elle-http-test-headers-ws" :read)))
  (let ((h (ev/spawn (fn [] (read-headers p)))))
    (port/close p)
    (assert-eq (get h :x-foo) "bar baz" "leading/trailing whitespace trimmed")))

# read-headers: malformed line signals :http-error
(spit "/tmp/elle-http-test-headers-bad" "no-colon-here\r\n\r\n")
(assert-err-kind
  (fn ()
    (let ((p (port/open "/tmp/elle-http-test-headers-bad" :read)))
      (let ((result (ev/spawn (fn [] (read-headers p)))))
        (port/close p)
        result)))
  :http-error
  "malformed header line signals :http-error")

# write-headers: verify format
(let ((p (port/open "/tmp/elle-http-test-write-headers" :write)))
  (ev/spawn (fn []
    (write-headers p {:content-type "text/plain" :content-length "11"})
    (stream/write p "\r\n")
    (stream/flush p)))
  (port/close p))
(let ((content (slurp "/tmp/elle-http-test-write-headers")))
  (assert-true (string-contains? content "Content-Type: text/plain")
    "write-headers: content-type line present")
  (assert-true (string-contains? content "Content-Length: 11")
    "write-headers: content-length line present"))
