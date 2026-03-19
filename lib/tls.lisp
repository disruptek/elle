## lib/tls.lisp — TLS client and server for Elle
##
## TLS client and server using the elle-tls plugin for state machine management.
## All socket I/O is async via native TCP ports and the fiber scheduler.
##
## Dependencies:
##   - elle-tls plugin loaded via (import-file "path/to/libelle_tls.so")
##   - tcp/connect, tcp/accept from net primitives
##   - stream/read, stream/write from stream primitives  (stream/read returns
##     bytes for TCP ports — TCP streams use binary encoding in this runtime)
##   - ev/run, ev/spawn from scheduler
##   - port/close for TCP port lifecycle
##   - subprocess/system for hostname resolution (getent fallback)
##
## Usage:
##   (def tls-plugin (import-file "target/release/libelle_tls.so"))
##   (def tls ((import-file "lib/tls.lisp") tls-plugin))
##   (ev/run (fn [] (let [[conn (tls:connect "example.com" 443)]]
##                    (defer (tls:close conn) ...))))
##
## The file exports a function that accepts the plugin struct and returns the
## public API struct. The plugin struct is closed over so all public
## functions can call plugin primitives without naming them globally.
##
## API change from spec: the entry point takes the plugin struct as argument.
## Load with: (def tls ((import-file "lib/tls.lisp") tls-plugin))
##
## tls-conn shape:
##   {:tcp port :tls tls-state}
##   :tcp — the underlying TcpStream port
##   :tls — the TlsState ExternalObject from the elle-tls plugin

## ── Hostname resolution ─────────────────────────────────────────────────────
##
## The io_uring TCP backend requires IP addresses for tcp/connect. Hostnames
## must be resolved before connecting. We use getent(1) via subprocess to
## delegate to the system resolver (glibc getaddrinfo), which handles /etc/hosts,
## systemd-resolved, and any configured DNS forwarder.

(defn first-word [s]
  "Extract first whitespace-delimited token from string s."
  (let [[sp (string/find s " ")]]
    (if (nil? sp) s (slice s 0 sp))))

(defn first-line [s]
  "Extract first line from string s."
  (let [[nl (string/find s "\n")]]
    (if (nil? nl) s (slice s 0 nl))))

(defn resolve-host [host]
  "Resolve hostname to an IP address string for use with tcp/connect.
   Delegates to getent(1) via 'getent ahosts' to get IPv4 first. Returns
   the IP string, or host unchanged if resolution fails (allows pre-resolved IPs).
   IPv4 addresses are preferred over IPv6 because the io_uring TCP connect
   path formats addresses as 'ip:port', which only works for IPv4. IPv6 would
   need bracket notation '[ip]:port' which is not yet implemented."
  (let [[result (subprocess/system "getent" ["ahosts" host])]]
    (if (not (= result:exit 0))
      host  # Resolution failed — pass host through (may be an IP already)
      (let [[out (string/trim result:stdout)]]
        (if (empty? out)
          host
          # getent ahosts output: "<ip>   STREAM <canonical>" (one per line)
          # Extract the IP from the first line (first whitespace-delimited field).
          (first-word (first-line out)))))))

## ── The entry-point thunk ───────────────────────────────────────────────────
##
## The file's last expression is a function that accepts the plugin struct
## and returns the public API. Call it like:
##   (def tls ((import-file "lib/tls.lisp") tls-plugin))

(fn [plugin]
  ## Extract plugin primitives from the struct so they can be called
  ## as local bindings. Plugin primitives are not resolvable by name
  ## at compile time — they must be accessed through the struct.
  (def process-fn             (get plugin :process))
  (def get-outgoing-fn        (get plugin :get-outgoing))
  (def handshake-complete?-fn (get plugin :handshake-complete?))
  (def client-state-fn        (get plugin :client-state))
  (def server-state-fn        (get plugin :server-state))

  ## ── Private: handshake driver ───────────────────────────────────────────

  (defn tls-handshake [port tls]
    "Drive TLS handshake to completion over a TCP port.
     Mutates tls in place. Returns nil on success.
     Must be called inside a scheduler context (ev/run or ev/spawn).

     Loop invariant:
       - After every tls/process call, drain and send outgoing bytes.
         TLS 1.3 may produce post-handshake messages at any time.
       - Check handshake-complete? AFTER sending outgoing — the server
         needs to receive our Finished before it considers us ready."
    # Pump the state machine with empty bytes to generate the initial
    # ClientHello (client side) or enter the wait state (server side).
    (process-fn tls (bytes))
    (forever
      # INVARIANT: Send any queued ciphertext before doing anything else.
      # This must happen on the first iteration for ClientHello (client side)
      # and after every subsequent process call.
      (let [[out (get-outgoing-fn tls)]]
        (when (> (length out) 0)
          (stream/write port out)))          # async — yields SIG_IO

      # If handshake is complete, we're done.
      (when (handshake-complete?-fn tls)
        (break nil))

      # Read more ciphertext from the network.
      # Note: TCP ports use binary encoding; stream/read returns bytes.
      (let [[data (stream/read port 16384)]]  # async — yields SIG_IO
        (when (nil? data)
          (error {:error :tls-error
                  :message "tls: connection closed during handshake"}))

        # Feed into state machine. Outgoing data from this call
        # will be sent at the top of the next loop iteration.
        (process-fn tls data))))

  ## ── Public: connection functions ────────────────────────────────────────

  (defn tls/connect [hostname port-num & args]
    "Connect to a TLS server. Returns a tls-conn struct {:tcp port :tls tls-state}.
     Must be called inside a scheduler context (ev/run or ev/spawn).

     hostname is used for TLS SNI and certificate verification.
     The hostname is DNS-resolved to an IP before connecting (required by
     the io_uring backend; the sync backend supports hostname connects directly).

     Optional third argument opts struct:
       :no-verify  bool   — skip certificate verification (dev/test only)
       :ca-file    string — path to PEM CA bundle
       :client-cert string — path to PEM client certificate chain
       :client-key  string — path to PEM client private key"
    (let* [[opts (or (first args) {})]
           # Resolve hostname to IP. The io_uring TCP backend requires an IP
           # address; hostnames must be resolved before calling tcp/connect.
           # SNI and cert verification still use the original hostname.
           [ip (resolve-host hostname)]
           [tcp-port (tcp/connect ip port-num)]       # async
           [tls (client-state-fn hostname opts)]]     # sync — state machine only
      (let [[[ok? result] (protect (tls-handshake tcp-port tls))]]
        (unless ok?
          # Handshake failed. Close TCP port before re-raising.
          # Do not attempt to send close_notify — the connection is broken.
          (port/close tcp-port)
          (error result))
        {:tcp tcp-port :tls tls})))

  (defn tls/accept [listener config]
    "Accept a TLS connection on a TCP listener. Returns a tls-conn struct.
     listener: a TcpListener port from (tcp/listen host port).
     config:   a tls-server-config from (tls/server-config cert key).
     Must be called inside a scheduler context."
    (let* [[tcp-port (tcp/accept listener)]       # async
           [tls (server-state-fn config)]]        # sync
      (let [[[ok? result] (protect (tls-handshake tcp-port tls))]]
        (unless ok?
          (port/close tcp-port)
          (error result))
        {:tcp tcp-port :tls tls})))

  ## ── Export struct ──────────────────────────────────────────────────────
  {:connect tls/connect
   :accept  tls/accept})
