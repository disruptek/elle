#!/usr/bin/env elle

# Collections in Elle — a contact book
#
# Demonstrates:
#   Literal syntax    — [tuple]  @[array]  {struct}  @{table}  "string"  @"buffer"
#   Mutability        — @ prefix means mutable; bare means immutable
#   Polymorphic ops   — get, length, empty?, append, concat work across types
#   put semantics     — always returns a collection (mutated original or new copy)
#   Iteration         — each ... in ... (prelude macro)
#   Destructuring     — {:key var} and [a b] patterns in let/def/fn
#   Threading macros  — ->  ->>
#   Splice            — ;expr for spreading arrays/tuples into calls
#   Key-value ops     — put, del, keys, values, has-key?
#   List ops          — cons, first, rest, reverse, take, drop, last, butlast
#   Array mutation    — push, pop, insert, remove
#   String ops        — string/find, string/split, string/join, string/slice,
#                       string/replace, string/contains?, string/trim,
#                       string/upcase, string/downcase
#   Grapheme clusters — string indexing operates on what humans see as characters
#
# The running example is a contact book: contacts are structs (immutable
# records), the book is a table (mutable), and we query, format, and
# export it using every collection facility the language offers.

(import-file "./examples/assertions.lisp")


# ========================================
# 1. The mutable/immutable split
# ========================================
#
# Every collection pair shares structure but differs in mutability.
# Bare delimiters are immutable; @-prefixed are mutable.
#
#   [1 2 3]    tuple  (immutable)     @[1 2 3]    array  (mutable)
#   {:a 1}     struct (immutable)     @{:a 1}     table  (mutable)
#   "hello"    string (immutable)     @"hello"    buffer (mutable)

(print "=== mutability split ===")

# A contact is an immutable struct — once created, it never changes.
(def alice {:name "Alice" :email "alice@example.com" :tags [:dev :lead]})

# A contact book is a mutable table — entries come and go.
(def book @{})                     # empty mutable table

# put always returns a collection.  For mutable types it returns the
# same object (mutated in place).  For immutable types it returns a new one.
(def same-book (put book "alice" alice))  # mutates book, returns book
(assert-eq same-book book "put on table: returns the same table")

# For structs, put returns a *new* struct — the original is unchanged.
(def alice2 (put alice :phone "555-0100"))  # new struct with :phone
(assert-false (has-key? alice :phone) "put on struct: original unchanged")
(assert-eq (get alice2 :phone) "555-0100" "put on struct: new struct has key")

# Populate the book with more contacts
(put book "bob"   {:name "Bob"   :email "bob@example.com"   :tags [:dev]})
(put book "carol" {:name "Carol" :email "carol@example.com" :tags [:ops :lead]})
(put book "dave"  {:name "Dave"  :email "dave@example.com"  :tags [:ops :dev]})

(assert-eq (length (keys book)) 4 "book has 4 contacts")


# ========================================
# 2. Polymorphic get — one function, every collection
# ========================================

(print "=== polymorphic get ===")

(assert-eq (get [10 20 30] 1) 20 "get on tuple")        # index by position
(assert-eq (get @[10 20 30] 2) 30 "get on array")       # same for arrays
(assert-eq (get {:a 1 :b 2} :b) 2 "get on struct")      # index by keyword
(assert-eq (get @{:a 1 :b 2} :a) 1 "get on table")      # same for tables
(assert-eq (get "abcde" 0) "a" "get on string")          # index by grapheme

# get with a default for missing keys
(assert-eq (get book "eve" :not-found) :not-found "get with default")

# Drill into a contact via thread-first
(def alice-email (-> book (get "alice") (get :email)))  # book → alice → email
(assert-eq alice-email "alice@example.com" "thread-first: nested access")


# ========================================
# 3. Destructuring — unpack directly into bindings
# ========================================
#
# Structs destructure by key; tuples/arrays by position.

(print "=== destructuring ===")

# Destructure a contact's fields
(def {:name aname :email aemail :tags atags} (get book "alice"))  # by key
(assert-eq aname "Alice" "struct destructure: name")
(assert-eq aemail "alice@example.com" "struct destructure: email")
(assert-eq (get atags 0) :dev "struct destructure: first tag")

# Destructure a tag tuple by position
(def [first-tag second-tag] atags)   # first element, second element
(assert-eq first-tag :dev "tuple destructure: first")
(assert-eq second-tag :lead "tuple destructure: second")

# Destructure in let — nested: struct containing a tuple
(let (({:name n :tags [t & _]} (get book "carol")))  # t = first tag, _ = rest
  (assert-eq n "Carol" "let destructure: name")
  (assert-eq t :ops "let destructure: first tag"))

# Destructure in function parameters
(defn contact-summary [{:name name :email email}]  # struct pattern in params
  "One-line summary of a contact."
  (-> name (append " <") (append email) (append ">")))

(assert-eq (contact-summary alice) "Alice <alice@example.com>"
  "fn destructure: format contact")


# ========================================
# 4. Lists — the linked backbone
# ========================================
#
# Lists are built from cons cells.  They're ideal for accumulation
# and recursive processing, less so for random access.

(print "=== lists ===")

# Build a list of all contact names from the book's keys
(def names (keys book))              # keys returns a list
(assert-true (list? names) "keys returns a list")
(assert-eq (length names) 4 "four names")

# List operations
(def extended (cons "eve" names))    # cons prepends an element
(assert-eq (first extended) "eve" "cons prepends")
(assert-eq (length extended) 5 "cons adds one")
(assert-eq (first (rest extended)) (first names) "rest skips first")
(assert-eq (last names) (first (reverse names)) "last = first of reverse")

# Slicing
(assert-eq (length (take 2 names)) 2 "take 2")      # first n elements
(assert-eq (length (drop 2 names)) 2 "drop 2")      # skip n elements
(assert-eq (length (butlast names)) 3 "butlast")    # everything but last


# ========================================
# 5. each — iteration across types
# ========================================

(print "=== each ===")

# Iterate the book's keys, collect contacts into an array
(var all-contacts @[])
(each k in (keys book)             # each over a list
  (push all-contacts (get book k)))
(assert-eq (length all-contacts) 4 "each over list: collected all")

# Iterate a tuple (alice's tags)
(var tag-count 0)
(each t in (get alice :tags)       # each over a tuple
  (set tag-count (+ tag-count 1)))
(assert-eq tag-count 2 "each over tuple")

# Iterate an array
(var sum 0)
(each c in all-contacts            # each over an array
  (set sum (+ sum (length (get c :tags)))))
(assert-true (> sum 0) "each over array: summed tag counts")

# Iterate a string (by grapheme cluster — see section 10)
(var char-count 0)
(each ch in "hello"                # each over a string
  (set char-count (+ char-count 1)))
(assert-eq char-count 5 "each over string")


# ========================================
# 6. Querying — finding contacts by tag
# ========================================

(print "=== querying ===")

(defn has-tag? [contact tag]
  "Check whether a contact's tag tuple contains the given tag."
  (var found false)
  (each t in (get contact :tags)   # iterate the tags tuple
    (when (= t tag)                # when match found...
      (set found true)))           # ...flag it
  found)

(assert-true (has-tag? alice :lead) "alice is a lead")
(assert-false (has-tag? (get book "bob") :lead) "bob is not a lead")

# Collect all leads
(var leads @[])
(each k in (keys book)
  (when-let ((_ (has-tag? (get book k) :lead)))  # bind and test
    (push leads k)))
(assert-eq (length leads) 2 "two leads found")

# Collect all devs
(var devs @[])
(each k in (keys book)
  (when (has-tag? (get book k) :dev)  # simpler: no binding needed
    (push devs k)))
(assert-eq (length devs) 3 "three devs found")


# ========================================
# 7. Formatting — destructuring and string building
# ========================================

(print "=== formatting ===")

(defn format-tags [tags]
  "Format a tag tuple as a comma-separated string in brackets."
  (var parts (list))               # accumulate into a list
  (each t in tags
    (set parts (append parts (list (keyword->string t)))))  # :dev → "dev"
  (-> "[" (append (string/join parts ", ")) (append "]")))  # "[dev, lead]"

(defn format-contact [{:name name :email email :tags tags}]  # destructure param
  "Format a contact for display."
  (-> name
      (append " <")               # thread-first builds the string
      (append email)
      (append "> ")
      (append (format-tags tags))))

(def alice-str (format-contact alice))
(assert-true (string/contains? alice-str "Alice") "formatted: has name")
(assert-true (string/contains? alice-str "alice@example.com") "formatted: has email")
(assert-true (string/contains? alice-str "dev") "formatted: has tag")

# Format every contact in the book
(var formatted @[])
(each k in (keys book)
  (push formatted (format-contact (get book k))))
(assert-eq (length formatted) 4 "formatted all contacts")
(each line in formatted
  (assert-true (string/contains? line "@") "every line has an email"))


# ========================================
# 8. String processing — cleaning imported data
# ========================================

(print "=== string processing ===")

# Imagine importing raw CSV contact names
(def raw-input "  Alice, Bob , Carol , Dave  ")

# string/split returns a list
(def split-names (string/split raw-input ","))  # split on commas
(assert-eq (length split-names) 4 "split into 4 parts")

# Clean up whitespace
(var clean @[])
(each n in split-names
  (push clean (string/trim n)))    # trim leading/trailing whitespace
(assert-eq (get clean 0) "Alice" "trimmed first name")
(assert-eq (get clean 3) "Dave" "trimmed last name")

# Case operations
(assert-eq (string/upcase "hello") "HELLO" "upcase")
(assert-eq (string/downcase "HELLO") "hello" "downcase")

# Replacement
(assert-eq (string/replace "foo-bar-baz" "-" "_") "foo_bar_baz" "replace")

# Searching
(assert-true (string/starts-with? alice-email "alice") "starts-with?")
(assert-true (string/ends-with? alice-email ".com") "ends-with?")
(assert-eq (string/find alice-email "@") 5 "find: @ in email")  # grapheme index
(assert-eq (string/find "abcabc" "bc" 2) 4 "find: with offset")  # start at 2
(assert-eq (string/find "hello" "xyz") nil "find: not found → nil")

# Slicing — extract the domain from an email
(defn email-domain [email]
  "Extract the domain part of an email address."
  (let* ([at-pos (string/find email "@")]        # find the @
         [domain (string/slice email (+ at-pos 1))])  # slice from after @
    domain))

(assert-eq (email-domain "alice@example.com") "example.com" "email-domain")


# ========================================
# 9. Array mutation — managing an invite list
# ========================================

(print "=== array mutation ===")

# Build an invite list from the leads
(var invites @[:alice :carol])

# Insert at a position
(insert invites 1 :bob)           # insert :bob at index 1
(assert-eq (get invites 1) :bob "insert at index 1")
(assert-eq (length invites) 3 "insert increases length")

# Remove someone
(remove invites 1)                 # remove element at index 1
(assert-eq (length invites) 2 "remove decreases length")
(assert-eq (get invites 1) :carol "remove shifts elements")

# Push and pop — stack-style operations on the end
(push invites :dave)               # append to end
(assert-eq (length invites) 3 "push extends")
(def popped (pop invites))         # remove and return last
(assert-eq popped :dave "pop returns last")
(assert-eq (length invites) 2 "pop decreases length")


# ========================================
# 10. Strings are grapheme clusters, not bytes
# ========================================
#
# String indexing, length, and iteration operate on Unicode grapheme
# clusters — what humans perceive as "characters."  An emoji with a
# skin-tone modifier is one element, not two codepoints or four bytes.

(print "=== grapheme clusters ===")

(assert-eq (length "hello") 5 "ASCII: one grapheme per byte")
(assert-eq (length "héllo") 5 "precomposed é: one grapheme")
(assert-eq (length "👋🏽") 1 "wave + skin tone: one grapheme cluster")
(assert-eq (get "👋🏽" 0) "👋🏽" "get returns the whole cluster")

# Iterating a string with each yields grapheme clusters
(var graphemes @[])
(each g in "aé👋🏽"                  # three graphemes, not five codepoints
  (push graphemes g))
(assert-eq (length graphemes) 3 "three grapheme clusters")
(assert-eq (get graphemes 0) "a" "first grapheme: a")
(assert-eq (get graphemes 1) "é" "second grapheme: é")
(assert-eq (get graphemes 2) "👋🏽" "third grapheme: wave emoji")

# Flag emoji — two regional indicator codepoints, one grapheme
(assert-eq (length "🇫🇷") 1 "flag emoji: one grapheme cluster")

# Slicing respects grapheme boundaries
(assert-eq (string/slice "héllo" 1 4) "éll" "slice: grapheme indices")

# string/find returns grapheme index, not byte offset
(assert-eq (string/find "aé👋🏽bc" "👋🏽") 2 "find: grapheme index of emoji")
(assert-eq (string/find "aé👋🏽bc" "bc") 3 "find: grapheme index past emoji")


# ========================================
# 11. Table mutation — updating and removing contacts
# ========================================

(print "=== table mutation ===")

(assert-true (has-key? book "dave") "dave exists before del")
(del book "dave")                  # remove a key from the table
(assert-false (has-key? book "dave") "del removes entry")
(assert-eq (length (keys book)) 3 "three contacts remain")

# Update alice's tags — replace the whole contact (structs are immutable)
(def updated-alice (put alice :tags [:dev :lead :admin]))  # new struct
(put book "alice" updated-alice)   # replace in table
(def {:tags new-tags} (get book "alice"))  # destructure to check
(assert-eq (length new-tags) 3 "alice now has 3 tags")


# ========================================
# 12. Struct operations — immutable by design
# ========================================
#
# Structs support put and del, but always return new structs.

(print "=== struct ops ===")

(def point {:x 1 :y 2 :z 3})
(def point2d (struct/del point :z))   # new struct without :z

(assert-eq (get point :z) 3 "original struct unchanged")
(assert-eq (get point2d :x) 1 "new struct has x")
(assert-false (has-key? point2d :z) "new struct lacks z")

# put on struct returns new struct
(def point-color (put point :color :red))  # add a field
(assert-false (has-key? point :color) "original still has no color")
(assert-eq (get point-color :color) :red "new struct has color")

# Destructure to extract multiple fields at once
(def {:x px :y py :z pz} point)   # unpack all three coordinates
(assert-eq (+ px py pz) 6 "struct destructure: sum of coordinates")


# ========================================
# 13. Concat vs append
# ========================================
#
# concat: always returns a new value.  Neither argument is mutated.
# append: for mutable types, mutates the first argument in place.

(print "=== concat vs append ===")

(def t1 [1 2])
(def t2 [3 4])
(def t3 (concat t1 t2))           # new tuple, originals unchanged
(assert-eq (length t1) 2 "concat: original tuple unchanged")
(assert-eq (length t3) 4 "concat: new tuple has all elements")

(def a1 @[1 2])
(def a2 @[3 4])
(append a1 a2)                     # mutates a1 in place
(assert-eq (length a1) 4 "append: mutable array extended in place")

(def s1 "hello")
(def s2 " world")
(def s3 (concat s1 s2))           # new string
(assert-eq s1 "hello" "concat: original string unchanged")
(assert-eq s3 "hello world" "concat: new string")


# ========================================
# 14. Splice — spreading into calls
# ========================================
#
# ;expr spreads an array or tuple into a function call's arguments.

(print "=== splice ===")

(def nums @[1 2 3])
(assert-eq (+ ;nums) 6 "splice: spread array into +")  # (+ 1 2 3)

(def more [10 20])
(assert-eq (+ ;more) 30 "splice: spread tuple into +")  # (+ 10 20)

# Splice in data constructors
(def base @[1 2])
(def extended-arr @[;base 3 4])    # spread base into new array
(assert-eq (length extended-arr) 4 "splice in array literal")
(assert-eq (get extended-arr 2) 3 "splice: correct element order")


# ========================================
# 15. Putting it together — export the book as CSV
# ========================================

(print "=== CSV export ===")

(defn export-csv [the-book]
  "Export the contact book as CSV lines."
  (var lines @["name,email,tags"])   # header row
  (each k in (keys the-book)
    (let (({:name name :email email :tags tags} (get the-book k)))  # destructure
      (push lines (-> name
                      (append ",")
                      (append email)       # thread-first builds each line
                      (append ",")
                      (append (format-tags tags))))))
  lines)

(def csv (export-csv book))
(assert-eq (get csv 0) "name,email,tags" "csv: header line")
(assert-eq (length csv) 4 "csv: header + 3 data lines")

# Every data line should contain an @
(each line in (rest (list ;csv))   # ;csv spreads array into list constructor
  (assert-true (string/contains? line "@") "csv: data has email"))

# Verify a specific line
(assert-true (string/contains? (get csv 1) "Alice") "csv: first data line is alice")


(print "=== All collection tests passed ===")
