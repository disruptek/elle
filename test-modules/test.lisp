# Simple test module for import-file tests
# This file contains basic definitions for testing module loading

(var test-var 42)
(var test-string "hello")
(var test-list (list 1 2 3))

# Module exports
(fn [] {:test-var test-var :test-string test-string :test-list test-list})
