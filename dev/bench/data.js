window.BENCHMARK_DATA = {
  "lastUpdate": 1772756624199,
  "repoUrl": "https://github.com/elle-lisp/elle",
  "entries": {
    "Elle Benchmarks": [
      {
        "commit": {
          "author": {
            "email": "disruptek@users.noreply.github.com",
            "name": "Smooth Operator",
            "username": "disruptek"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "73f55acfa3727b48a2376170448d2603e70ba437",
          "message": "Add (environment) primitive and defined-globals tracking (#485)\n\n* Add defined_globals tracking to VM for O(defined) environment enumeration\n\n* Add (environment) primitive via SIG_QUERY for runtime introspection\n\n* Fix rustdoc warning: escape angle brackets in environment doc comment",
          "timestamp": "2026-03-05T20:26:22Z",
          "tree_id": "e66304d18d1707d54088783daecf5aeccb670740",
          "url": "https://github.com/elle-lisp/elle/commit/73f55acfa3727b48a2376170448d2603e70ba437"
        },
        "date": 1772746310500,
        "tool": "cargo",
        "benches": [
          {
            "name": "parsing/simple_number",
            "value": 158,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/list_literal",
            "value": 1306,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/nested_expr",
            "value": 2193,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/deep_nesting",
            "value": 1401,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/large_list_100",
            "value": 25369,
            "range": "± 534",
            "unit": "ns/iter"
          },
          {
            "name": "symbol_interning/first_intern",
            "value": 75,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "symbol_interning/repeat_intern",
            "value": 9,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "symbol_interning/many_unique",
            "value": 18638,
            "range": "± 126",
            "unit": "ns/iter"
          },
          {
            "name": "compilation/simple_arithmetic",
            "value": 355006,
            "range": "± 25423",
            "unit": "ns/iter"
          },
          {
            "name": "compilation/conditional",
            "value": 460471,
            "range": "± 31776",
            "unit": "ns/iter"
          },
          {
            "name": "compilation/nested_arithmetic",
            "value": 465992,
            "range": "± 8240",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/int_add",
            "value": 567,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/mixed_arithmetic",
            "value": 441,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/comparison",
            "value": 280,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/cons",
            "value": 1002,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/first",
            "value": 877,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "conditionals/if_true",
            "value": 552,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "conditionals/nested_if",
            "value": 7333,
            "range": "± 490",
            "unit": "ns/iter"
          },
          {
            "name": "end_to_end/simple",
            "value": 620729,
            "range": "± 26411",
            "unit": "ns/iter"
          },
          {
            "name": "end_to_end/complex",
            "value": 637914,
            "range": "± 20398",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/10",
            "value": 1776,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/10",
            "value": 810,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/50",
            "value": 32457,
            "range": "± 3937",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/50",
            "value": 22512,
            "range": "± 1272",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/100",
            "value": 62870,
            "range": "± 9336",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/100",
            "value": 43694,
            "range": "± 2494",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/500",
            "value": 281270,
            "range": "± 43208",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/500",
            "value": 195754,
            "range": "± 21501",
            "unit": "ns/iter"
          },
          {
            "name": "memory_operations/value_clone",
            "value": 0,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "memory_operations/list_to_vec",
            "value": 113,
            "range": "± 78",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "disruptek@users.noreply.github.com",
            "name": "Smooth Operator",
            "username": "disruptek"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "41766f4d5f4397af35b67c523d98a35700543f66",
          "message": "escape analysis (#421)\n\n* migrate prelude and eval integration tests to Elle scripts\n\nMove 48 prelude tests and 46 eval tests from Rust integration tests\nto Elle test scripts in tests/elle/. Both files were 100% eligible\nfor migration. Delete the Rust originals and update mod.rs.\n\n* escape analysis: add 9 missing primitive aliases to whitelist, wrong-arity test\n\nAdd eq?, string-contains?, string-starts-with?, string-ends-with?,\ncoroutine?, fn/mutates-params?, mutates-params?, fn/raises?, raises?\nto IMMEDIATE_PRIMITIVES (now 48). Aliases get their own SymbolId so\nthey're invisible to both intrinsics map and whitelist unless explicit.\nAdd wrong-arity safety test.\n\n* fix: fn/raises? → fn/errors? — Elle has (error), not raise\n\nThe primitive is registered as fn/errors? with no aliases. fn/raises? and\nraises? were phantom whitelist entries silently ignored by the SymbolId\nlookup. Fixed in IMMEDIATE_PRIMITIVES and AGENTS.md.\n\n* fix: portable temp paths in eval.lisp, add list splice tests\n\n* tier proptest cases: 8 for PRs, 64 for merge queue, 128 for weekly\n\n* fix: resolve PatternKey API mismatch after rebase\n\n* migrate: move property tests to elle scripts for faster CI\n\n* ci: reduce proptest cases to 16 for regular CI (nightly/weekly run 64/128)\n\n* docs: improve doc comment for can_scope_allocate_let\n\n* ci: retry after checking for stuck runs",
          "timestamp": "2026-03-05T21:15:59Z",
          "tree_id": "e519e41a7dad5a5d61b84e7ff20c662197a9a2db",
          "url": "https://github.com/elle-lisp/elle/commit/41766f4d5f4397af35b67c523d98a35700543f66"
        },
        "date": 1772748619094,
        "tool": "cargo",
        "benches": [
          {
            "name": "parsing/simple_number",
            "value": 158,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/list_literal",
            "value": 1287,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/nested_expr",
            "value": 2190,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/deep_nesting",
            "value": 1343,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/large_list_100",
            "value": 25030,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "symbol_interning/first_intern",
            "value": 75,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "symbol_interning/repeat_intern",
            "value": 9,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "symbol_interning/many_unique",
            "value": 17360,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "compilation/simple_arithmetic",
            "value": 261847,
            "range": "± 24099",
            "unit": "ns/iter"
          },
          {
            "name": "compilation/conditional",
            "value": 344229,
            "range": "± 32445",
            "unit": "ns/iter"
          },
          {
            "name": "compilation/nested_arithmetic",
            "value": 429515,
            "range": "± 60042",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/int_add",
            "value": 581,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/mixed_arithmetic",
            "value": 442,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/comparison",
            "value": 283,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/cons",
            "value": 1005,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/first",
            "value": 882,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "conditionals/if_true",
            "value": 581,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "conditionals/nested_if",
            "value": 3393,
            "range": "± 657",
            "unit": "ns/iter"
          },
          {
            "name": "end_to_end/simple",
            "value": 557276,
            "range": "± 36115",
            "unit": "ns/iter"
          },
          {
            "name": "end_to_end/complex",
            "value": 573114,
            "range": "± 34659",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/10",
            "value": 1774,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/10",
            "value": 824,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/50",
            "value": 29662,
            "range": "± 5889",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/50",
            "value": 15142,
            "range": "± 2992",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/100",
            "value": 60765,
            "range": "± 9438",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/100",
            "value": 28304,
            "range": "± 5829",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/500",
            "value": 263626,
            "range": "± 41723",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/500",
            "value": 164867,
            "range": "± 14510",
            "unit": "ns/iter"
          },
          {
            "name": "memory_operations/value_clone",
            "value": 0,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "memory_operations/list_to_vec",
            "value": 113,
            "range": "± 1",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "disruptek@users.noreply.github.com",
            "name": "Smooth Operator",
            "username": "disruptek"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e3707d6f74498590c3a923925853918db74873aa",
          "message": "escape: tier 6 — while/block/break-aware scope allocation (#488)\n\n* escape: tier 6 — while/block/break-aware scope allocation\n\nAdd While, Block, and Break handling to result_is_safe in escape\nanalysis. While always returns nil (safe). Block checks both normal\nexit (last expression) and all break values targeting the block.\nBreak is safe in result position (it jumps away, never produces a\nvalue locally).\n\nRelax can_scope_allocate_block: instead of rejecting all blocks with\nbreaks, check that all break values targeting the block are safe\nimmediates. Relax can_scope_allocate_let similarly: instead of\nrejecting all lets with breaks, check that all break values are safe.\n\nRemove dead walkers (body_contains_break, hir_contains_break,\nwalk_for_break) replaced by value-aware variants.\n\nAdd 18 new escape analysis tests (positive, negative, correctness).\nFlip 2 existing tests that now correctly emit regions.\n\nRefs #421\n\n* fix: disable scope allocation for let bindings with escaping breaks\n\n* fix: reconcile tier 6 escape analysis with merged #421 changes\n\n* fix: record region depth before RegionEnter in block lowering\n\nBreaks targeting a scope-allocated block must emit compensating\nRegionExit instructions for the block's own region. The block\ncontext's region_depth_at_entry was recorded AFTER emit_region_enter(),\nso breaks computed 0 compensating exits and leaked the region mark.\n\nFix: record depth before RegionEnter. Update the arena test to\nexpect 2 RegionExit (1 compensating from break + 1 normal exit)\nnow that the block qualifies for scope allocation under tier 6.",
          "timestamp": "2026-03-05T23:27:36Z",
          "tree_id": "750981aa2dc0cc6db9e7f5dbc9e916571e1ec6da",
          "url": "https://github.com/elle-lisp/elle/commit/e3707d6f74498590c3a923925853918db74873aa"
        },
        "date": 1772756623153,
        "tool": "cargo",
        "benches": [
          {
            "name": "parsing/simple_number",
            "value": 138,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/list_literal",
            "value": 1228,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/nested_expr",
            "value": 2105,
            "range": "± 173",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/deep_nesting",
            "value": 1278,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "parsing/large_list_100",
            "value": 23339,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "symbol_interning/first_intern",
            "value": 61,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "symbol_interning/repeat_intern",
            "value": 7,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "symbol_interning/many_unique",
            "value": 18239,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "compilation/simple_arithmetic",
            "value": 240812,
            "range": "± 21947",
            "unit": "ns/iter"
          },
          {
            "name": "compilation/conditional",
            "value": 293907,
            "range": "± 14470",
            "unit": "ns/iter"
          },
          {
            "name": "compilation/nested_arithmetic",
            "value": 360372,
            "range": "± 28886",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/int_add",
            "value": 698,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/mixed_arithmetic",
            "value": 481,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/comparison",
            "value": 243,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/cons",
            "value": 1069,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "vm_execution/first",
            "value": 972,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "conditionals/if_true",
            "value": 717,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "conditionals/nested_if",
            "value": 4213,
            "range": "± 997",
            "unit": "ns/iter"
          },
          {
            "name": "end_to_end/simple",
            "value": 467904,
            "range": "± 28813",
            "unit": "ns/iter"
          },
          {
            "name": "end_to_end/complex",
            "value": 516326,
            "range": "± 24152",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/10",
            "value": 1673,
            "range": "± 1115",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/10",
            "value": 892,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/50",
            "value": 29064,
            "range": "± 3852",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/50",
            "value": 15931,
            "range": "± 3503",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/100",
            "value": 54674,
            "range": "± 7015",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/100",
            "value": 40082,
            "range": "± 5701",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/list_construction/500",
            "value": 218421,
            "range": "± 25432",
            "unit": "ns/iter"
          },
          {
            "name": "scalability/addition_chain/500",
            "value": 146967,
            "range": "± 15908",
            "unit": "ns/iter"
          },
          {
            "name": "memory_operations/value_clone",
            "value": 0,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "memory_operations/list_to_vec",
            "value": 121,
            "range": "± 0",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}