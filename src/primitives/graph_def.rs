use crate::pipeline::eval;
use crate::symbol::SymbolTable;
use crate::vm::VM;

/// Define fn/dot-escape, fn/graph, and fn/save-graph as Elle functions.
///
/// These use only special forms (def, fn, let, if, begin, set, while) —
/// no prelude macros (let*, ->, each, defn) — because init_stdlib runs
/// before the symbol table thread-local is set, and those macros need
/// gensym which requires that thread-local.
pub fn define_graph_functions(vm: &mut VM, symbols: &mut SymbolTable) {
    let dot_escape_code = r#"
        (def fn/dot-escape (fn (s)
          (string/replace
            (string/replace
              (string/replace
                (string/replace
                  (string/replace s "{" "\\{")
                  "}" "\\}")
                "|" "\\|")
              "<" "\\<")
            ">" "\\>")))
    "#;

    let graph_code = r#"
        (def fn/graph (fn (cfg)
          (let ((name (get cfg :name)))
            (let ((doc (get cfg :doc)))
              (let ((label (if (nil? name)
                             (if (nil? doc) "anonymous" doc)
                             name)))
                (let ((result
                        (append
                          (append
                            (append
                              (append
                                (append
                                  (append
                                    (append
                                      (append "digraph {\n  label=\""
                                        label)
                                      " arity:")
                                    (get cfg :arity))
                                  " regs:")
                                (string (get cfg :regs)))
                              " locals:")
                            (string (get cfg :locals)))
                          "\";\n  node [shape=record];\n")))
              (let ((blocks (get cfg :blocks)))
                (let ((bi 0))
                  (while (< bi (length blocks))
                    (let ((block (get blocks bi)))
                      (let ((lbl (string (get block :label))))
                        (let ((instrs (get block :instrs)))
                          (let ((term (get block :term)))
                            (let ((edges (get block :edges)))
                              (set result
                                (append
                                  (append
                                    (append
                                      (append result "  block")
                                      lbl)
                                    " [label=\"{block")
                                  lbl))
                              (set result (append result "|"))
                              (let ((ii 0))
                                (while (< ii (length instrs))
                                  (begin
                                    (set result
                                      (append
                                        (append result (fn/dot-escape (get instrs ii)))
                                        "\\l"))
                                    (set ii (+ ii 1)))))
                              (set result
                                (append
                                  (append
                                    (append result "|")
                                    (fn/dot-escape term))
                                  "}\"];\n"))
                              (let ((ei 0))
                                (while (< ei (length edges))
                                  (begin
                                    (set result
                                      (append
                                        (append
                                          (append
                                            (append
                                              (append result "  block")
                                              lbl)
                                            " -> block")
                                          (string (get edges ei)))
                                        ";\n"))
                                    (set ei (+ ei 1)))))))))
                      (set bi (+ bi 1))))))
              (append result "}\n")))))))
    "#;

    let save_graph_code = r#"
        (def fn/save-graph (fn (closure path)
          (file/write path (fn/graph (fn/flow closure)))))
    "#;

    for (name, code) in &[
        ("fn/dot-escape", dot_escape_code),
        ("fn/graph", graph_code),
        ("fn/save-graph", save_graph_code),
    ] {
        if let Err(e) = eval(code, symbols, vm) {
            eprintln!("Warning: Failed to define {}: {}", name, e);
        }
    }
}
