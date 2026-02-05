;;; LLVM IR Generation Example
;;;
;;; Demonstrates FFI integration with LLVM C API:
;;; - LLVM context creation
;;; - Module and function creation
;;; - Basic block and IR builder setup
;;; - IR instruction generation
;;; - Module dumping and introspection
;;;
;;; To run:
;;; (load "examples/llvm-compiler.lisp")
;;;
;;; This example generates LLVM IR equivalent to:
;;; ```c
;;; int main() {
;;;   return 42;
;;; }
;;; ```
;;;
;;; Requirements:
;;; - LLVM C API development libraries installed
;;; - libLLVM.so in system library path

;;; Load LLVM-C header and auto-generate bindings
(load-header-with-lib "/usr/include/llvm-c/Core.h"
                       "/usr/lib/libLLVM.so")

;;; Create LLVM module
;;; LLVMModuleRef LLVMModuleCreateWithName(const char *ModuleID)
(let module (llvm-module-create-with-name "hello"))

;;; Get the global context
;;; LLVMContextRef LLVMGetGlobalContext(void)
(let context (llvm-get-global-context))

;;; Create the integer type for i32 (32-bit integer)
;;; LLVMTypeRef LLVMInt32Type(void)
(let i32-type (llvm-int32-type))

;;; Create function type: () -> i32
;;; This represents a function that takes no arguments and returns an i32
;;; LLVMTypeRef LLVMFunctionType(LLVMTypeRef ReturnType,
;;;                              LLVMTypeRef *ParamTypes,
;;;                              unsigned ParamCount,
;;;                              LLVMBool IsVarArg)
;;; For simplicity, we pass:
;;; - return type: i32
;;; - param types: empty array (represented as #())
;;; - param count: 0
;;; - is vararg: false (#f)
(let func-type (llvm-function-type i32-type #() #f))

;;; Add function to module: int main()
;;; LLVMValueRef LLVMAddFunction(LLVMModuleRef M,
;;;                              const char *Name,
;;;                              LLVMTypeRef FunctionType)
(let func (llvm-add-function module "main" func-type))

;;; Create IR builder for inserting instructions
;;; LLVMBuilderRef LLVMCreateBuilderInContext(LLVMContextRef C)
(let builder (llvm-create-builder-in-context context))

;;; Create basic block named "entry"
;;; This is where the function's first instructions will go
;;; LLVMBasicBlockRef LLVMAppendBasicBlockInContext(LLVMContextRef C,
;;;                                                  LLVMValueRef Fn,
;;;                                                  const char *Name)
(let bb (llvm-append-basic-block-in-context context func "entry"))

;;; Position builder at the end of the basic block
;;; All subsequent instructions will be inserted here
;;; void LLVMPositionBuilderAtEnd(LLVMBuilderRef Builder,
;;;                                LLVMBasicBlockRef Block)
(llvm-position-builder-at-end builder bb)

;;; Create constant integer value 42
;;; LLVMValueRef LLVMConstInt(LLVMTypeRef IntTy,
;;;                           unsigned long long N,
;;;                           LLVMBool SignExtend)
(let forty-two (llvm-const-int i32-type 42 #f))

;;; Build return instruction: return 42
;;; LLVMValueRef LLVMBuildRet(LLVMBuilderRef B,
;;;                           LLVMValueRef V)
(llvm-build-ret builder forty-two)

;;; Dump the module to stdout
;;; This prints human-readable LLVM IR
;;; Expected output:
;;;   ; ModuleID = 'hello'
;;;   define i32 @main() {
;;;   entry:
;;;     ret i32 42
;;;   }
;;; void LLVMDumpModule(LLVMModuleRef M)
(llvm-dump-module module)

;;; Cleanup: Dispose of the builder
;;; This frees the builder's resources
;;; void LLVMDisposeBuilder(LLVMBuilderRef Builder)
(llvm-dispose-builder builder)

;;; Cleanup: Dispose of the module
;;; This frees the module and all its contents (functions, types, etc.)
;;; void LLVMDisposeModule(LLVMModuleRef M)
(llvm-dispose-module module)

;;; Notes:
;;; - LLVM uses reference counting for memory management
;;; - Modules contain functions, global variables, and metadata
;;; - Basic blocks are sequential containers of instructions
;;; - The IR builder is a convenience tool for generating instructions
;;; - Function types are defined independently from functions
;;; - All C pointers (contexts, modules, functions, etc.) are managed by Elle
;;; - This example generates valid LLVM IR that could be compiled to machine code
;;; - In a real compiler, you'd generate control flow, calls, and more complex operations
