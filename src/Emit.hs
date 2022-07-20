module Emit where

import Codegen
import Syntax
import LLVM.Module
import LLVM.Context


codegenTop:: Syntax.Expr -> LLVM ()
codegenTop Syntax.Function name args body = do