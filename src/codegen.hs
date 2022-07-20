module Codegen where

{--
stack install type-level
root@a27cbd2de9e2:/# stack install llvm-hs
--}

import Control.Monad.State

double :: Type
double = FloatingPointType 64 IEEE

-- use the state monad to maintain the state http://cnhaskell.com/chp/14.html#the-state-monad
newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a}
    deriving (Functor, Applicative, Monad, MonadState CodeGenState)

-- the state record for CodeGen state monad
data CodeGenState =
    CodeGenState {
    currentBlock :: Name        -- the Name of current working block, just string
} deriving Show

newtype LLVM a = LLVM (State AST.Module a)
    deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM module_ (LLVM m) = execState m module_

-- modify the state and return the blockname as result
setBlock :: Name -> CodeGen Name
setBlock name = do
    modify $ \s -> s{currentBlock = name}
    return name

{-
The member `currentBlock` of record type is a function with type CodeGenState -> Name,
and the gets is `gets f = do { x <- get; return (f x) }`,
hence the final result is `CodeGen Name`
-}
getBlock :: CodeGen Name
getBlock = gets currentBlock

{-
Need to pass a LLVM.AST.Module to moduleLLVMAssembly to emit LLVM IR
-}


{-
term :: Named Terminator
term = Do $ Ret (Just (ConstantOperand $ LLVM.AST.Constant.Float $ LLVM.AST.Float.Double 1.0)) []

ref = UnName 1
ins = [ref := FAdd LLVM.AST.noFastMathFlags (ConstantOperand $ LLVM.AST.Constant.Float $ LLVM.AST.Float.Double 1.0) (ConstantOperand $ LLVM.AST.Constant.Float $ LLVM.AST.Float.Double 2.0) []]
bb = BasicBlock (UnName 2) ins term

define ::  Type -> ShortByteString -> [(Type, Name)] -> [BasicBlock] -> Definition
define retty label argtys body =
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

--set -XOverloadedStrings
define double "main" [] [bb]
let m = defaultModule { moduleName = "123", moduleDefinitions = [define double "main" [] [bb]] }
import qualified Data.ByteString.Char8 as B

toBS = B.pack

:{
    withContext $ \context ->
    withModuleFromAST context m $ \mm -> do
    llstr <- moduleLLVMAssembly mm
    B.putStrLn llstr
:}
-}
