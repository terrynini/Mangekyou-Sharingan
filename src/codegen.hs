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
    currentBlock :: Name
} deriving Show