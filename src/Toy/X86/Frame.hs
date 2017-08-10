-- | This module contains operations to form function stack frame and access
-- its elements.

module Toy.X86.Frame
    ( Frame
    , mkFrame
    , resolveMemRefs
    , evalStackShift
    ) where

import           Control.Applicative ((<|>))
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as S
import           Formatting          (build, sformat, (%))
import           Universum           hiding (Const)

import           Toy.Base            (Var)
import           Toy.X86.Data        (Inst, Operand (..), traverseOperands)
import           Toy.X86.SymStack    (SymStackSpace, regSymStack)

---------------------------
------ Frame layout -------
---------------------------
-- Argument n
-- ...
-- Argument 1
-- --- <return address> ---
-- Registers backup space
-- Local variables
-- Symbolic stack
---------------------------
-- inspired by @wotopul

data Frame = Frame
    { fArgs :: M.Map Var Int  -- ^ names of arguments
    , fVars :: M.Map Var Int  -- ^ names of variables
    , fSym  :: SymStackSpace  -- ^ space size for symbolic stack
    }

mkFrame :: [Var] -> S.Set Var -> SymStackSpace -> Frame
mkFrame args vars fSym =
    let vars' = S.toList $ foldr S.delete vars args
        fArgs = M.fromList $ zip args [0..]
        fVars = M.fromList $ zip vars' [0..]
    in  Frame {..}

resolveMemRefs :: Traversable f => Frame -> f Inst -> f Inst
resolveMemRefs Frame{..} = fmap $ traverseOperands %~ \case
    Stack i  -> Mem i
    Backup i -> Mem (stSymSize + varsNum + i)
    Local n  ->
        let noVar = error $ sformat ("No such variable / argument: "%build) n
            asVar i = Mem (stSymSize + i)
            asArg i = Mem (stSymSize + varsNum + backupSize + 1 + i)
        in  fromMaybe noVar $ asVar <$> M.lookup n fVars
                          <|> asArg <$> M.lookup n fArgs
    o@HardMem{} -> o
    o@HeapMem{} -> o
    o@HeapMemExt{} -> o
    Mem _       -> error "Resolving Mem reference??"
    o@Reg{}     -> o
    o@Const{}   -> o
    o@GlobalVar{} -> o
  where
    backupSize = length regSymStack
    stSymSize  = fromIntegral fSym
    varsNum    = M.size fVars

evalStackShift :: Frame -> Int
evalStackShift Frame{..} = fromIntegral fSym + M.size fVars + length regSymStack
