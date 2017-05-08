{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Toy.X86.Frame
    ( Frame
    , mkFrame
    , resolveMemRefs
    , evalStackShift
    ) where

import           Control.Applicative ((<|>))
import           Control.Lens        ((%~))
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as S
import           Formatting          (build, formatToString, (%))

import           Toy.Exp             (Var)
import           Toy.X86.Data        (Inst, Operand (..), traverseOperands)
import           Toy.X86.SymStack    (SymStackSpace, regSymStack)

---------------------------
-- arguments
-- --- <return address> ---
-- registers backup space
-- local variables
-- symbolic stack
---------------------------

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
        let noVar = error $formatToString ("No such variable / argument: "%build) n
            asVar i = Mem (stSymSize + i)
            asArg i = Mem (stSymSize + varsNum + backupSize + 1 + i)
        in  fromMaybe noVar $ asVar <$> M.lookup n fVars
                          <|> asArg <$> M.lookup n fArgs
    o@HardMem{} -> o
    Mem _    -> error "Resolving Mem reference??"
    other    -> other
  where
    backupSize = length regSymStack
    stSymSize  = fromIntegral fSym
    varsNum    = M.size fVars

evalStackShift :: Frame -> Int
evalStackShift Frame{..} =
    fromIntegral fSym + M.size fVars + length regSymStack
