-- | Usefull statement templates

module Toy.Lang.Primitives where

import           Universum          hiding (drop)

import           Toy.Base           (FunName (..), Var)
import           Toy.Exp.Data       (Exp (..), UserLabelId, readE)
import           Toy.Exp.Operations
import           Toy.Lang.Data      (Stmt (..))

-- | Drops given expression
drop :: Exp -> Stmt
drop e = "_" := e

-- | @while@ loop in terms of `Stmt`.
while :: Exp -> Stmt -> Stmt
while cond stmt = If cond (DoWhile stmt cond) Skip

-- | @repeat@ loop in terms of `Stmt`.
repeat :: Stmt -> Exp -> Stmt
repeat stmt stop = DoWhile stmt (stop ==: 0)

-- | @repeat@ loop in terms of `Stmt`.
for :: Stmt -> Exp -> Stmt -> Stmt -> Stmt
for s1 cond sr body = s1 <> while cond (body <> sr)

-- | @read@ to a given variable.
read :: Var -> Stmt
read v = v := readE

-- | Function call in terms of `Stmt`.
funCall :: Var -> [Exp] -> Stmt
funCall name args = drop $ FunE (FunName name) args

-- | @write@ given expression.
write :: Exp -> Stmt
write = funCall "write" . pure

-- | Array initializer, which imideatelly writes to variable.
arrayVar :: Var -> [Exp] -> Stmt
arrayVar var exps = mconcat
    [ var := ArrayUninitE (length exps)
    , uncurry (ArrayAssign $ VarE var) `foldMap` (zip (map ValueE [0..]) exps)
    ]

-- | Array initializer, which allows to get array as exression.
array :: (Exp -> Stmt) -> [Exp] -> Stmt
array f exps = mconcat
    [ arrayVar "_" exps
    , f "_"
    ]

-- | Goto given label by name.
goto :: UserLabelId -> Stmt
goto = Goto . LabelE

