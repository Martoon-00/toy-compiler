-- | Simple library linkage capabilities for functions written on language
-- (not ones defined in runtime.o).

module Toy.Lang.Lib where

import           Universum

import           Toy.Base.Fun  (FunSign (..))
import           Toy.Exp       (Exp (..), (<:))
import           Toy.Lang.Data (FunDecls, Program (..), Stmt (..), forS, funCallS, ifS,
                                mkFunDecls)
import           Toy.Util      ((>:))

-- * Library

newtype Library = Library FunDecls

linkLib :: Library -> Program -> Program
linkLib (Library libFuns) prog = prog{ pFunDecls = libFuns <> pFunDecls prog }

-- * Stdlib

stdLib :: Library
stdLib = Library $ mkFunDecls
    [ FunSign "strmake" ["n", "def"] >:
        Return $ FunE "arrmake" ["n", "def"]

    , FunSign "strlen" ["s"] >:
        Return $ FunE "arrlen" ["s"]

    , FunSign "strget" ["s", "i"] >:
        Return $ ArrayAccessE "s" "i"

    , FunSign "strset" ["s", "i", "c"] >:
        ArrayAssign "s" "i" "c"

    , FunSign "printList" ["a"] >:
        forS ("i" := 0, "i" <: FunE "arrlen" ["a"], "i" := "i" + 1) $
            funCallS "write" [ArrayAccessE "a" "i"]

    , FunSign "strsub" ["s", "i", "k"] >: mconcat
        [ "s0" := ArrayUninitE "k"
        , forS ("j" := 0, "j" <: "k", "j" := "j" + 1) $
            ArrayAssign "s0" "j" (ArrayAccessE "s" ("i" + "j"))
        , Return "s0"
        ]

    , FunSign "strdup" ["s"] >:
        Return $ FunE "strsub" ["s", 0, FunE "strlen" ["s"]]

    , FunSign "strcmp" ["s1", "s2"] >: mconcat
        [ "n1" := FunE "strlen" ["s1"]
        , "n2" := FunE "strlen" ["s2"]
        , If ("n1" <: "n2")
             ("m" := "n1")
             ("m" := "n2")
        , forS ("i" := 0, "i" <: "m", "i" := "i" + 1) $ mconcat
            [ "c1" := ArrayAccessE "s1" "i"
            , "c2" := ArrayAccessE "s2" "i"
            , ifS ("c1" <: "c2") $
                Return (-1)
            , ifS ("c2" <: "c1") $
                Return 1
            ]
        , ifS ("n1" <: "n2") $
            Return (-1)
        , ifS ("n2" <: "n1") $
            Return 1
        , Return 0
        ]

    , FunSign "strcat" ["s1", "s2"] >: mconcat
        [ "n1" := FunE "strlen" ["s1"]
        , "n2" := FunE "strlen" ["s2"]
        , "s" := ArrayUninitE ("n1" + "n2")
        , forS ("i" := 0, "i" <: "n1", "i" := "i" + 1) $
            ArrayAssign "s" "i" (ArrayAccessE "s1" "i")
        , forS ("i" := 0, "i" <: "n2", "i" := "i" + 1) $
            ArrayAssign "s" ("n1" + "i") (ArrayAccessE "s2" "i")
        , Return "s"
        ]
    ]

