{-# LANGUAGE ImplicitParams #-}

module Language.DifferentialDatalog.Compile where

import Text.PrettyPrint

import Language.DifferentialDatalog.Syntax
import {-# SOURCE #-} Language.DifferentialDatalog.Type

data Scope = Local | Types String

mkConstructorName :: Scope -> String -> Type -> String -> Doc
mkType :: (WithType a) => DatalogProgram -> Bool -> a -> Doc
rnameFlat :: String -> Doc
rnameScoped :: Scope -> String -> Doc
progTypes :: DatalogProgram -> Scope

tupleStruct :: Scope -> [Doc] -> Doc

recordAfterPrefix :: DatalogProgram' name -> Rule -> Int -> [Expr]
