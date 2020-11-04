{-# LANGUAGE FlexibleContexts #-}

module Language.DifferentialDatalog.Type where

import Control.Monad.Except
import Language.DifferentialDatalog.Syntax
import Language.DifferentialDatalog.Pos
import Language.DifferentialDatalog.Var

data ConsTree

class WithType a where
    typ  :: a -> Type
    setType :: a -> Type -> a

instance WithType Relation where
instance WithType Type where
instance WithType Field where
instance WithType FuncArg where

typeStaticMemberTypes :: DatalogProgram' name -> Type -> [String]
typesMatch :: (WithType a, WithType b) => DatalogProgram' name -> a -> b -> Bool
checkTypesMatch :: (MonadError String me, WithType a, WithType b) => Pos -> DatalogProgram' name -> a -> b -> me ()
consTreeEmpty :: ConsTree -> Bool
consTreeAbduct :: DatalogProgram' name -> ConsTree -> Expr -> (ConsTree, ConsTree)
typeConsTree :: Type -> ConsTree
exprType :: DatalogProgram' name -> ECtx -> Expr -> Type
exprType' :: DatalogProgram' name -> ECtx -> Expr -> Type
exprType'' :: DatalogProgram' name -> ECtx -> Expr -> Type
sET_TYPES :: [String]
gROUP_TYPE :: String
ePOCH_TYPE :: String
iTERATION_TYPE :: String
nESTED_TS_TYPE :: String
wEIGHT_TYPE :: String
checkIterable :: (MonadError String me, WithType a) => String -> Pos -> DatalogProgram' name -> a -> me ()
typeIterType :: DatalogProgram' name -> Type -> (Type, Bool)
exprNodeType :: DatalogProgram' name -> ECtx -> ExprNode Type -> Type
isBool :: (WithType a) => DatalogProgram' name -> a -> Bool
isBit :: (WithType a) => DatalogProgram' name -> a -> Bool
isSigned :: (WithType a) => DatalogProgram' name -> a -> Bool
isString :: (WithType a) => DatalogProgram' name -> a -> Bool
isBigInt :: (WithType a) => DatalogProgram' name -> a -> Bool
isInteger :: (WithType a) => DatalogProgram' name -> a -> Bool
isMap :: (WithType a) => DatalogProgram' name -> a -> Bool
isGroup :: (WithType a) => DatalogProgram' name -> a -> Bool
isStruct :: (WithType a) => DatalogProgram' name -> a -> Bool
isSharedRef :: (WithType a) => DatalogProgram' name -> a -> Bool
isFloat :: (WithType a) => DatalogProgram' name -> a -> Bool
isDouble :: (WithType a) => DatalogProgram' name -> a -> Bool
isFP :: (WithType a) => DatalogProgram' name -> a -> Bool
typ' :: (WithType a) => DatalogProgram' name -> a -> Type
typeMapM :: (Monad m) => (Type -> m Type) -> Type -> m Type
typeIsPolymorphic :: Type -> Bool
varType :: DatalogProgram' name -> Var -> Type
