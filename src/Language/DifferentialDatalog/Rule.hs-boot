module Language.DifferentialDatalog.Rule where

import Language.DifferentialDatalog.Syntax
import Language.DifferentialDatalog.Var

ruleRHSVars :: DatalogProgram' name -> Rule -> Int -> [Var]
ruleVars :: DatalogProgram' name -> Rule -> [Var]
ruleRHSTermVars :: DatalogProgram' name -> Rule -> Int -> [Var]
ruleLHSVars :: DatalogProgram' name -> Rule -> [Var]
ruleTypeMapM :: (Monad m) => (Type -> m Type) -> Rule -> m Rule
ruleHasJoins :: Rule -> Bool
ruleIsDistinctByConstruction :: DatalogProgram -> Rule -> Int -> Bool
ruleHeadIsRecursive :: DatalogProgram' name -> Rule -> Int -> Bool
ruleIsRecursive :: DatalogProgram' name -> Rule -> Bool
ruleGroupByKeyType :: DatalogProgram' name -> Rule -> Int -> Type
ruleGroupByValType :: DatalogProgram' name -> Rule -> Int -> Type
