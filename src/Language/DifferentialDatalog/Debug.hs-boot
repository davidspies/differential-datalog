module Language.DifferentialDatalog.Debug where

import Language.DifferentialDatalog.Syntax

debugUpdateRHSRules :: DatalogProgram' name -> Int -> Rule -> [RuleRHS]
