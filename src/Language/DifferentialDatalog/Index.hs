{-
Copyright (c) 2019 VMware, Inc.
SPDX-License-Identifier: MIT

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

{-# LANGUAGE TupleSections, LambdaCase, RecordWildCards #-}

{- | 
Module     : Index
Description: Helper functions for manipulating Indexes.
-}
module Language.DifferentialDatalog.Index (
    idxIdentifier,
    idxRelation,
    idxRelation_,
    idxKeyType
) 
where

import qualified Data.Map as M

import Language.DifferentialDatalog.Name
import Language.DifferentialDatalog.Syntax
import Language.DifferentialDatalog.NS
import Language.DifferentialDatalog.Type

-- | Unique id, assigned to the index
idxIdentifier :: DatalogProgram' name -> Index -> Int
idxIdentifier d idx = M.findIndex (name idx) $ progIndexes d

idxRelation :: DatalogProgram' name -> Index -> Relation
idxRelation d idx = getRelation d $ atomRelation $ idxAtom idx

idxRelation_ :: DatalogProgram -> Index -> Relation
idxRelation_ = idxRelation

idxKeyType :: Index -> Type
idxKeyType = tTuple . map typ . idxVars
