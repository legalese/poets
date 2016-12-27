
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Simplify.FieldConstraints
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr, Michael Werk
-- Stability   :  unknown
-- Portability :  unknown
--
-- This is a utility modules to facilitate specific calculations 
-- on atomic field constraints. 
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Simplify.FieldConstraints
       ( AFieldConstraint
       , splitFieldConstraints
       , fromFieldConstraint
       , fieldRecordVariables
       ) where

import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicTypes 
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicDecomp 

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

{-| Representation of a atomic field constraint -} 

type AFieldConstraint = (AtomicPType, String, PType, SrcPos)
      
{-| This function splits the given list of atimoc constraints into a list of
(atomic) field constraints, and a list of the remaining
constraints. -}

splitFieldConstraints :: [AConstraint a] -> ([AFieldConstraint], [AConstraint a])
splitFieldConstraints [] = ([],[])
splitFieldConstraints (c:cs) =
  let (fcs, others) = splitFieldConstraints cs
  in case c of 
    AHasField r n t :&: pos -> ((r, n, t, pos):fcs, others)
    _ -> (fcs, c:others)
    
{-| Convert an atomic field constraint back to a atomic constraint -}
    
fromFieldConstraint :: AFieldConstraint -> AConstraint a
fromFieldConstraint (r, n, t, pos) = AHasField r n t :&: pos


{-| Finds the type variables in the left-hand position of the field 
constraints. -}

fieldRecordVariables :: [AFieldConstraint] -> Set TVarId
fieldRecordVariables cs = Set.fromList ts
  where
    recordVariable (r, _, _, _) = atomicVar r
    ts = catMaybes $ map recordVariable cs
