-- Notices:
--
-- Copyright 2017 United States Government as represented by the Administrator of the National Aeronautics and Space Administration.
-- All Rights Reserved.
--
-- Disclaimers:
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED,
-- IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT
-- SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS,
-- HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
--
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT,
-- ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING
-- FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH
-- MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

module Common.DecisionPath where

import Debug.Trace
import PPExt

class Eq p => DecisionPath p where
  root            :: p

  (~>)            :: p -> Bool -> p

  maxCommonPrefix :: p -> p -> p

  maxCommonPrefixOfList :: [p] -> p
  maxCommonPrefixOfList [] = root
  maxCommonPrefixOfList ds = foldr1 maxCommonPrefix ds

  isPrefix :: p -> p -> Bool
  isPrefix p1 p2 = (maxCommonPrefix p1 p2) == p1

  isPrefixInList :: p -> [p] -> Bool
  isPrefixInList d = any (isPrefix d)

  existsPrefixInList :: p -> [p] -> Bool
  existsPrefixInList d = any (flip isPrefix d)

instance DecisionPath LDecisionPath where
  root = LDP []

  (LDP dp) ~> d = LDP (d:dp)

  maxCommonPrefix (LDP ds1) (LDP ds2) = LDP $ reverse $ map fst $ takeWhile point2pointEqual $ zip revDs1 revDs2
    where revDs1 = reverse ds1
          revDs2 = reverse ds2

          point2pointEqual (d1,d2) = d1 == d2

newtype LDecisionPath = LDP [Bool] deriving (Eq,Ord)

instance Show LDecisionPath where
  show (LDP decisions) = show decisions

type TargetDPs = [(String, [LDecisionPath])]
