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

module Utils where

import qualified Data.Set as Set

setEq :: Ord a => [a] -> [a] -> Bool
setEq x y = Set.fromList x == Set.fromList y

combos :: [[t]] -> [[t]]
-- returns all the possible combination for a lists of lists
-- (ex combos [[1,2,3],[4,5]] = [1,4],[1,5],[2,4],[2,5],[3,4],[3,5])
combos [] = [[]]
combos ([]:_) = []
combos ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)

foldlWithDefault :: (a -> a -> a) -> a -> [a] -> a
foldlWithDefault _ base [] = base
foldlWithDefault f _ list = foldl1 f list

isPow2 :: (Floating r, RealFrac r) => r -> Bool
isPow2 n = (logBase 2 n == fromInteger (round $ logBase 2 n)) && (logBase 2 n >= 0)

elimDuplicates :: (Ord a) => [a] -> [a]
elimDuplicates = Set.toList . Set.fromList

lookup3 :: (Show t, Eq t) => t -> [(t, a, b)] -> (a, b)
lookup3 a [] = error $ "lookup3: element " ++ show a ++ "not found."
lookup3 a ((b,c,d):rest) | a == b = (c,d)
                         | otherwise = lookup3 a rest

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,a,_) = a

pairCombinations :: (Num b, Enum b, Eq b) => b -> [(b, b)]
pairCombinations m = [ (a,b) | a <- [0..(m-1)], b <- [0..(m-1)], a /= b]