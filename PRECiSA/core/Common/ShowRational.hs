-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  

module Common.ShowRational where
import Data.List(elemIndex)
import Utils

-- | Convert a 'Rational' to a 'String' using the given number of decimals.
-- If the number of decimals is not given the full precision is showed.
-- E.g., 13.7/3 is shown as \"4.5(6)\".
showRational :: Maybe Int -> Rational -> String
showRational (Just n) r =
  let d = round (abs r * 10^n)
      s = show (d :: Integer)
      s' = replicate (n - length s + 1) '0' ++ s
      (h, f) = splitAt (length s' - n) s'
  in (if r < 0 then "-" else "") ++ h ++ "." ++ f

showRational Nothing n =
  let (i, f) = properFraction (abs n) :: (Integer, Rational)
      si = if n < 0 then "-" ++ show i else show i
      decimals g = loop g [] ""
      loop x fs ds = if x == 0 then ds
        else
          case elemIndex x fs of
            Just j  -> let (l, r) = splitAt j ds in l ++ "(" ++ r ++ ")"
            Nothing -> let (c, g) = properFraction (10 * x) :: (Integer, Rational) in loop g (fs ++ [x]) (ds ++ show c)
  in if f == 0 then si else si ++ "." ++ decimals f

showFloatC :: Rational -> String
showFloatC rat = if isInt rat then num ++ ".0" else num 
  where
    num = showRational Nothing rat