module Common.ShowRational where
import Data.List(elemIndex)

-- | Convert a 'Rational' to a 'String' using the given number of decimals.
-- If the number of decimals is not given the full precision is showed using (DDD) for repeating digits.
-- E.g., 13.7/3 is shown as \"4.5(6)\".
showRational :: Maybe Int -> Rational -> String
showRational (Just n) r =
    let d = round (abs r * 10^n)
        s = show (d :: Integer)
        s' = replicate (n - length s + 1) '0' ++ s
        (h, f) = splitAt (length s' - n) s'
    in  (if r < 0 then "-" else "") ++ h ++ "." ++ f
-- The length of the repeating digits is related to the totient function of the denominator.
-- This means that the complexity of computing them is at least as bad as factoring, i.e., it quickly becomes infeasible.
showRational Nothing n =
    let (i, f) = properFraction (abs n) :: (Integer, Rational)
        si = if n < 0 then "-" ++ show i else show i
        decimals g = loop g [] ""
        loop x fs ds =
            if x == 0 then
                ds
            else
                case elemIndex x fs of
                    Just j  -> let (l, r) = splitAt j ds in l ++ "(" ++ r ++ ")"
                    Nothing -> let (c, g) = properFraction (10 * x) :: (Integer, Rational) in loop g (fs ++ [x]) (ds ++ show c)
    in  if f == 0 then si else si ++ "." ++ decimals f