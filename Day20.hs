module Main where
import Utils hiding (at)
import Types
import qualified Utils as U

type Image = Grid Bool

at :: Image -> Point -> Bool
at img p = 

enhance :: [Bool] -> Image -> Image
enhance alg img = _a

part1 :: String -> String
part1 = evaluate ((\(c:[]:img) -> (c, G img)) . map (map toBool) . lines) (countTrue . foldr (\_ d -> uncurry enhance d) [1, 2])
  where
    toBool '#' = True
    toBool '.' = False
    toBool _ = undefined

main :: IO ()
main = interact part1
