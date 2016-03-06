import Data.Bool
import Test.QuickCheck

type Binary = [Bool]

binaryToInt :: Binary -> Int
binaryToInt [] = 0
binaryToInt (x : xs) = bool 0 1 x + 2 * binaryToInt xs

increment :: Binary -> Binary
increment x = case x of
  [] -> [True]
  (y : ys) -> not y : bool id increment y ys

prop_increment :: Binary -> Property
prop_increment x = binaryToInt x + 1 === binaryToInt (increment x) 

prop_addition :: Binary -> Binary -> Property
prop_addition x y = binaryToInt (addBinary x y) === (binaryToInt x + binaryToInt y)

addBinary :: Binary -> Binary -> Binary
addBinary [] y = y
addBinary x [] = x
addBinary (x : xs) (y : ys) =
  case (x, y) of
    (False, False) -> False : addBinary xs ys
    (False, True) -> True : addBinary xs ys
    (True, False) -> True : addBinary xs ys
    (True, True) -> False : addBinary (increment xs) ys 
