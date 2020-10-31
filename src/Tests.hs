module Tests where
import Test.QuickCheck

propList :: [()] -> Property
propList xs =
  not (null xs) ==>
  (length $ tail xs) === ((length xs)-1)
  -- === shows the data

testPropList = quickCheck (verbose propList)

rev xs = revAux [] xs
  where 
    revAux acc [] = acc
    revAux acc (x:xs) = revAux (x:acc) xs

propRev :: [()] -> Property
-- propRev xs = collect (length xs) $ reverse xs === rev xs
-- collect: shows percentage based on collected info
propRev xs = classify (length xs == 0) "empty" $ reverse xs === rev xs
-- classify shows percentage of classified cases

testPropRev = quickCheck (withMaxSuccess 10000 propRev)

propLookup k v m = lookup k ((k, v):m) === Just v
  where types = (k :: Int, v :: Int)

testPropLookup = quickCheck (verbose propLookup)