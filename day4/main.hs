module Main where

import Data.List.Split

allFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
reqFields = drop 1 $ reverse allFields

strInRange :: String -> Int -> Int -> Bool
strInRange str lo hi = (read str >= lo) && (read str <= hi)

digits = "1234567890"
hex = "abcdef"

allValidators :: [String -> Bool]
allValidators = [byr, iyr, eyr, hgt, hcl, ecl, pid, cid] where
  byr str = (length str == 4) && strInRange str 1920 2002
  iyr str = (length str == 4) && strInRange str 2010 2020
  eyr str = (length str == 4) && strInRange str 2020 2030
  hgt str
    | unit == "cm" = strInRange val 150 193
    | unit == "in" = strInRange val 59 76
    | otherwise = False
    where (val, unit) = span (`elem` digits) str
  hcl ('#':vals) = all (`elem` (digits ++ hex)) vals
  hcl _ = False
  ecl = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  pid str = (length str == 9) && all (`elem` digits) str
  cid = const True

validField :: String -> String -> Bool
validField field val = any (f field val) $ zip allFields allValidators where
  f field val (fieldf, valf) = (field == fieldf) && valf val

toPassports :: String -> [String]
toPassports str = map unwords $ splitOn [""] $ lines str

validPassport1 :: String -> Bool
validPassport1 str = all (`elem` fields) reqFields where
  entries = splitOn " " str
  splitEntries = map (splitOn ":") entries
  fields = map head splitEntries

validPassport2 :: String -> Bool
validPassport2 str = all (`elem` fields) reqFields && and (zipWith validField fields vals)  where
  entries = splitOn " " str
  splitEntries = map (splitOn ":") entries
  fields = map head splitEntries
  vals = map (!!1) splitEntries

main :: IO ()
main = do
  part1 <- (length <$> filter validPassport1) . toPassports <$> readFile "input"
  part2 <- (length <$> filter validPassport2) . toPassports <$> readFile "input"
  putStr "Part 1:"
  print part1
  putStr "Part 2:"
  print part2
