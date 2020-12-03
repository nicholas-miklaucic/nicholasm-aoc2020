import System.IO

multSum2020 :: [Int] -> Int
multSum2020 nums = let pairs = [x * y * z | x <- nums, y <- nums, z <- nums, x + y + z == 2020] in
  head $ pairs

main :: IO ()
main = do
  withFile "input.txt" ReadMode (
    \handle -> do
      contents <- hGetContents handle
      putStrLn $ show $ multSum2020 $ fmap read (lines contents)
    )
