{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}

module Problem03A where

import           Control.Applicative
import           Control.Exception   (evaluate)
import qualified System.CPUTime      as System


-- Main

data BinTree a
  = Node a (BinTree a) (BinTree a)
  | Leaf a
  deriving (Eq, Show, Functor)

genBinTree1
  :: Int
  -- ^ seed
  -> Int
  -- ^ height
  -> BinTree Int
genBinTree1 s h
    | h < 1     = error "expect positive number"
    | otherwise = go h s
  where
    go 1 x0 = Leaf x0
    go n x0 =
      let
        x1 = genElem1 x0
        x2 = genElem2 x0
      in Node x0 (go (n - 1) x1) (go (n - 1) x2)

    genElem1 x = (1103 * x + 4497) `mod` 2000001
    genElem2 x = (1107 * x + 3549) `mod` 2000001

genBinTree2
  :: Int
  -- ^ element
  -> Int
  -- ^ height
  -> BinTree Int
genBinTree2 x h
    | h < 1     = error "expect positive number"
    | otherwise = go h
  where
    go 1 = Leaf x
    go n = Node x (go $ n - 1) (go $ n - 1)

findRightOdd :: BinTree Int -> Maybe Int
findRightOdd = go
  where
    go (Leaf x)
      = maybeOdd x
    go (Node x lt rt)
      =   go rt
      <|> maybeOdd x
      <|> go lt

    maybeOdd x
      | x `mod` 2 == 1 = Just x
      | otherwise      = Nothing

bench :: Show b => (a -> b) -> a -> IO ()
bench f x = do
  t1 <- System.getCPUTime
  r <- evaluate $ f x
  t2 <- System.getCPUTime
  print r
  putStr "\t"
  print $ (t2 - t1) `div` 1000
  putStrLn ""

main :: IO ()
main = do
  let n = 27
  bench findRightOdd $ genBinTree1 0 n
  bench findRightOdd $ genBinTree2 0 n
  bench findRightOdd $ genBinTree2 1 n
