{-# Language OverloadedStrings, FlexibleInstances, BangPatterns #-}
module Main where

import Data.PathTrie as P

import Control.Monad
import Data.String
import Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map as M

newtype DotPath a = DotPath { unpath :: a }
                    deriving (Eq, Ord, Show)

instance IsString a => IsString (DotPath a) where
  fromString x = DotPath (fromString x)

instance Path (DotPath Text) where
  breakup (DotPath x) = L.map DotPath (T.splitOn "." x)
  merge xs = DotPath $ T.intercalate "." (L.map unpath xs)

type DPath = DotPath Text

test1 = do
  let e = P.empty :: PathTrie DPath Int

  let a = P.insert "a" 1 e

  let !(Nothing) = P.lookup "a" e
  let !(Just 1) = P.lookup "a" a

  let d = P.insert "a.b.c.d" 4 a

  let !(Just 4) = P.lookup "a.b.c.d" d

  putStrLn "TEST 1"


test2 = do
  putStrLn "TEST 2"

  let e = P.empty :: PathTrie DPath Int

  let a = P.insert "a.b.c.d.e" 1 e
  let b = P.insert "a.b" 2 a
  let g = P.insert "a.b.c.g" 3 b

  putStrLn "lookup"
  print $ ("a"         ,P.lookup "a" g)
  print $ ("a.b"       ,P.lookup "a.b" g)
  print $ ("a.b.c"     ,P.lookup "a.b.c" g)
  print $ ("a.b.c.d"   ,P.lookup "a.b.c.d" g)
  print $ ("a.b.c.d.e" ,P.lookup "a.b.c.d.e" g)
  print $ ("a.b.c.g"   ,P.lookup "a.b.c.g" g)
  print $ ("a.b.c.g.z" ,P.lookup "a.b.c.g.z" g)


  putStrLn "find"
  print $ ("a"         ,P.find "a" g)
  print $ ("a.b"       ,P.find "a.b" g)
  print $ ("a.b.c"     ,P.find "a.b.c" g)
  print $ ("a.b.c.d"   ,P.find "a.b.c.d" g)
  print $ ("a.b.c.d.e" ,P.find "a.b.c.d.e" g)
  print $ ("a.b.c.g"   ,P.find "a.b.c.g" g)
  print $ ("a.b.c.g.z" ,P.find "a.b.c.g.z" g)

  putStrLn "TEST 2 DONE"

printPT :: (Show a, Show k, Path k) => PathTrie k a -> IO ()
printPT t = go [] t
  where

    go :: (Show a, Show k, Path k) => [k] -> PathTrie k a -> IO ()

    go p (PathNode (Just v) cs)  = do
      print (merge (reverse p), v)
      forM_ (M.toList cs) $ \(k,v) -> go (k:p) v

    go p (PathNode (Nothing) cs) = do
      forM_ (M.toList cs) $ \(k,v) -> go (k:p) v

test3 = do
  putStrLn "TEST 3"

  let e = P.empty :: PathTrie DPath Int
  let l = [ ("a.b.c", 3)
          , ("a.b.c.d.e", 5)
          , ("z", 26)
          , ("z.a", 26*100 + 1)
          , ("a.0.2", 1002)
          ] :: [(DPath, Int)]
  let t = fromList l
  forM_ (L.map fst l) $ \k -> do
    print (k, P.lookup k t)

  putStrLn "---"

  forM_ (P.toList t) print

  putStrLn "TEST 3 DONE"



main = do
  test2
  test3


