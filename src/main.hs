import Data.Char
import Data.List
import System.IO
import System.Environment

data Interp = Interp {
                        mem :: [Int],
                        pos :: Int,
                        ptr :: Int, 
                        stack :: [Int],
                        braces :: [(Int, Int)],
                        tokens :: String
                     } deriving (Show)

braceMap :: String -> Int -> [Int] -> [(Int, Int)]
braceMap [] _ _ = [] 
braceMap ('[':xs) n ys = braceMap xs (n+1) (n:ys) 
braceMap (']':xs) n ys = (head ys, n):braceMap xs (n+1) (drop 1 ys)
braceMap (x:xs) n ys = braceMap xs (n+1) ys

getRight :: [(Int, Int)] -> Int -> Int
getRight braces toMatch = foldl (\acc (x, y) -> if x == toMatch then y else acc) 0 braces

-- Set the memory at a given position with the given element. 
setMem :: Int -> Int -> [Int] -> [Int] 
setMem pos ele mem = (take pos mem) ++ [ele] ++ (drop (pos + 1) mem)

prev :: Interp -> Interp
prev bf = bf { ptr = (ptr bf) - 1 }

next :: Interp -> Interp
next bf = bf { ptr = (ptr bf) + 1 }

add :: Interp -> Interp
add bf = bf { mem = setMem (ptr bf) ((curMem + 1) `mod` 256) (mem bf)}
   where curMem = (mem bf) !! (ptr bf)

sub :: Interp -> Interp
sub bf = bf { mem = setMem (ptr bf) ((curMem - 1) `mod` 256) (mem bf)}
   where curMem = (mem bf) !! (ptr bf)

testWhile :: Interp -> Bool
testWhile bf = curMem /= 0 
    where curMem = (mem bf) !! (ptr bf)

initInterp :: String -> Interp
initInterp x = Interp { mem = [0,0..], pos = 0, ptr = 0, stack = [], braces = braceMap x 0 [], tokens = x}

isEnd :: Interp -> Bool
isEnd bf = (pos bf) >= length (tokens bf) 

push :: Int -> [Int] -> [Int]
push ele stack = ele:stack

-- Step :: Current Token -> Current State -> Modified State
step :: Interp -> IO Interp
step bf = case tok of '>' -> return (next bf)
                      '<' -> return (prev bf)
                      '+' -> return (add bf)
                      '-' -> return (sub bf)
                      '[' -> if testWhile bf then
                                return bf { stack = push (pos bf) (stack bf) }
                             else
                                return bf { pos = getRight (braces bf) (pos bf) }
                      ']' -> do 
                                return bf { pos = head (stack bf) - 1, stack = drop 1 (stack bf) }
                      '.' -> do 
                                putChar $ chr ((mem bf) !! (ptr bf))
                                return bf
                      ',' -> do
                                char <- getChar
                                return bf { mem = setMem (pos bf) (ord char) (mem bf)}
                      otherwise -> return bf
            where tok = (tokens bf) !! (pos bf)

execute :: Interp -> IO () 
execute bf = if isEnd bf then 
                    return ()
             else 
                do
                    bf' <- step bf
                    execute bf' { pos = (pos bf') + 1 }

-- Main funciton.
main = do
    args <- getArgs
    let fileName = head args
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle

    let bf = initInterp contents
    execute bf
    hClose handle
