import Data.List

g :: Int -> Int -> [Int] -> [Int]
g n m l = if (2^m > n)
            then []
            else let x = f n m l
                in if (null x)
                    then g n (m+1) l
                    else x

f :: Int -> Int -> [Int] -> [Int]
f n m []    = []
f n 1 l     = if n == head l
                then [n]
                else f n 1 (tail l)
f n m l     = if (n `mod` (head l)) == 0
                then
                    let x = f (n `div` (head l)) (m-1) l
                    in if (null x)
                        then f n m (tail l)
                        else ((head l) : x)
                else f n m (tail l)

listify :: String -> Int -> [Int] -> [Int]
listify "" n l  = l
listify str n l = if (null (drop n str))
                            then ((read str :: Int) : l)
                        else if head (drop n str) == ' '
                            then listify (drop (n+1) str) 0 ((read (take n str) :: Int) : l)
                        else listify str (n+1) l
                        
saysteps :: Int -> [Int] -> IO ()
saysteps i []   = putStr (show i)
saysteps i l    = do
                    putStr (show i)
                    putStr " "
                    saysteps (i*(head l)) (tail l)
                
check :: [Int] -> IO ()
check []    = print (-1)
check l     = saysteps 1 l

main = do
    input1 <- getLine
    let l1 = listify input1 0 []
    input2 <- getLine
    let l = listify input2 0 []
    let n = head (tail l1)
    check (g n 1 (sort l))
