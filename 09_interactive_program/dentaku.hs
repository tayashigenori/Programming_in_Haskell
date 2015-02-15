import System.IO

-- 9.2. 入出力の型
--type IO a = World -> (a, World)

-- 9.3. 基本アクション
getCh :: IO Char
getCh =  do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

--putChar :: Char -> IO ()
--putChar c = ...

--return   :: a -> IO a
--return v = \world -> (v, world)

-- 9.4. 順序付け
--(>>=) :: IO a -> (a -> IO b) -> IO b
--f >>= g = \world -> case f world of
--                     (v, world') -> g v world'

--do does not work yet
--echo :: IO ()
--echo = do c <- getChar
--         putChar '\n'
--         putChar c
--         putChar '\n'

--getLine :: IO String
--getLine = do x <- getChar
--             if x == '\n' then
--               return []
--             else
--               do xs <- getLine
--                  return (x: xs)

--putStr         :: String -> IO()
--putStr []      = return()
--putStr (x: xs) = do putChar x
--                    putStr xs

--putStrLn    :: String -> IO()
--putStrLn xs = do putStr xs
--                 putChar '\n'

--strlen :: IO()
--stelen = do putStr "Enter a string: "
--            xs <- getLine
--            putStr "The string has "
--            putStr (show (length xs))
--            putStrLn " characters"

beep :: IO()
beep = putStr "\BEL"

cls :: IO()
cls = putStr "ESC[2J"

type Pos = (Int, Int)

goto        :: Pos -> IO()
goto (x, y) =
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs

seqn         :: [IO a] -> IO()
seqn []      = return ()
seqn (a: as) = do a
                  seqn as

--putStr xs = seqn [putChar x | x <- xs]


