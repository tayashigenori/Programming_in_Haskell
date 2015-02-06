-- 9.2. 入出力の型
--type IO a = World -> (a, World)

-- 9.3. 基本アクション
--getChar :: IO Char
--getChar = ...
getCh :: IO Char
getCh =  do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

--putChar :: Char -> IO ()
--putChar c = ...

--return   :: a -> IO a
--return v = \world -> (v, world)
_return   :: a -> IO a
_return v = \world -> (v, world)

-- 9.4. 順序付け
--(>>=) :: IO a -> (a -> IO b) -> IO b
--f >>= g = \world -> case f world of
--                     (v, world') -> g v world'
(>>>=) :: IO a -> (a -> IO b) -> IO b
f >>>= g = \world -> case f world of
                      (v, world') -> g v world'

--do does not work yet
echo :: IO ()
--echo = do c <- getChar
--        putChar '\n'
--		  putChar c
--		  putChar '\n'
--echo = getChar      >>>= \c ->
--       putChar '\n' >>>= \_ ->
--       putChar c    >>>= \_ ->
--       putChar '\n'

--getLine :: IO String
--getLine = do x <- getChar
--             if x == '\n' then
--               return []
--             else
--               do xs <- getLine
--               return (x: xs)

--putStr         :: String -> IO()
--putStr []      = _return()
--putStr (x: xs) = do putChar x
--                    putStr xs
--putStr (x: xs) = putChar x >>>= \_ ->
                 putStr xs

--putStrLn    :: String -> IO()
--putStrLn xs = do putStr xs
--                 putChar '\n'
--putStrLn xs = putStr xs    >>>= \_ ->
--              putChar '\n'

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
--writeat p xs = do goto p
--                  putStr xs
writeat p xs = goto p    >>>= \_ ->
               putStr xs

seqn         :: [IO a] -> IO()
seqn []      = _return ()
--seqn (a: as) = do a
--                  seqn as
seqn (a: as) = a       >>>= \_ ->
               seqn as

--putStr xs = seqn [putChar x | x <- xs]

