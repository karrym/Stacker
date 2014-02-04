{-# LANGUAGE PackageImports , UnicodeSyntax #-} 

module Main where

import Control.Monad.Trans.State
import Control.Applicative
import Control.Monad.IO.Class

type Stack = [Int]

-- push to stack
push :: Int → StateT Stack IO ()
push x = do
        xs <- get
        put (x:xs)

-- pop from stack
pop :: StateT Stack IO Int
pop = do
        (x:xs) <- get
        put xs
        return x

-- add two nums of stack
adds :: StateT Stack IO Int
adds = (+) <$> pop <*> pop

-- min two nums of stack
mins :: StateT Stack IO Int
mins = flip (-) <$> pop <*> pop

-- mul two nums of stack
muls :: StateT Stack IO Int
muls = (*) <$> pop <*> pop

-- div two nums of stack
divs :: StateT Stack IO Int
divs = flip div <$> pop <*> pop

-- evalulate input code
eval :: String → StateT Stack IO ()
eval "+" = adds >>= push
eval "-" = mins >>= push
eval "*" = muls >>= push
eval "/" = divs >>= push
eval ns  = push . read $ ns

-- evalulate input code
repl :: String → StateT Stack IO ()
repl = foldr (\x y -> eval x >> y) (return ()) . words

while :: Monad m => (a → Bool) → m a → (a → m ()) → m ()
while p x f = do
        code <- x
        if p code
            then f code >> while p x f
            else return ()


-- evalute and print stack
runRepl :: String → StateT Stack IO () 
runRepl code = do
        repl code
        stack ←  get
        liftIO $ mapM_ print stack

calculator :: StateT Stack IO ()        
calculator = while (/= "quit") (putStrT ">> " >> getLineT) runRepl

putStrT :: MonadIO m => String → m ()
putStrT = liftIO . putStr
getLineT :: MonadIO m => m String
getLineT = liftIO getLine

main :: IO ()
main = runStateT calculator [] >> return ()

