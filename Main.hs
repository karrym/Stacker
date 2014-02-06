
{-# LANGUAGE PackageImports , UnicodeSyntax #-} 

module Main where

import Control.Monad.Trans.State
import Control.Applicative
import Control.Monad.IO.Class
import System.IO
import Data.List
import Nat

data Val = Int { fromInt :: Int }
         | Bool { fromBool :: Bool}
         deriving (Eq, Ord)

instance Show Val where
        show (Int n) = show n
        show (Bool n) = show n

type Stack = [Val]

-- push to stack
push :: Monad m => a → StateT [a] m ()
push x = do
        xs <- get
        put (x:xs)

-- pop from stack
pop :: Monad m => StateT [a] m a
pop = do
        (x:xs) <- get
        put xs
        return x

-- push list
pushs :: Monad m => [a] → StateT [a] m ()
pushs []     = return ()
pushs (x:xs) = pushs xs >> push x

-- pop some value
pops :: (Functor m, Monad m) => Nat → StateT [a] m [a]
pops Z     = return []
pops (S n) = (:) <$> pop <*> pops n

-- add two nums of stack
adds :: (Functor m, Monad m) => StateT Stack m Val
adds = add <$> pop <*> pop where
    add (Int n) (Int m) = Int $ m + n

-- min two nums of stack
mins :: (Functor m, Monad m) => StateT Stack m Val
mins = min <$> pop <*> pop where
    min (Int n) (Int m) = Int $ m - n

-- mul two nums of stack
muls :: (Functor m, Monad m) => StateT Stack m Val
muls = mul <$> pop <*> pop where
    mul (Int n) (Int m) = Int $ m * n

-- div two nums of stack
divs :: (Functor m, Monad m) => StateT Stack m Val
divs = div' <$> pop <*> pop where
    div' (Int n) (Int m) = Int $ m `div` n

-- mod two nums of stack
mods :: (Functor m, Monad m) => StateT Stack m Val
mods =  mod' <$> pop <*> pop where
    mod' (Int n) (Int m) = Int $ m `mod` n

-- exp two nums of stack
exps :: (Functor m, Monad m) => StateT Stack m Val
exps = exp <$> pop <*> pop where
    exp (Int n) (Int m) = Int $ m ^ n

-- if two nums are equal, return True
equal :: (Functor m, Monad m) => StateT Stack m Val
equal = (\x y → Bool $ fromInt y == fromInt x) <$> pop <*> pop

-- if top of stack is less than second of stack, return True
less :: (Functor m, Monad m) => StateT Stack m Val
less = (\x y → Bool $ fromInt y < fromInt x) <$> pop <*> pop

-- if top of stack is greater than second of stack, return True
great :: (Functor m, Monad m) => StateT Stack m Val
great = (\x y → Bool $ fromInt y > fromInt x) <$> pop <*> pop

-- or two boolean of stack
ors :: (Functor m, Monad m) => StateT Stack m Val
ors = (\x y → Bool $ fromBool x || fromBool y) <$> pop <*> pop

-- and two boolean of stack
ands :: (Functor m, Monad m) => StateT Stack m Val
ands = (\x y → Bool $ fromBool x && fromBool y) <$> pop <*> pop

-- not a boolean of stack
nots :: (Functor m, Monad m) => StateT Stack m Val
nots = (Bool . not . fromBool) <$> pop where

-- less or equal
lesse :: (Functor m, Monad m) => StateT Stack m Val
lesse = (\x y → Bool $ fromInt y <= fromInt x) <$> pop <*> pop

-- great or equal
greate :: (Functor m, Monad m) => StateT Stack m Val
greate = (\x y → Bool $ fromInt y >= fromInt x) <$> pop <*> pop

-- swap two values of stack
swap :: (Functor m, Monad m) => StateT [a] m ()
swap = pops 2 >>= pushs . reverse

-- sum all values of stack
sums :: (Functor m, Monad m) => StateT Stack m Val
sums = (Int . sum . map fromInt) <$> get

-- product all values of stack
products :: (Functor m, Monad m) => StateT Stack m Val
products = (Int . product . map fromInt) <$> get

-- evalulate input code
eval :: (Functor m, Monad m) => String → StateT Stack m ()
eval "+"    = adds >>= push
eval "-"    = mins >>= push
eval "*"    = muls >>= push
eval "/"    = divs >>= push
eval "%"    = mods >>= push
eval "^"    = exps >>= push
eval "="    = equal >>= push
eval "<"    = less >>= push
eval ">"    = great >>= push
eval "<="   = lesse >>= push
eval ">="   = greate >>= push
eval "not"  = nots >>= push
eval "&&"   = ands >>= push
eval "||"   = ors >>= push
eval "swap" = swap
eval "pop"  = pop >> return ()
eval "sum" = sums >>= put . (:[])
eval "product" = products >>= put . (:[])
eval "sum'"  = sums >>= push
eval "product'" = products >>= push
eval "True" = push . Bool $ True
eval "False" = push . Bool $ False
eval ns     = push . Int . read $ ns

-- evalulate input code
repl :: (Functor m, Monad m) => String → StateT Stack m ()
repl = foldr (\x y -> eval x >> y) (return ()) . words

-- second argument and first argument apply to third argument while first argument return true
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
        let list = zipWith4 (\x y w z → x ++ y ++ w ++ z) (repeat "[") (map show $ iterate succ 1) (repeat "]  ") (map show stack)
        liftIO $ mapM_ putStrLn list

calculator :: StateT Stack IO ()        
calculator = while (/= "quit") (readCodeT ">> ") runRepl

flushStr :: String → IO ()
flushStr str = putStr str >> hFlush stdout

readCode :: String → IO String
readCode str = flushStr str >> getLine

readCodeT :: MonadIO m => String → m String
readCodeT = liftIO . readCode

main :: IO ()
main = runStateT calculator [] >> return ()
