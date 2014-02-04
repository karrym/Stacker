
{-# LANGUAGE PackageImports , UnicodeSyntax #-} 

module Main where

import Control.Monad.Trans.State
import Control.Applicative
import Control.Monad.IO.Class
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

pushs :: Monad m => [a] → StateT [a] m ()
pushs []     = return ()
pushs (x:xs) = pushs xs >> push x

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

mods :: (Functor m, Monad m) => StateT Stack m Val
mods =  mod' <$> pop <*> pop where
    mod' (Int n) (Int m) = Int $ m `mod` n

exps :: (Functor m, Monad m) => StateT Stack m Val
exps = exp <$> pop <*> pop where
    exp (Int n) (Int m) = Int $ m ^ n

equal :: (Functor m, Monad m) => StateT Stack m Val
equal = (\x y → Bool $ fromInt y == fromInt x) <$> pop <*> pop

less :: (Functor m, Monad m) => StateT Stack m Val
less = (\x y → Bool $ fromInt y < fromInt x) <$> pop <*> pop

great :: (Functor m, Monad m) => StateT Stack m Val
great = (\x y → Bool $ fromInt y > fromInt x) <$> pop <*> pop

ors :: (Functor m, Monad m) => StateT Stack m Val
ors = (\x y → Bool $ fromBool x || fromBool y) <$> pop <*> pop

ands :: (Functor m, Monad m) => StateT Stack m Val
ands = (\x y → Bool $ fromBool x && fromBool y) <$> pop <*> pop

nots :: (Functor m, Monad m) => StateT Stack m Val
nots = (Bool . not . fromBool) <$> pop where

lesse :: (Functor m, Monad m) => StateT Stack m Val
lesse = (\x y → Bool $ fromInt y <= fromInt x) <$> pop <*> pop

greate :: (Functor m, Monad m) => StateT Stack m Val
greate = (\x y → Bool $ fromInt y >= fromInt x) <$> pop <*> pop

-- swap two values of stack
swap :: (Functor m, Monad m) => StateT [a] m ()
swap = pops 2 >>= pushs . reverse

sums :: (Functor m, Monad m) => StateT Stack m Val
sums = (Int . sum . map fromInt) <$> get

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
