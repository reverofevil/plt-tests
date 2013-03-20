{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding (lex)
import Data.Char
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Applicative
import qualified Data.Map as M

type V = Char
type Prog = [Row]
data Row = EEq V Expr | EInp V | EOut V
data Expr = EInt Integer | EAdd Expr Expr | EVar V
type Value = Integer
type Context = M.Map V Value
data ParseState = ParseState { program :: String, context :: Context }
type Parser a = StateT ParseState (ErrorT Exc IO) a
data Exc = XUnexpected | XExpect [String] | XNoSuchVar V
	deriving (Show)
instance Error Exc where
	noMsg = undefined
	strMsg _ = undefined

runParser :: Parser a -> String -> IO (Either Exc a)
runParser p s = runErrorT $ evalStateT p $ ParseState s M.empty

get_input :: Parser String
get_input = program <$> get

put_input :: String -> Parser ()
put_input s = modify $ \c -> c { program = s }

read_var :: V -> Parser Value
read_var v = do
	c <- get
	case M.lookup v (context c) of
		Just r -> return r
		Nothing -> throwError $ XNoSuchVar v

set_var :: V -> Value -> Parser ()
set_var v r = modify $ \c -> c { context = M.insert v r $ context c }

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
	s <- get_input
	if null s then
		throwError XUnexpected
	else do
		let (c : cs) = s
		put_input cs
		if f c then
			return c
		else
			throwError XUnexpected

char :: Char -> Parser Char
char c = do
	satisfy (== c) `catchError` \e -> do
		case e of
			XUnexpected -> throwError $ XExpect [show c]
			_           -> throwError e

range :: Char -> Char -> Parser Char
range from to = do
	let inRange c = from <= c && c <= to
	satisfy inRange `catchError` \e -> do
		let showRange = show from ++ ".." ++ show to
		case e of
			XUnexpected -> throwError $ XExpect [showRange]
			_           -> throwError e

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isLetter

space :: Parser Char
space = satisfy isSpace

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 x f = action <$> x <*> many ((,) <$> f <*> x) where
	action x ps = foldl (\a (f, b) -> f a b) x ps

eof :: Parser ()
eof = do
	s <- get_input
	if not $ null s then
		throwError $ XExpect ["end of input"]
	else return ()

prog    = spaces0 *> many row <* eof
row     = (equat <|> input <|> output) <* lex ';'
equat   = EEq <$> vari <*> (lex '=' *> expr0)
input   = EInp <$> (lex '<' *> vari)
output  = EOut <$> (lex '>' *> vari)
expr0   = chainl1 expr1 plus
plus    = EAdd <$ lex '+'
expr1   = intlit <|> var <|> brace
brace   = lex '(' *> expr0 <* lex ')'
intlit  = EInt . read . return <$> digit <* spaces0
var     = EVar <$> vari
vari    = letter <* spaces0
lex c   = char c <* spaces0
spaces0 = many space

evalExpr :: Expr -> Parser Value
evalExpr (EInt x) = return x
evalExpr (EVar v) = read_var v
evalExpr (EAdd l r) = (+) <$> evalExpr l <*> evalExpr r
	
evalRow :: Row -> Parser ()
evalRow (EEq v e) = do
	r <- evalExpr e
	set_var v r
evalRow (EInp v) = do
	liftIO $ putStr $ v : "? "
	s <- liftIO getLine
	set_var v $ (read s :: Integer)
evalRow (EOut v) = do
	r <- read_var v
	liftIO $ print r

evalProg :: Prog -> Parser ()
evalProg = mapM_ evalRow

main :: IO ()
main = do
	let test = "a = 2; b = 2; c = a + b; > c;"
	let comp = prog >>= evalProg
	p <- runParser comp test
	case p of
		Right () -> return ()
		Left e -> print e