module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

{-
-- ABSTRACT SYNTAX TREE 
data Tree a = Empty | Node a [Tree a] deriving Show

ast :: Tree LispVal
ast = makeTree [Atom "+", Number 1, Number 2]

makeTree :: [LispVal] -> Tree LispVal
makeTree [] = Empty
makeTree (x:xs) = Node x $ 

-- Node + [ Number 1 [Empty] , Number 2 [Empty] ] 
-}

main :: IO ()
main = do getArgs >>= print . eval . readExpr . head 

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val 

spaces :: Parser()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Float Float
             | Bool Bool

parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        otherwise -> Atom atom

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = many1 digit >>= return . Number . read
parseNumber = do 
    x <- many1 digit
    return $ Number $ read x

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float $ read $ x++['.']++y

parseList :: Parser LispVal
-- parseList = liftM List $ sepBy parseExpr spaces
parseList = do
    x <- sepBy parseExpr spaces
    return $ List x

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

showVal :: LispVal -> String
showVal (String x) = "\"" ++ x ++ "\""
showVal (Atom x) = x
showVal (Number x) = show x
showVal (Float x) = show x
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List x) = "(" ++ unwordsList x ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
-- unwordsList [] = ""
-- unwordsList (x:xs) = showVal x ++ " " ++ unwordsList xs
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List [Atom "if", pred, conseq, alt]) = case (eval pred) of
                                               Bool False -> eval alt
                                               Number 0 -> eval alt
                                               otherwise -> eval conseq
eval (List (Atom func : args) ) = apply func $ map eval args


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($args) $ lookup func primitives
-- apply "+" (map eval (Number 1, Number 2))
-- apply "+" [Number 1, Number 2]
-- numericBinop (+) [Number 1, Number 2]

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", isString),
              ("symbol?", isSymbol),
              ("number?", isNumber),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=",numBoolBinop (>=)),
              ("<=",numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("xor", boolBoolBinop (/=)),
              ("nand", boolBoolBinop nand),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv)]

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

isNumber :: [LispVal] -> LispVal
isNumber (Number x:xs) = Bool True
isNumber _ = Bool False

isString :: [LispVal] -> LispVal
isString (String x:xs) = Bool True
isString _ = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol (Atom x:xs) = Bool True
isSymbol _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
-- numericBinop (+) [Number 1, Number 2]
-- Number $ foldl1 (+) $ map unpackNum [Number 1, Number 2]
-- Number $ foldl1 (+) $ [1, 2]
-- Number $ 3


boolBinop :: (LispVal -> a) -> (a->a->Bool) -> [LispVal] -> LispVal
boolBinop unpacker op (x:y:_) = Bool $ op (unpacker x) (unpacker y)

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

-- numBoolBinop (==) [Number 1, Number 2]
-- a <- unpackNum Number 1
-- b <- unpackNum Number 2
-- return $ Bool $ (==) 1 2
-- return  $ Bool False

{-boolBoolBinop :: (a->a->Bool) -> [LispVal] -> LispVal
boolBoolBinop op params = Bool $ op (params!!0) (params!!1)

strBoolBinop :: (a->a->Bool) -> [LispVal] -> LispVal
strBoolBinop op params = Bool $ op (params!!0) (params!!1)
-}

unpackNum :: LispVal -> Integer 
unpackNum (Number n) = n
unpackNum _ = 0

unpackStr :: LispVal -> String
unpackStr (String s) = s
unpackStr (Number s) = show s
unpackStr (Bool s) = show s

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b

car :: [LispVal] -> LispVal
car [List (x: xs)] = x
car [DottedList (x : xs) _] = x

cdr :: [LispVal] -> LispVal
cdr [List (x: xs)] =  List xs
cdr [DottedList [_] x] = x
cdr [DottedList (_: xs) x] = DottedList xs x

cons :: [LispVal] -> LispVal
cons [x1, List[]] = List [x1]
cons [x, List xs] = List $ x : xs
cons [x, DottedList xs xlast] = DottedList (x:xs) xlast
cons [x1, x2] = DottedList [x1] x2

-- weak typing
eqv :: [LispVal] -> LispVal
eqv[(Bool a), (Bool b)] = Bool $ a == b
eqv[(Number a), (Number b)] = Bool $ a == b
eqv[(String a), (String b)] = Bool $ a == b
eqv[(Atom a), (Atom b)] = Bool $ a == b
eqv[(List a), (List b)] = Bool $ (length a == length b) && (all eqvPair $ zip a b)
    where eqvPair (x1, x2) = unpackBool $ eqv [x1,x2]
