module Main where
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.IORef

{-
Source -> Lex into Tokens -> Parse into AST -> Evaluate -> ... -> REPL

-- ABSTRACT SYNTAX TREE (AST)
-- Forest -> [Tree]
data Tree a = Empty | Node a [Tree a] deriving Show

ast :: Tree LispVal
ast = makeTree [Atom "+", Number 1, Number 2]

makeTree :: [LispVal] -> Tree LispVal
makeTree [] = Empty
makeTree (x:xs) = Node x $ 

-- Node + [ Number 1 [Empty] , Number 2 [Empty] ] 
-}

-- main :: IO ()
-- main = do getArgs >>= print . eval . readExpr . head 

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IO LispVal
getVar envRef var = do env <- readIORef envRef
                       maybe (undefined)
                             (readIORef)
                             (lookup var env)

-- readIORef :: IORef a -> IO a
-- flip writeIORef :: a -> IORef a -> IO ()
-- lookup :: a -> [(a,b)] -> Maybe b

{-
setVar :: Env -> String -> LispVal -> IO ()
setVar envRef var value = do env <- readIORef envRef
                             maybe (return ())
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
-}

setVar :: Env -> String -> LispVal -> IO LispVal
setVar envRef var value = do env <- readIORef envRef
                             maybe (return value)
                                   (const (return value) . (flip writeIORef value))
                                   (lookup var env)
                             return value


defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar envRef var value = do
    alreadyDefined  <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readOrThrow :: Parser a -> String -> IO a
readOrThrow parser input = case parse parser "lisp" input of
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

{-
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val 
-}

spaces :: Parser()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Float Float
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IO LispVal)
             | Port Handle

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
    "(lambda (" ++ unwords (map show args) ++ (case varargs of
                                                Nothing -> ""
                                                Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


unwordsList :: [LispVal] -> String
-- unwordsList [] = ""
-- unwordsList (x:xs) = showVal x ++ " " ++ unwordsList xs
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IO LispVal
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IO LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of 
           Bool False -> eval env alt
           Number 0 -> eval env alt
           otherwise -> eval env conseq
-- eval env (List [Atom "set!", Atom var, form]) = eval env form  >>= setVar env var
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "define", Atom var, form]) = eval env form  >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List (function : args) ) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
-- mapM :: (LispVal -> IO LispVal) -> [LispVal] -> IO [LispVal]

apply :: LispVal -> [LispVal] -> IO LispVal
apply (PrimitiveFunc func) args = return $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
      (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
-- apply "+" (map eval (Number 1, Number 2))
-- apply "+" [Number 1, Number 2]
-- numericBinop (+) [Number 1, Number 2]


ioPrimitives :: [(String, [LispVal] -> IO LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

applyProc :: [LispVal] -> IO LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IO LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IO LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IO LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftIO . readExpr

writeProc :: [LispVal] -> IO LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IO LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IO [LispVal]
load filename = (liftIO $ readFile filename) >>= liftIO . readExprList

readAll :: [LispVal] -> IO LispVal
readAll [String filename] = liftM List $ load filename

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

unpackNum :: LispVal -> Integer 
unpackNum (Number n) = n
-- unpackNum _ = 0

unpackStr :: LispVal -> String
unpackStr (String s) = s
unpackStr (Number s) = show s
unpackStr (Bool s) = show s

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b

unpackAtom :: LispVal -> String
unpackAtom (Atom s) = s

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


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- readExpr :: String -> IO LispVal
-- eval :: LispVal -> IO LispVal
evalString :: Env -> String -> IO String
-- evalString env expr = liftM show $ (eval env . readExpr) expr
evalString env expr = readExpr expr >>= eval env >>= return . show  
  

evalAndPrint :: Env -> String -> IO()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action 

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (liftIO $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "exit") (readPrompt "Lisp>> ") . evalAndPrint

main :: IO()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
