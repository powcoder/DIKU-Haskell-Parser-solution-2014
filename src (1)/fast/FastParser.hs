https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module FastParser
       ( Error
       , parseString
       , parseFile
       )
       where

import FastAST
import Data.Char
import Text.ParserCombinators.ReadP

keywords :: [String]
keywords = ["class","match","new","receive","return","self","send","set"]

-- | You may change this type to whatever you want - just make sure it
-- is an instance of 'Show'.
type Error = String

-- CONVENIENCE
ws = munch (\c -> c `elem` " \t\n")
ws1 = munch1 (\c -> c `elem` " \t\n")

isKeyword :: String -> Bool
isKeyword s = s `elem` keywords

leftParen = chrToken '('
rightParen = chrToken ')'

parens :: ReadP p -> ReadP p
parens p = between leftParen rightParen p

leftBrace = chrToken '{'
rightBrace = chrToken '}'

braces :: ReadP p -> ReadP p
braces p = between leftBrace rightBrace p

-- parses char tokens
chrToken :: Char -> ReadP ()
chrToken c = do skipSpaces; char c; skipSpaces

-- parses string tokens
strToken :: String -> ReadP ()
strToken s = do skipSpaces; string s; skipSpaces

-- parses digits
digits :: ReadP String
digits = munch1 isDigit

-- parses the sign
sign :: ReadP Integer
sign = (do
        chrToken '-'
        return $ -1)
    +++ return 1

-- parses a name
parseName :: ReadP Name
parseName = do
    f <- satisfy isLetter -- first must be a letter
    r <- munch (\c -> isAlphaNum c || c `elem` "_") -- rest
    return $ (f:r)

-- EXPRESSIONS
parseInt :: ReadP Integer
parseInt = do
    s <- sign
    d <- digits
    return $ (s * (read d))

parseStr :: ReadP String
parseStr = do
    char '"'
    s <- munch (\c -> c /= '"')
    char '"'
    return $ s

parseParams :: ReadP [Name]
parseParams = parens (sepBy parseName (chrToken ','))

parseArgs :: ReadP Exprs
parseArgs = parens (sepBy parseExpr (chrToken ','))

parseExprs :: ReadP Exprs
parseExprs =
    do {
        leftBrace;
        es <- (sepBy parseExpr (chrToken ';'));
        chrToken ';';
        rightBrace;
        return es
    } +++
    do {
        between leftBrace rightBrace ws;
        return []
    }

parseExpr :: ReadP Expr
parseExpr = parseTerm -- TODO: allow nesting

parseTerm :: ReadP Expr
parseTerm =
    parens parseExpr +++
    parseNew +++
    parseSend +++
    parseMatch +++
    arithmetic +++
    parseSetVar +++
    parseSetField
    where arithmetic = chainl1 e0 op0
          e0 = chainl1 e1 op0
          e1 = chainl1 e2 op1
          e2 = chainl1 e3 op2
          e3 = chainl1 parseLiteral op3
          op0 = do { chrToken '+'; return Plus }
          op1 = do { chrToken '-'; return Minus }
          op2 = do { chrToken '*'; return Times }
          op3 = do { chrToken '/'; return DividedBy }

parseLiteral :: ReadP Expr
parseLiteral =
    do { strToken "self"; return $ Self } +++
    do { (n, es) <- parseCall; return $ TermLiteral n es } +++
    do { n <- parseName; return $ ReadVar n } +++
    do { s <- parseStr; return $ StringConst s } +++
    do { n <- parseInt; return $ IntConst n }

parseMatch :: ReadP Expr
parseMatch = do
    strToken "match"
    e <- parseExpr
    leftBrace
    cs <- many1 parseCase
    rightBrace
    return $ Match e cs

parseSend :: ReadP Expr
parseSend = do
    strToken "send"
    leftParen
    r <- parseExpr
    chrToken ','
    m <- parseExpr
    rightParen
    return $ SendMessage r m

parseNew :: ReadP Expr
parseNew = do
    strToken "new"
    (n, es) <- parseCall
    return $ New n es

parseSetVar :: ReadP Expr
parseSetVar = do
    strToken "set"
    n <- parseName
    chrToken '='
    e <- parseExpr
    return $ SetVar n e

parseSetField :: ReadP Expr
parseSetField = do
    strToken "set"
    strToken "self."
    n <- parseName
    chrToken '='
    e <- parseExpr
    return $ SetField n e

parseCall :: ReadP (Name, [Expr])
parseCall = do
    n <- parseName
    ws;
    args <- parseArgs
    ws;
    return (n, args)

parseReturn :: ReadP Expr
parseReturn = do
    strToken "return"
    e <- parseExpr
    return $ Return e

-- PATTERNS
parsePattern :: ReadP Pattern
parsePattern =
    do { n <- parseName; return $ AnyValue n } +++
    do { n <- parseName; ps <- parseParams; return $ TermPattern n ps } +++
    do { s <- parseStr; return $ ConstString s } +++
    do { n <- parseInt; return $ ConstInt n }

parseCase :: ReadP Case
parseCase = do
    p <- parsePattern
    strToken "->"
    es <- parseExprs
    return $ (p, es)

-- CLASSES
parseConstructor :: ReadP (Maybe ConstructorDecl)
parseConstructor = do
    string "new"
    ps <- between (char '(') (char ')') (sepBy parseName (char ','))
    ws1
    es <- between (char '{') (char '}') (sepBy parseExpr (char ';'))
    ws
    return $ Just ( MethodDecl { methodParameters=ps, methodBody=es} )

parseMethod :: ReadP NamedMethodDecl
parseMethod = do
    n <- parseName
    ws1
    ps <- parseParams
    ws
    es <- parseExprs
    ws
    return $ NamedMethodDecl n (method ps es)
    where method a b = MethodDecl { methodParameters=a, methodBody=b}

parseReceive :: ReadP (Maybe ReceiveDecl)
parseReceive = do
    string "receive"
    ws1
    p <- parseName
    ws
    es <- between (char '{') (char '}') (sepBy parseExpr (char ';'))
    ws
    return $ Just (ReceiveDecl { receiveParam=p, receiveBody=es })

parseClass :: ReadP ClassDecl
parseClass = do
    string "class"
    ws1
    n <- parseName
    ws
    char '{'
    ws
    c <- option Nothing parseConstructor
    ws
    r <- option Nothing parseReceive -- TODO: fix order!
    ws
    m <- sepBy parseMethod ws
    ws
    char '}'
    ws
    return $ ClassDecl { className=n
                       , classConstructor=Nothing--c
                       , classMethods=m
                       , classReceive=Nothing--r
                       }

-- PROGRAM
parseProg :: ReadP Prog
parseProg = do
    decls <- many parseClass
    eof
    return decls

parseString :: String -> Either Error Prog
parseString str = case opt of
    [] -> Left "Parser error."
    (x:_) -> Right (fst x)
    where opt = readP_to_S parseProg str

parseFile :: FilePath -> IO (Either Error Prog)
parseFile filename = fmap parseString $ readFile filename
