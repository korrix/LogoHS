module Parser where

import qualified Data.Text as T
import Data.Scientific
import qualified Data.HashSet as HS

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text hiding (Parser(..))
import qualified Text.Megaparsec.Lexer as L

import Control.Monad.State as ST

import Ast

data ParseScope = ParseScope { scFunctionTable :: HM.HashMap Identifier Int
                             , scOperatorTable :: Map.Map Integer [Operator Parser Expr]
                             }

type Parser = ParsecT Dec T.Text (ST.State ParseScope)

initialScope = ParseScope HM.empty $ Map.fromList [
    (2, [Prefix (ENeg <$ symbol "-")])
  , (14, [InfixR (EAssign <$ symbol "=")])
  ]

-- Whitespace skipping
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Parens parsing
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curls :: Parser a -> Parser a
curls = between (symbol "{") (symbol "}")

-- Atomic values parsing
number :: Parser Scientific
number = lexeme L.number

hexadecimal :: Parser Integer
hexadecimal = lexeme (symbol "0x" *> L.hexadecimal)

boolean :: Parser Bool
boolean = (rword "prawda" *> return True) <|> (rword "fałsz" *> return False)

natural :: Parser Integer
natural = lexeme L.integer

character :: Parser String
character = fmap return (noneOf "\\\"\0\n\r\v\t\b\f") <|> escape
  where escape = do
          d <- char '\\'
          c <- oneOf "\\\"0nrvtbf"
          return [d, c]

quotedString :: Parser String
quotedString = do
  symbol "\""
  x <- many character
  symbol "\""
  return $ read $ ('\"' : concat x) ++ "\"" -- Dirty hack

-- Reserved keywords
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

-- list of reserved words
reservedWords :: HS.HashSet String
reservedWords = HS.fromList [ "oto", "pierwotna"
                            , "operator", "prawostronny", "lewostronny"
                            , "jeśli", "wpw"
                            , "iteruj", "warunek", "przez"
                            , "prawda", "fałsz", "="
                            ]

identifier :: Parser Identifier
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_")
    check x = if x `HS.member` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return $ Identifier (T.pack x)

stmt :: Parser Stmt
stmt = f <$> many stmt'
  where f [a] = a
        f l   = Seq l

stmt' :: Parser Stmt
stmt' = fundefStmt <|> opdefStmt <|> ifStmt <|> forStmt <|> exprStmt

variable :: Parser Identifier
variable = symbol ":" *> identifier <?> "variable"

operatorName :: Parser Identifier
operatorName = (Identifier . T.pack) <$> lexeme (some (oneOf "~!@#$%^&//*+-|<>:=.," :: Parser Char))
  <?> "allowed operator name"

fundefStmt :: Parser Stmt
fundefStmt = do
  rword "oto"
  funName <- identifier <?> "function name"
  funArgs <- many variable

  -- FIXME Function overriding
  modify $ \(ParseScope ft ot) -> let ft' = HM.insert funName (length funArgs) ft
                                  in ParseScope ft' ot

  funBody <- (Just <$> curls stmt) <|> (rword "pierwotna" >> return Nothing)

  return $ FunDef funName funArgs funBody
  <?> "function declaration"

opAssociativity :: Parser Assoc
opAssociativity =  (rword "prawostronny" >> return AssocRight)
               <|> (rword "lewostronny"  >> return AssocLeft)
               <|> return AssocNeutral
               <?> "associativity"

opdefStmt :: Parser Stmt
opdefStmt = do
  rword "operator"
  assoc <- opAssociativity
  opName <- operatorName
  fixity <- natural
  lvalue <- variable
  rvalue <- variable

  let opApp = EOpApp opName <$ symbol (T.unpack $ getIdentifier opName)
  let opDef = case assoc of { AssocLeft -> InfixL ; AssocRight -> InfixR; AssocNeutral -> InfixN }

  modify $ \(ParseScope ft ot) -> let ot' = Map.insertWith (++) fixity [opDef opApp] ot
                                  in ParseScope ft ot'

  opBody <- (Just <$> curls stmt) <|> (rword "pierwotna" >> return Nothing)

  return $ OpDef assoc opName fixity lvalue rvalue opBody
  <?> "operator declaration"

ifStmt :: Parser Stmt
ifStmt = do
  rword "jeśli"
  condition <- expression
  ifBody <- curls stmt
  elseBody <- optional (rword "wpw" >> curls stmt)
  return $ If condition ifBody elseBody
  <?> "if statement"

forStmt :: Parser Stmt
forStmt = do
  rword "iteruj"
  preExp <- expression
  rword "warunek"
  condition <- expression
  rword "przez"
  iterExp <- expression
  forBody <- curls stmt
  return $ For preExp condition iterExp forBody
  <?> "for statement"

exprStmt :: Parser Stmt
exprStmt = Expression <$> expression <?> "expression"

expression :: Parser Expr
expression = exprOpApp <|> term

term :: Parser Expr
term =  parens expression
    <|> exprFunApp
    <|> (EVar <$> variable)
    <|> (EHex <$> hexadecimal)
    <|> (ENumber <$> number)
    <|> (EString <$> quotedString)
    <|> (EBool <$> boolean)
    <|> (EList <$> brackets (expression `sepBy1` symbol ","))

exprFunApp :: Parser Expr
exprFunApp = do
  ctx <- gets scFunctionTable
  funName <- identifier
  funArgs <- case HM.lookup funName ctx of
        Just arity -> replicateM arity expression
        Nothing -> fail $ "Undefined function " ++ show (getIdentifier funName)

  return $ EFunApp funName funArgs

exprOpApp :: Parser Expr
exprOpApp = do
  ctx <- Map.toAscList <$> gets scOperatorTable
  makeExprParser term (map snd ctx)

logoParser :: Parser Stmt
logoParser = between sc eof stmt
