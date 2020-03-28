{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiWayIf     #-}

module Problem01A where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable       (foldl')
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Semigroup


-- Header

newtype Parser t a = Parser
  { unParser :: ParseState t -> ParseResult (a, ParseState t)
  } deriving Functor

data ParseState t = ParseState Position [t]
  deriving (Show)

initialParseState :: [t] -> ParseState t
initialParseState xs = ParseState 0 xs

nextParseState :: ParseState t -> Maybe (t, ParseState t)
nextParseState (ParseState pos0 s0) = case s0 of
  []   -> Nothing
  t:s1 -> Just (t, ParseState (pos0 + 1) s1)

type Position = Int

data ParseResult a
  = ParseSuccess (NonEmpty a)
  | ParseFailure [(Position, String)]
  deriving (Show, Functor)

fromParseResult :: ParseResult a -> Either [(Position, String)] a
fromParseResult = \case
  ParseSuccess (x :| _) -> Right x
  ParseFailure es       -> Left $ take 10 es

instance Applicative ParseResult where
  pure x = ParseSuccess (x :| [])
  (<*>) = ap

instance Monad ParseResult where
  ParseSuccess xs >>= f = sconcat $ f <$> xs
  ParseFailure es >>= _ = ParseFailure es

parseFail :: ParseState t -> String -> ParseResult a
parseFail (ParseState pos _) e = ParseFailure [(pos, e)]

instance Semigroup (ParseResult a) where
  ParseSuccess xs1 <> ParseSuccess xs2 = ParseSuccess $ xs1 <> xs2
  ParseFailure _   <> ParseSuccess xs2 = ParseSuccess xs2
  ParseSuccess xs1 <> ParseFailure _   = ParseSuccess xs1
  ParseFailure es1 <> ParseFailure es2 = ParseFailure $ es1 <> es2

runParser :: Parser t a -> [t] -> Either [(Position, String)] a
runParser (Parser p) s = second fst $ fromParseResult $ p $ initialParseState s

instance Applicative (Parser t) where
  pure x = Parser \s -> pure (x, s)

  Parser pf <*> Parser px = Parser \s0 -> do
    (f, s1) <- pf s0
    (x, s2) <- px s1
    pure (f x, s2)

instance Monad (Parser t) where
  Parser p >>= f = Parser \s0 -> do
    (r, s1) <- p s0
    let Parser pr = f r
    pr s1

instance MonadFail (Parser t) where
  fail e = Parser \s -> parseFail s e

instance Alternative (Parser t) where
  empty = fail "empty"

  Parser p1 <|> Parser p2 = Parser \s0 -> p1 s0 <> p2 s0

eofP :: Parser t ()
eofP = Parser \s0 -> case nextParseState s0 of
  Nothing -> pure ((), s0)
  _       -> parseFail s0 "not end yet"

takeTokenP :: Parser t t
takeTokenP = Parser \s0 -> case nextParseState s0 of
  Just (t, s1) -> pure (t, s1)
  Nothing      -> parseFail s0 "no tokens"

takeMapP :: (t -> Either String a) -> Parser t a
takeMapP f = Parser \s0 -> case nextParseState s0 of
  Just (t, s1) -> case f t of
    Right x -> pure (x, s1)
    Left  e -> parseFail s0 e
  Nothing      -> parseFail s0 "no tokens"

tokenP :: Eq t => t -> Parser t t
tokenP x = takeMapP \case
  t | t == x -> Right x
  _          -> Left "no matched token"

manyP :: Parser t a -> Parser t [a]
manyP p = goP
  where
    goP = ((:) <$> p <*> goP) <|> pure []


-- Main

type Lit = Int
type Var = String

data Token
  = TInt Int
  | TIdent String
  | TSymbol String
  | TArrow
  | TBackSlash
  | TParensL
  | TParensR
  deriving (Eq, Show)

prettyTokens :: [Token] -> String
prettyTokens = go
  where
    go []     = ""
    go [t]    = pToken t
    go (t:ts) = pToken t <> " " <> go ts

    pToken = \case
      TInt    x  -> show x
      TIdent  x  -> x
      TSymbol x  -> x
      TArrow     -> "->"
      TBackSlash -> "\\"
      TParensL   -> "("
      TParensR   -> ")"

data Expr
  = App Expr Expr
  | InfixApp Expr Var Expr
  | Abs Var Expr
  | Var Var
  | Lit Lit
  deriving (Eq, Show)


type TokenParser = Parser Token

programP :: TokenParser Expr
programP = exprP <* eofP

exprP :: TokenParser Expr
exprP = blockExprP

blockExprP :: TokenParser Expr
blockExprP
    =   absP
    <|> infixExpr1P
  where
    absP = Abs <$> (tokenP TBackSlash *> varP) <*> (tokenP TArrow *> exprP)

infixExpr1P :: TokenParser Expr
infixExpr1P
  =   (InfixApp <$> infixExpr2P <*> varopTokenP "+" <*> infixExpr1P)
  <|> (InfixApp <$> infixExpr2P <*> varopTokenP "-" <*> infixExpr1P)
  <|> infixExpr2P

infixExpr2P :: TokenParser Expr
infixExpr2P
  =   (InfixApp <$> infixExpr3P <*> varopTokenP "*" <*> infixExpr2P)
  <|> (InfixApp <$> infixExpr3P <*> varopTokenP "/" <*> infixExpr2P)
  <|> infixExpr3P

infixExpr3P :: TokenParser Expr
infixExpr3P = fexprP

fexprP :: TokenParser Expr
fexprP = foldl' App <$> aexprP <*> manyP aexprP

aexprP :: TokenParser Expr
aexprP
  =   (tokenP TParensL *> exprP <* tokenP TParensR)
  <|> (Lit <$> litP)
  <|> (Var <$> varP)

litP :: TokenParser Lit
litP = takeMapP \case
  TInt x -> Right x
  _      -> Left "no literal token"

varP :: TokenParser Var
varP
  =   identP
  <|> (tokenP TParensL *> symbolP <* tokenP TParensR)

varopTokenP :: String -> TokenParser Var
varopTokenP x = (tokenP $ TSymbol x) *> pure x

identP :: TokenParser Var
identP = takeMapP \case
  TIdent x -> Right x
  _        -> Left "no ident token"

symbolP :: TokenParser Var
symbolP = takeMapP \case
  TSymbol x -> Right x
  _         -> Left "no ident token"


-- Sample

-- |
--
-- @@
-- \x -> x
-- @@
--
-- >>> runParser exprP sampleToken1
-- Right (Abs "x" (Var "x"))
--
sampleToken1 :: [Token]
sampleToken1 =
  [ TBackSlash
  , TIdent "x"
  , TArrow
  , TIdent "x"
  ]

-- |
--
-- @@
-- \f -> \x -> ((x + f 1 * 2)) / 3
-- @@
--
-- >>> runParser exprP sampleToken2
-- Right (Abs "f" (Abs "x" (InfixApp (InfixApp (Var "x") "+" (InfixApp (App (Var "f") (Lit 1)) "*" (Lit 2))) "/" (Lit 3))))
--
sampleToken2 :: [Token]
sampleToken2 =
  [ TBackSlash
  , TIdent "f"
  , TArrow
  , TBackSlash
  , TIdent "x"
  , TArrow
  , TParensL
  , TParensL
  , TIdent "x"
  , TSymbol "+"
  , TIdent "f"
  , TInt 1
  , TSymbol "*"
  , TInt 2
  , TParensR
  , TParensR
  , TSymbol "/"
  , TInt 3
  ]

-- |
--
-- @@
-- \f -> \x -> x + f \y -> x * y
-- @@
--
-- >>> runParser exprP sampleToken3
-- Left ...
--
sampleToken3 :: [Token]
sampleToken3 =
  [ TBackSlash
  , TIdent "f"
  , TArrow
  , TBackSlash
  , TIdent "x"
  , TArrow
  , TIdent "x"
  , TSymbol "+"
  , TIdent "f"
  , TBackSlash
  , TIdent "y"
  , TArrow
  , TIdent "x"
  , TSymbol "*"
  , TIdent "y"
  ]

-- |
--
-- @@
-- \f -> \x -> (((x + f 1) * 2)
-- @@
--
-- >>> runParser exprP sampleToken4
-- Left ...
--
sampleToken4 :: [Token]
sampleToken4 =
  [ TBackSlash
  , TIdent "f"
  , TArrow
  , TBackSlash
  , TIdent "x"
  , TArrow
  , TParensL
  , TParensL
  , TParensL
  , TIdent "x"
  , TSymbol "+"
  , TIdent "f"
  , TInt 1
  , TParensR
  , TSymbol "*"
  , TInt 2
  , TParensR
  ]
