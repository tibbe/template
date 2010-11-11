-- | A simple string substitution library that supports \"$\"-based
-- substitution. Substitution uses the following rules:
--
--    * \"$$\" is an escape; it is replaced with a single \"$\".
--
--    * \"$identifier\" names a substitution placeholder matching a
--      mapping key of \"identifier\". \"identifier\" must spell a
--      Haskell identifier. The first non-identifier character after the
--      \"$\" character terminates this placeholder specification.
--
--    * \"${identifier}\" is equivalent to \"$identifier\". It is
--      required when valid identifier characters follow the placeholder
--      but are not part of the placeholder, such as
--      \"${noun}ification\".
--
-- Any other appearance of \"$\" in the string will result in an
-- 'Prelude.error' being raised.
--
-- If you render the same template multiple times it's faster to first
-- convert it to a more efficient representation using 'template' and
-- then render it using 'render'. In fact, all that 'substitute' does
-- is to combine these two steps.

module Data.Text.Template
    (
     -- * The @Template@ type
     Template,

     -- * The @Context@ type
     Context,
     ContextA,

     -- * Basic interface
     template,
     render,
     substitute,
     showTemplate,

     -- * Applicative interface
     renderA,
     substituteA,

     -- * Example
     -- $example
    ) where

import Control.Applicative (Applicative(pure), (<$>))
import Control.Monad (liftM, liftM2)
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Traversable (traverse)
import Prelude hiding (takeWhile)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- -----------------------------------------------------------------------------

-- | A representation of a 'Data.Text' template, supporting efficient
-- rendering.
newtype Template = Template [Frag]

instance Eq Template where
    (==) = (==) `on` showTemplate

instance Show Template where
    show = T.unpack . showTemplate

-- | Shows the template string.
showTemplate :: Template -> T.Text
showTemplate (Template fs) = T.concat $ map showFrag fs

-- | A template fragment.
data Frag = Lit {-# UNPACK #-} !T.Text | Var {-# UNPACK #-} !T.Text !Bool

instance Show Frag where
    show = T.unpack . showFrag

showFrag :: Frag -> T.Text
showFrag (Var s b)
    | b          = T.concat [T.pack "${", s, T.pack "}"]
    | otherwise  = T.concat [T.pack "$", s]
showFrag (Lit s) = T.concatMap escape s
    where escape '$' = T.pack "$$"
          escape c   = T.singleton c

-- | A mapping from placeholders in the template to values.
type Context = T.Text -> T.Text

-- | Like 'Context', but with an applicative lookup function.
type ContextA f = T.Text -> f T.Text

-- -----------------------------------------------------------------------------
-- Basic interface

-- | Creates a template from a template string.
template :: T.Text -> Template
template = Template . combineLits . runParser pFrags

combineLits :: [Frag] -> [Frag]
combineLits [] = []
combineLits xs =
    let (lits,xs') = span isLit xs
    in case lits of
         []    -> gatherVars xs'
         [lit] -> lit : gatherVars xs'
         _     -> Lit (T.concat (map fromLit lits)) : gatherVars xs'
  where
    gatherVars [] = []
    gatherVars ys =
      let (vars,ys') = span isVar ys
      in vars ++ combineLits ys'

    isLit (Lit _) = True
    isLit _       = False

    isVar = not . isLit

    fromLit (Lit v) = v
    fromLit _       = undefined

-- | Performs the template substitution, returning a new 'LT.Text'.
render :: Template -> Context -> LT.Text
render (Template frags) ctxFunc = LT.fromChunks $ map renderFrag frags
  where
    renderFrag (Lit s)   = s
    renderFrag (Var x _) = ctxFunc x

-- | Like 'render', but allows the lookup to have side effects.  The
-- lookups are performed in order that they are needed to generate the
-- resulting text.
--
-- You can use this e.g. to report errors when a lookup cannot be made
-- successfully.  For example, given a list @ctx@ of key-value pairs
-- and a @Template@ @tmpl@:
--
-- > renderA tmpl (flip lookup ctx)
--
-- will return @Nothing@ if any of the placeholders in the template
-- don't appear in @ctx@ and @Just text@ otherwise.
renderA :: Applicative f => Template -> ContextA f -> f LT.Text
renderA (Template frags) ctxFunc = LT.fromChunks <$> traverse renderFrag frags
  where
    renderFrag (Lit s)   = pure s
    renderFrag (Var x _) = ctxFunc x

-- | Performs the template substitution, returning a new
-- 'LT.Text'. Note that
--
-- > substitute tmpl ctx == render (template tmpl) ctx
substitute :: T.Text -> Context -> LT.Text
substitute = render . template

-- | Performs the template substitution in the given @Applicative@,
-- returning a new 'LT.Text'. Note that
--
-- > substituteA tmpl ctx == renderA (template tmpl) ctx
substituteA :: Applicative f => T.Text -> ContextA f -> f LT.Text
substituteA = renderA . template

-- -----------------------------------------------------------------------------
-- Template parser

pFrags :: Parser [Frag]
pFrags = do
    c <- peek
    case c of
        Nothing  -> return []
        Just '$' -> do c' <- peekSnd
                       case c' of
                           Just '$' -> do Just '$' <- char
                                          Just '$' <- char
                                          continue (return $ Lit $ T.pack "$")
                           _        -> continue pVar
        _        -> continue pLit
  where
    continue x = liftM2 (:) x pFrags

pLit :: Parser Frag
pLit = do
    s <- takeWhile (/= '$')
    return $ Lit s

pVar :: Parser Frag
pVar = do
    Just '$' <- char
    c <- peek
    case c of
        Just '{' -> do Just '{' <- char
                       v <- pIdentifier
                       c' <- peek
                       case c' of
                         Just '}' -> do Just '}' <- char
                                        return $ Var v True
                         _        -> liftM parseError pos
        _        -> do v <- pIdentifier
                       return $ Var v False

pIdentifier :: Parser T.Text
pIdentifier = do
    c <- peek
    case c of
      Just c'
          | isAlphaNum c' -> takeWhile isIdentifier
          | otherwise     -> liftM parseError pos
      Nothing             -> liftM parseError pos
  where
    isIdentifier c = or [isAlphaNum c, c `elem` "_'"]

parseError :: (Int, Int) -> a
parseError (row, col) = error $ "Invalid placeholder in string: line " ++
                        show row ++ ", col " ++ show col

-- -----------------------------------------------------------------------------
-- Text parser

-- | The parser state.
data S = S {-# UNPACK #-} !T.Text  -- Remaining input
           {-# UNPACK #-} !Int     -- Row
           {-# UNPACK #-} !Int     -- Col

type Parser = State S

char :: Parser (Maybe Char)
char = do
    S s row col <- get
    if T.null s
      then return Nothing
      else do c <- return $! T.head s
              case c of
                '\n' -> put $! S (T.tail s) (row + 1) 1
                _    -> put $! S (T.tail s) row (col + 1)
              return $ Just c

peek :: Parser (Maybe Char)
peek = do
    s <- get
    c <- char
    put s
    return c

peekSnd :: Parser (Maybe Char)
peekSnd = do
    s <- get
    _ <- char
    c <- char
    put s
    return c

takeWhile :: (Char -> Bool) -> Parser T.Text
takeWhile p = do
    S s row col <- get
    case T.spanBy p s of
      (x, s') -> do
                  let xlines = T.lines x
                      row' = row + fromIntegral (length xlines - 1)
                      col' = case xlines of
                               [] -> col -- Empty selection
                               [sameLine] -> T.length sameLine
                                             -- Taken from this line
                               _  -> T.length (last xlines)
                                     -- Selection extends
                                     -- to next line at least
                  put $! S s' row' col'
                  return x

pos :: Parser (Int, Int)
pos = do
    S _ row col <- get
    return (row, col)

runParser :: Parser a -> T.Text -> a
runParser p s = evalState p $ S s 1 1

-- -----------------------------------------------------------------------------
-- Example

-- $example
--
-- Here is an example of a simple substitution:
--
-- > module Main where
-- >
-- > import qualified Data.ByteString.Lazy as S
-- > import qualified Data.Text as T
-- > import qualified Data.Text.Lazy.Encoding as E
-- >
-- > import Data.Text.Template
-- >
-- > -- | Create 'Context' from association list.
-- > context :: [(T.Text, T.Text)] -> Context
-- > context assocs x = maybe err id . lookup x $ assocs
-- >   where err = error $ "Could not find key: " ++ T.unpack x
-- >
-- > main :: IO ()
-- > main = S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
-- >   where
-- >     helloTemplate = T.pack "Hello, $name!\n"
-- >     helloContext  = context [(T.pack "name", T.pack "Joe")]
--
-- The example can be simplified slightly by using the
-- @OverloadedStrings@ language extension:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main where
-- >
-- > import qualified Data.ByteString.Lazy as S
-- > import qualified Data.Text as T
-- > import qualified Data.Text.Lazy.Encoding as E
-- >
-- > import Data.Text.Template
-- >
-- > -- | Create 'Context' from association list.
-- > context :: [(T.Text, T.Text)] -> Context
-- > context assocs x = maybe err id . lookup x $ assocs
-- >   where err = error $ "Could not find key: " ++ T.unpack x
-- >
-- > main :: IO ()
-- > main = S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
-- >   where
-- >     helloTemplate = "Hello, $name!\n"
-- >     helloContext  = context [("name", "Joe")]
