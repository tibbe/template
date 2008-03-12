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
-- Any other apperance of \"$\" in the string will result in an
-- 'Prelude.error' being raised.
--
-- Here is an example of a simple substitution:
--
-- > import qualified Data.ByteString.Lazy.Char8 as B
-- > import Text.Template
-- >
-- > context = Map.fromList . map packPair
-- >     where packPair (x, y) = (B.pack x, B.pack y)
-- >
-- > helloTemplate = B.pack "Hello, $name! Want some ${fruit}s?"
-- > helloContext = context [("name", "Johan"), ("fruit", "banana")]
-- >
-- > main = B.putStrLn $ substitute helloTemplate helloContext
--
-- If you render the same template multiple times it's faster to first
-- convert it to a more efficient representation using 'template' and
-- then rendering it using 'render'. In fact, all that 'substitute' does
-- is to combine these two steps.

module Text.Template
    (
     -- * The @Template@ type.
     Template,           -- abstract

     -- * The @Context@ type.
     Context,
            
     -- * Basic interface
     template,           -- :: ByteString -> Template
     render,             -- :: Template -> Context -> ByteString
     substitute,         -- :: ByteString -> Context -> ByteString
     showTemplate,       -- :: Template -> ByteString

     -- * I\/O with 'Template's

     -- ** Files
     readTemplate,       -- :: FilePath -> IO Template
     renderToFile,       -- :: FilePath -> Template -> Context -> IO ()

     -- ** I\/O with Handles
     hRender             -- :: Handle -> Template -> Context -> IO ()
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Int
import Control.Monad.State
import qualified Control.Monad.State as State
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (takeWhile)
import System.IO

-- -----------------------------------------------------------------------------

-- | A repesentation of a 'Data.ByteString.Lazy.Char8.ByteString'
-- template, supporting efficient rendering.
newtype Template = Template [Frag]

instance Eq Template where
    t1 == t2 = showTemplate t1 == showTemplate t2

instance Show Template where
    show = B.unpack . showTemplate

-- | Shows the template string.
showTemplate :: Template -> ByteString
showTemplate (Template fs) = B.concat $ map showFrag fs

data Frag = Lit !ByteString | Var !ByteString !Bool

instance Show Frag where
    show = B.unpack . showFrag

showFrag :: Frag -> ByteString
showFrag (Var s b) | b         = B.concat [B.pack "${", s, B.pack "}"]
                   | otherwise = B.concat [B.pack "$", s]
showFrag (Lit s) = B.concatMap escape s
    where escape c = case c of
                       '$' -> B.pack "$$"
                       c'  -> B.singleton c'

-- | A mapping with keys that match the placeholders in the template.
type Context = Map ByteString ByteString

-- -----------------------------------------------------------------------------
-- Basic interface

-- | Creates a template from a template string.
template :: ByteString -> Template
template = runParser pTemplate

pTemplate :: Parser Template
pTemplate = pFrags >>= return . Template

pFrags :: Parser [Frag]
pFrags = do
  c <- peek
  case c of
    Nothing  -> return []
    Just '$' -> do c' <- peekSnd
                   case c' of
                     Just '$' -> do Just '$' <- char
                                    Just '$' <- char
                                    continue (return $ Lit $ B.pack "$")
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

pIdentifier :: Parser ByteString
pIdentifier = do
  c <- peek
  case c of
    Just c' -> if isAlphaNum c'
                 then takeWhile isIdentifier
                 else liftM parseError pos
    Nothing -> liftM parseError pos
    where
      isIdentifier c = or [isAlphaNum c, c `elem` "_'"]

parseError :: (Int64, Int64) -> a
parseError (row, col) = error $ "Invalid placeholder in string: line " ++
                        show row ++ ", col " ++ show col

-- | Performs the template substitution, returning a new
-- 'Data.ByteString.Lazy.Char8.ByteString'.
--
-- If a key is not found in the context an 'Prelude.error' is raised.
render :: Template -> Context -> ByteString
render (Template frags) ctx = B.concat $ map (renderFrag ctx) frags

renderFrag :: Context -> Frag -> ByteString
renderFrag _ (Lit s)     = s
renderFrag ctx (Var x _) =
    case Map.lookup x ctx of
      Just s  -> s
      Nothing -> error $ "Key not found: " ++ (show $ B.unpack x)

-- | Performs the template substitution, returning a new
-- 'Data.ByteString.Lazy.Char8.ByteString'. Note that
--
-- > substitute tmpl ctx == render (template tmpl) ctx
--
-- If a key is not found in the context an 'Prelude.error' is raised.
substitute :: ByteString -> Context -> ByteString
substitute tmpl = render (template tmpl)

-- -----------------------------------------------------------------------------
-- Files

-- | Reads a template from a file lazily.  Use 'text mode' on Windows to
-- interpret newlines
readTemplate :: FilePath -> IO Template
readTemplate f = (return . template) =<< B.readFile f

-- | Renders and writes a template to a file. This is more efficient
-- than first 'render'ing the template to a
-- 'Data.ByteString.Lazy.Char8.ByteString' and then writing it to a file
-- using 'Data.ByteString.Lazy.Char8.writeFile'.
renderToFile :: FilePath -> Template -> Context -> IO ()
renderToFile f tmpl = B.writeFile f . render tmpl

-- -----------------------------------------------------------------------------
-- I/O with Handles

-- | Renders and writes a template to a 'System.IO.Handle'. This is more
-- efficient than first 'render'ing the template to a
-- 'Data.ByteString.Lazy.Char8.ByteString' and then writing it to a
-- 'System.IO.Handle' using 'Data.ByteString.Lazy.Char8.hPutStr'.
hRender :: Handle -> Template -> Context -> IO ()
hRender h (Template frags) ctx = mapM_ (B.hPut h . renderFrag ctx) frags

-- -----------------------------------------------------------------------------
-- ByteString parser

type Parser = State (ByteString, Int64, Int64)
    
char :: Parser (Maybe Char)
char = do
  (s, row, col) <- get
  if B.null s
    then return Nothing
    else do c <- return $! B.head s
            case c of
              '\n' -> put (B.tail s, row + 1 :: Int64, 1 :: Int64)
              _    -> put (B.tail s, row, col + 1 :: Int64)
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
  char
  c <- char
  put s
  return c

takeWhile :: (Char -> Bool) -> Parser ByteString
takeWhile p = do
  (s, row, col) <- get
  case B.span p s of
    (x, s') -> do 
                let newlines = B.elemIndices '\n' x
                    n = B.length x
                    row' = row + fromIntegral (length newlines)
                    col' = case newlines of
                             [] -> col + n
                             _  -> n - last newlines
                put (s', row', col')
                return x

pos :: Parser (Int64, Int64)
pos = do
  (_, row, col) <- get
  return (row, col)

runParser :: Parser a -> ByteString -> a
runParser p s = evalState p (s, 1 :: Int64, 1 :: Int64)
