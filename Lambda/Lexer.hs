
-- | A lexer for lambda expression syntax.
module Lambda.Lexer 
        ( Token (..)
        , tokenize)
where
import qualified Data.Char      as C


-- | A token in lambda expression syntax.
data Token
        = KError Char           -- ^ Some junk or invalid character.
        | KBra                  -- ^ Open braket.
        | KKet                  -- ^ Close braket.
        | KLam                  -- ^ Lambda symbol.
        | KDot                  -- ^ Dot symbol.
        | KVar  String          -- ^ Variable names.
        | KMacro String         -- ^ Macro names.
        deriving (Show, Eq)


-- | Tokenize a string.
tokenize :: String -> [Token]
tokenize []             =  []
tokenize (c:cs)

        -- Ignore whitespace.
        | C.isSpace c     = tokenize cs

        -- Single character tokens.
        | c == '('      = KBra : tokenize cs
        | c == ')'      = KKet : tokenize cs
        | c == '\\'     = KLam : tokenize cs
        | c == '.'      = KDot : tokenize cs

        -- Macro names start with a '#' symbol.
        | c == '#'
        , name          <- takeWhile C.isAlphaNum cs
        , rest          <- drop (length name) cs
        = KMacro name : tokenize rest

        -- Variables must start with an alphabetic charater.
        -- Subsequent characters can be alpha or numeric.
        | C.isAlpha c     
        = let   restOfVar       = takeWhile C.isAlphaNum cs
                restOfString    = drop (length restOfVar) cs
          in    KVar (c:restOfVar) : tokenize restOfString

        -- If we see a junk character then stop scanning.
        | otherwise
        = [KError c]
