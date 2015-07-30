
-- | A simple Lambda Calculus Interpreter.
module Main where
import Lambda.Exp
import Lambda.Reduce
import Lambda.Pretty
import Lambda.Lexer
import Lambda.Parser
import Data.List
import qualified Control.Monad.IO.Class         as C
import qualified System.Console.Haskeline       as H

---------------------------------------------------------------------------------------------------
-- | The program starts here.
--
--   * The 'IO ()' is a type signature that says this computation
--     may perform some IO actions, and returns a unit value.
--
main :: IO ()
main 
 = do   -- Load macros from a file in the working directory.
        macros  <- loadMacros "Prelude.macros"

        -- Start the REPL.
        --  We use an external library called "Haskeline" to manage input.
        --  http://hackage.haskell.org/package/haskeline
        H.runInputT H.defaultSettings (repl macros)


-- | The Read-Eval-Print-Loop.
--
--   * The 'InputT IO ()' means this computation gets some input from the
--     console, as well as performing some IO actions.
--
repl :: [(Macro, Exp)] -> H.InputT IO ()
repl macros
 = getline
 where
        -- Get a line from the console.
        getline
         = do   minput <- H.getInputLine "> "
                case minput of
                 Nothing        -> return ()
                 Just ":quit"   -> return ()
                 Just input     -> execute (tokenize input)

        execute tokens
         -- If there was no input, then just show the prompt again.
         | null tokens
         = getline

         -- If the expression parses ok then run run it.
         | Just xx      <- parse tokens
         = do   C.liftIO $ reduceExp macros xx
                getline

         -- The expression didn't parse.
         | otherwise
         = do   H.outputStr "Parse Error\n\n"
                getline


-- | Reduce an expression with the given macros, 
--   printing out each intermediate step in the reduction.
reduceExp :: [(Macro, Exp)] -> Exp -> IO ()
reduceExp macros xx
        -- | If the expression is already in normal form,
        --   then just print it back.
        | isNormal xx
        = putStr $ pretty xx ++ "\n\n"

        -- | Run an expression that contains redexes.
        | otherwise
        = reduceLoop macros xx


-- | Keep reducing this expression and printing out intermediate
--      steps until it has no more redexes.
reduceLoop :: [(Macro, Exp)] -> Exp -> IO ()
reduceLoop macros xx0
 = loop (0 :: Int) xx0
 where
        loop count xx

         -- Keep reducing until the result is either in normal form
         -- or just a macro name.
         | not $ isNormal xx
         = case reduce macros xx of
                Left name 
                 -> error $ "Undefined macro " ++ show name

                Right xx'
                 -> do  putStr $ pretty xx' ++ "\n"

                        -- If the expression has redexes, but trying to reduce it just
                        -- produces the same expression again, then bail out now instead
                        -- of running forever.
                        if xx == xx'
                         then putStr "...\n\n"
                         else loop (count + 1) xx'

         | otherwise
         = do   putStr $ "[" ++ show count ++ " steps]\n"
                putStr "\n"


---------------------------------------------------------------------------------------------------
-- | Load macro definitions from the given file.
loadMacros :: String -> IO [(Macro, Exp)]
loadMacros fileName
 = do   -- Read the file.
        file    <- readFile fileName

        -- Macros are defined on lines that start with a # character.
        -- Treat everything else as a comment.
        let stripped
                = filter (isPrefixOf "#") $ lines file

        -- Parse all the definitions.
        case sequence $ map parseDef stripped of
         Left  err    -> error $ show err
         Right macros -> return macros


-- | Parse a macro definition 
parseDef :: String -> Either ErrorMacro (Macro, Exp)
parseDef line
 = case words line of 
        ('#' : name) : "=" : defStr
          -> case parse $ tokenize $ concat $ intersperse " " $ defStr of
                Just def -> Right (M name, def)
                Nothing  -> Left $ ErrorCannotParse (M name)
        _  -> Left $ ErrorBadLine


-- | Errors that can happen when we're parsing macro definitions.
data ErrorMacro
        = ErrorBadLine
        | ErrorCannotParse Macro
        deriving Show

