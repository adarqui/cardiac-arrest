module Test.Main (
  main
) where

import Control.Alt ((<|>))
import Control.Applicative           ((*>))
import Control.Monad.Aff             (makeAff)
import Control.Monad.Eff             (Eff)
import Control.Monad.Eff.Console     (CONSOLE)
import Control.Monad.Trampoline      (Trampoline, runTrampoline)
import Data.Array                    ((..))
import Data.Either                   (Either(..))
import Data.Identity (runIdentity)
import Data.String                   as String
import Data.Traversable              (for)
import Data.Unfoldable               (replicate)
import Prelude                       (class Monad, Unit, unit, void, bind, show, ($), (*), (<>), (==), (<<<))
import Test.Unit                     (suite, test)
import Test.Unit.Main                (runTest)
import Test.Unit.Console             (TESTOUTPUT, print)
import Test.Unit.Assert              as Assert
import Text.Parsing.Parser
import Text.Parsing.Parser.Language
import Text.Parsing.Parser.Pos
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token



type TrampolineTokenParser = GenTokenParser String Trampoline
type TrampolineLanguageDef = GenLanguageDef String Trampoline



-- | The language definition for the Haskell language.
trampolineHaskellDef  :: TrampolineLanguageDef
trampolineHaskellDef   =
    case trampolineHaskell98Def of
        (LanguageDef def) -> LanguageDef def
                { identLetter    = def.identLetter <|> char '#'
                , reservedNames  = def.reservedNames <>
                                   ["foreign","import","export","primitive"
                                   ,"_ccall_","_casm_"
                                   ,"forall"
                                   ]
                }



-- | The language definition for the language Haskell98.
trampolineHaskell98Def :: TrampolineLanguageDef
trampolineHaskell98Def = LanguageDef (unGenLanguageDef trampolineHaskellStyle)
                { reservedOpNames = ["::","..","=","\\","|","<-","->","@","~","=>"]
                , reservedNames   = [ "let","in","case","of","if","then","else"
                                    , "data","type"
                                    , "class","default","deriving","do","import"
                                    , "infix","infixl","infixr","instance","module"
                                    , "newtype","where"
                                    , "primitive"
                                    -- "as","qualified","hiding"
                                    ]
                }



trampolineHaskellStyle :: TrampolineLanguageDef
trampolineHaskellStyle = LanguageDef (unGenLanguageDef trampolineEmptyDef)
                { commentStart    = "{-"
                , commentEnd      = "-}"
                , commentLine     = "--"
                , nestedComments  = true
                , identStart      = letter
                , identLetter     = alphaNum <|> oneOf ['_', '\'']
                , opStart         = op'
                , opLetter        = op'
                , reservedOpNames = []
                , reservedNames   = []
                , caseSensitive   = true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']



trampolineEmptyDef :: TrampolineLanguageDef
trampolineEmptyDef = LanguageDef
            { commentStart:    ""
            , commentEnd:      ""
            , commentLine:     ""
            , nestedComments:  true
            , identStart:      letter <|> char '_'
            , identLetter:     alphaNum <|> oneOf ['_', '\'']
            , opStart:         op'
            , opLetter:        op'
            , reservedOpNames: []
            , reservedNames:   []
            , caseSensitive:   true
            }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']



testTokenParser :: TrampolineTokenParser
testTokenParser = makeTokenParser trampolineHaskellDef



bigString :: Int -> String
bigString n = String.fromCharArray (replicate (1024*n) 'A')



bigWords :: Int -> String
bigWords n = String.joinWith " " (replicate (132*n) "word")



runTrampolineParser s = runTrampoline <<< runParserT (PState { input: s, position: initialPos })



main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | e) Unit
main = runTest do

  suite "test big strings" do

    -- big strings test fails around ~3072 bytes
    --
    test "testing big strings" do
      void $ for (1..10) $ \n -> do
        makeAff $ \_ succeed -> (print $ "\x2192 Testing string of size: " <> show (1024 * n) <> "\n") *> succeed unit
        Assert.equal (Right $ bigString n) (runTrampolineParser (bigString n) testTokenParser.identifier)
