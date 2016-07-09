module Test.Main (
  main
) where

import Control.Monad.Eff             (Eff)
import Control.Monad.Eff.Console     (CONSOLE)
import Control.Monad.Aff.Console     (logShow)
import Data.Array                    ((..))
import Data.Either                   (Either(..))
import Data.String                   as String
import Data.Traversable              (for)
import Data.Unfoldable               (replicate)
import Prelude                       (Unit, void, bind, show, ($), (*), (<>))
import Test.Assert                   (ASSERT, assert')
import Test.Unit                     (suite, test)
import Test.Unit.Main                (runTest)
import Test.Unit.Console             (TESTOUTPUT)
import Test.Unit.Assert              as Assert
import Text.Parsing.Parser           (runParser)
import Text.Parsing.Parser.Language  (haskellDef)
import Text.Parsing.Parser.Token     (TokenParser, makeTokenParser)



testTokenParser :: TokenParser
testTokenParser = makeTokenParser haskellDef



bigString :: Int -> String
bigString n = String.fromCharArray (replicate (1024*n) 'A')



main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | e) Unit
main = runTest do
  suite "test big strings" do
    test "simple strings" do
      void $ for (1..10) $ \n -> do
        logShow $ "Test big string of size: " <> show (1024 * n)
        Assert.equal (runParser (bigString n) testTokenParser.identifier) (Right $ bigString n)
