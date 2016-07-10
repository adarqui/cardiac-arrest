module Test.Main (
  main
) where

import Control.Monad.Aff                     (makeAff)
import Control.Monad.Eff                     (Eff)
import Control.Monad.Eff.Console             (CONSOLE)
import Control.Applicative                   ((*>))
import Data.Array                            ((..))
import Data.Either                           (Either(..))
import Data.List                             as List
import Data.String                           as String
import Data.Traversable                      (for)
import Data.Unfoldable                       (replicate)
import Prelude                               (Unit, unit, void, bind, show, ($), (*), (<>), (==), (<$>))
import Test.Unit                             (suite, test)
import Test.Unit.Main                        (runTest)
import Test.Unit.Console                     (TESTOUTPUT, print)
import Test.Unit.Assert                      as Assert
import Text.Parsing.Simple                   (parse, sepBy1, string)



bigString :: Int -> String
bigString n = String.fromCharArray (replicate (1024*n) 'A')



bigWords :: Int -> String
bigWords n = String.joinWith " " (replicate (132*n) "word")



main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | e) Unit
main = runTest do

  suite "test big strings" do

    -- big strings test passes
    --
    test "testing big strings" do
      void $ for (1..10) $ \n -> do
        makeAff $ \_ succeed -> (print $ "\x2192 Testing string of size: " <> show (1024 * n) <> "\n") *> succeed unit
        Assert.equal (Right $ bigString n) (parse (string (bigString n)) (bigString n))

    -- words test passes
    --
    test "simple lots of words" do
      void $ for (1..10) $ \n -> do
        Assert.equal (Right $ List.singleton "word") (List.take 1 <$> parse (sepBy1 (string "word") (string " ")) (bigWords n))
