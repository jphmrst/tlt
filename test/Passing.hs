
import Test.TLT
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Control.Monad
import Control.Monad.IO.Class

main :: IO ()
main = do
  tlt test
  {-
  tlt ttest
  tlt $ runExceptT extest
  -}

test :: TLT IO ()
test = do
  "True passes" ~::- True
  "2 is 2 as single Bool" ~::- 2 == 2
  inGroup "== assertions" $ do
    inGroup "pure" $ do
      "2 is 2 as pure assertion" ~: 2 @==- 2
    inGroup "monadic" $ do
      "2 is 2 as result" ~: 2 @== return 2
  inGroup "/= pure assertions" $ do
    "2 not 3" ~: 2 @/=- 3
    "2 not 4" ~: 2 @/=- 4
  "2 not 3 as result" ~: 2 @/= return 3

{-
extest :: ExceptT String (TLT IO) ()
extest = do
  noUncaught "extest1" $ do
    "6 is 6 as pure assertion" ~: 6 @==- 6
    "7 is 7 as pure assertion" ~: 7 @==- 7
  uncaught "extest2" $ do
    "8 is 8 as pure assertion" ~: 8 @==- 8
    throwE "Boom"
    "9 is 9 as pure assertion" ~: 9 @==- 9
  uncaughtWith "extest3"
    (do "10 is 10 as pure assertion" ~: 10 @==- 10
        throwE "Boom"
        "11 is 11 as pure assertion" ~: 11 @==- 11)
    (\e -> "The exception should be \"Boom\""
             ~: "Boom" @==- e)
-}
