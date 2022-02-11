import Test.TLT

main :: IO ()
main = do
  tlt test
  {- runJTMST $ do
    counts <- testEx1
    liftIO $ putStrLn $ show counts -}
  putStrLn "Test run completed"

test :: Monad m => TLT m ()
test = do
  "True passes" ~::- True
  "2 is 3 as single Bool" ~::- 2 == 3
  "2 is 2 as single Bool" ~::- 2 == 2
  "2 is 3 as pure assertion" ~: 2 !==- 3
  "2 is 2 as pure assertion" ~: 2 !==- 2
  "2 is 3 as result" ~: 2 !== return 3
  "2 is 2 as result" ~: 2 !== return 2
  "2 not 3 as pure assertion" ~: 2 !/=- 3
  "2 not 2 as pure assertion" ~: 2 !/=- 2
  "2 not 3 as result" ~: 2 !/= return 3
  "2 not 2 as result" ~: 2 !/= return 2
