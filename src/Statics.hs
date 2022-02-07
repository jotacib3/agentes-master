module Statics where
type Statics = (Int, Int, Int, Int)

printStatics :: Statics -> IO ()
printStatics (victory, defeat, tie, dirt) =  do
    print $ "Robot's victories: " ++ show victory
    print $ "Robot's defeats: " ++ show defeat
    print $ "Simulations with end inconclusive: " ++ show tie
    print $ "The average amount of dirty cells: " ++ show dirt