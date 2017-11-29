import Control.Monad

main = do
    answers <- forM [1,2,3,4] (\i -> do
        putStrLn $ "Say something about " ++ show i
        getLine)
    putStrLn "Answers:"
    mapM_ putStrLn answers