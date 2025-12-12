main :: IO ()
main = do
    args <- getArgs
    config <- parseConfig args
    inputContent <- getContents
    let points = map parsePoint (lines inputContent) 
    let result = solveStream config points          
    mapM_ printPoint result