import ICalParse
import Text.ParserCombinators.Parsec

main = do
    c <- getContents
    case parse icalendarFile "(stdin)" c of
        Left e -> do
                    putStrLn "Error parsing input: "
                    print e
        Right r -> mapM_ print r
