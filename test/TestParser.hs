import ICalParse
import Text.ParserCombinators.Parsec
import Folding

main :: IO ()
main = do
    c <- getContents
    case parse icalendarFile "(stdin)" (unfold c) of
        Left e -> do
                    putStrLn "Error parsing input: "
                    print e
        Right r -> mapM_ print r
