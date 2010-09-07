import System.Environment

import Text.ICalendar.Folding

{-
 - The aim of this test is to ensure that lines get unfolded correctly.
 - For now, taking two input file names it should tell us wether or not one is the unfolded version
 - of the other and vice versa.
 -}

arg_count = 2

main :: IO ()
main = do
    args <- getArgs
    if length args /= arg_count
        then 
            error ("Expected two arguments but got: " ++ show args)
        else
            let [folded_file, unfolded_file] = args
                in do
                    foldedText <- readFile folded_file
                    unfoldedText <- readFile unfolded_file

                    if unfold foldedText == unfoldedText 
                        then putStrLn "Unfold Worked."
                        else putStrLn "Unfold FAILED."

                    if fold unfoldedText == foldedText
                        then putStrLn "Fold Worked."
                        else putStrLn "Fold FAILED."

                    --putStrLn . fold $ unfoldedText
                    --putStrLn . unfold $ foldedText
