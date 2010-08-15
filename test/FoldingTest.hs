import Folding

main :: IO ()
main = do
    c <- getContents
    let d = unfold c
    putStrLn d
