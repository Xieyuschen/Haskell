import System.IO 
main = do
    handle<-openFile "1.txt" ReadMode
    contents<-hGetContents handle
    putStr contents
    hClose handle