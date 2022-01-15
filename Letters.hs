import System.Environment
import Data.List
import System.Random
import Control.Monad (replicateM)

dropChar :: String -> [String]
dropChar str = map head $ group $ aux "" (head str) (tail str)
    where
        aux :: String -> Char -> String -> [String]
        aux begin _ [] = [ begin ]
        aux begin current rest = (begin ++ rest) : aux (begin ++ [current]) (head rest) (tail rest)

solve :: [String] -> [String] -> [String]
solve letters knownWords = (filter (\word -> (sort word) `elem` letters) knownWords) ++ solve (concatMap dropChar letters) knownWords

main :: IO ()
main = do
    args <- getArgs

    randomLetters <- flip replicateM (randomRIO ('a','z')) =<< randomRIO (9,9)
    let userLetters = head args

    let letters = if (length args < 1) then randomLetters else userLetters

    knownWordsFile <- readFile("words.txt")
    let knownWords = sortBy (\w1 w2 -> compare (length w2) (length w1)) $ filter ((<= (length letters)) . length) $ lines knownWordsFile

    let answer = take 10 $ solve [(sort letters)] knownWords

    let answerSize = length $ head answer

    putStrLn $ ("Provided Letters: " ++ letters)
    putStrLn ""
    putStrLn ((show answerSize) ++ " letter answer is longest")
    putStrLn ""


    putStrLn $ unlines answer
