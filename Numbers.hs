import Data.List
import System.Environment
import System.Random
import Control.Monad (replicateM)

type Steps = [String]

-- Bool is used to signify if the value was calculated or not. I don't want
-- solutions with calculated values that weren't used
type Nums = [(Int, Bool)]

type StepNum = (Nums, Steps)
type StepNums = [StepNum]

allPairs :: StepNum -> StepNums
allPairs (list, steps) = [([x,y], steps) | (x:ys) <- tails $ reverse $ sort list, y <- ys]

getOp :: Char -> (Int -> Int -> Int)
getOp '+' = (+)
getOp '-' = (-)
getOp '*' = (*)
getOp '/' = div

operate :: StepNum -> Char -> StepNums
operate list op = foldl' folder [] $ allPairs list
    where
        folder :: StepNums -> StepNum -> StepNums
        folder acc pair@((x, _):(y, _):[], steps)
            | op `elem` "+-*"         = common
            | op == '/' && divides    = common
            | otherwise                = acc
            where
                divides = y /= 0 && x `mod` y == 0

                canRadical = (ans ^ y) == x

                newStep = (show x) ++ " " ++ [op] ++ " " ++ (show y) ++ " = "

                listDiff = (fst list) \\ (fst pair)

                ans = (getOp op) x y

                common = ((ans, True) : listDiff, (newStep ++ (show ans)) : steps) : acc

doStep :: StepNum -> StepNums
doStep stepNum = concatMap (operate stepNum) "+-*/"

setListUncalculated :: [Int] -> Nums
setListUncalculated = map (flip (,) False)

allUncalculated :: Nums -> Bool
allUncalculated = all (== False) . map snd

doSteps :: Int -> Int -> [Steps] -> StepNums -> [Steps]
doSteps 0 _ results _ = results
doSteps numSteps target results stepNums = doSteps (numSteps - 1) target newResults $ concatMap doStep stepNums
    where
        -- If the first num in our list is the target, and all values except for
        -- the first one are uncalculated values, then that is a solution
        -- (checking that all values except the first one are uncalculated makes
        -- sure that we don't end up making calculations that don't get used)
        newResults = results ++ (map snd $ filter ((\nums -> (fst $ head nums) == target && (allUncalculated $ tail nums)) . fst) stepNums)

solve :: [Int] -> Int -> [(Steps, Int)]
solve nums target = map (\group -> (head group, length group)) grouped
    where
        grouped = groupBy (\a b -> (length a) == (length b)) $ doSteps (length nums) target [] [(setListUncalculated nums, [])]

addHeader :: (Steps, Int) -> Steps
addHeader (steps, count) = steps ++ [header]
    where
        header = ((show $ 1 + (length steps)) ++ " Number Solution (1 of " ++ (show count) ++ " found):")

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

main :: IO()
main = do
    args <- getArgs

    -- Random values to use in case nothing is provided by the user
    randomNums <- replicateM 6 (randomRIO (1, 100) :: IO Int)
    randomTarget <- randomRIO (100, 999) :: IO Int

    -- If user provides 3 or less args, then use randomly generated values
    -- instead. 3 or less args means 2 numbers and 1 target which is pretty
    -- pointless.
    let intArgs = if length args <= 3 then randomNums ++ [randomTarget] else map read args :: [Int]

    let nums = init intArgs
    let target = last intArgs

    let results = solve nums target

    putStrLn $ "Numbers: " ++ (show nums)
    putStrLn $ "Target:  " ++ (show target)
    putStrLn ""
    putStrLn "Results:\n"
    putStr $ (safeInit $ unlines $ map (unlines . reverse . addHeader) $ filter ((/= []) . fst) results)
