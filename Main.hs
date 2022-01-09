import Data.List
import System.Environment

type Steps = [String]

-- Bool is used to signify if the value was calculated or not. I don't want
-- solutions with calculated values that weren't used
type Nums = [(Int, Bool)]

type StepNum = (Nums, Steps)
type StepNums = [StepNum]

allPairs :: StepNum -> StepNums
allPairs (list, steps) = [([x,y], steps) | (x:ys) <- tails $ reverse $ sort list, y <- ys]

operate :: Char -> StepNum -> StepNums
operate op list = foldl' folder [] $ allPairs list
    where
        folder :: StepNums -> StepNum -> StepNums
        folder acc pair@((x, _):(y, _):[], steps)
            | op == '+'            = ((x + y, True)     : listDiff, (newStep ++ (show (x + y)))     : steps) : acc
            | op == '-'            = ((x - y, True)     : listDiff, (newStep ++ (show (x - y)))     : steps) : acc
            | op == '*'            = ((x * y, True)     : listDiff, (newStep ++ (show (x * y)))     : steps) : acc
            | op == '/' && divides = ((x `div` y, True) : listDiff, (newStep ++ (show (x `div` y))) : steps) : acc
            | otherwise            = acc
            where
                divides = y /= 0 && x `mod` y == 0
                newStep = (show x) ++ " " ++ [op] ++ " " ++ (show y) ++ " = "

                listDiff = (fst list) \\ (fst pair)

doStep :: StepNum -> StepNums
doStep stepNum = (operate '+' stepNum) ++ (operate '-' stepNum) ++ (operate '*' stepNum) ++ (operate '/' stepNum)

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

    let intArgs = map read args :: [Int]

    let nums = init intArgs
    let target = last intArgs

    let results = solve nums target

    putStrLn $ "Numbers: " ++ (show nums)
    putStrLn $ "Target:  " ++ (show target)
    putStrLn ""
    putStrLn "Results:\n"
    putStr $ (safeInit $ unlines $ map (unlines . reverse . addHeader) $ filter ((/= []) . fst) results)
