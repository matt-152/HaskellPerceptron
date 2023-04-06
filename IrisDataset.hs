module IrisDataset where

irisDataset :: IO [(String,[Float])]
irisDataset = do
    fileContents <- readFile "./iris.csv"
    let fileLines = lines fileContents
    let dataset = csvToDataset fileLines
    return dataset

csvToDataset :: [String] -> [(String,[Float])]
csvToDataset []     = []
csvToDataset (x:xs) = (classLabel,inputs) : csvToDataset xs
  where
    classLabel = extractLabel x
    inputs = parseInputs x
    parseInputs = read . surroundBrackets . extractInputs 
    extractLabel = actOnEndOfList $ takeWhile (/=',')
    extractInputs = actOnEndOfList $ tail . dropWhile (/=',')
    surroundBrackets x = "[" ++ x ++ "]"
    actOnEndOfList f = reverse . f . reverse

