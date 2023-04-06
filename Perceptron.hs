module Perceptron 
    ( mkPerceptron 
    , attemptToFire
    , adjustWeights
    , train
    , weights
    )
  where

data Perceptron = P
    { weights :: [Float]
    }
  deriving (Eq, Show)

type Inputs = [Float]
type LabledInputs = (Int, Inputs)
type LabeledDataset = [LabledInputs]

mkPerceptron :: Float -> [Float] -> Perceptron
mkPerceptron t w = P biasedWeights
  where
    biasedWeights = (-1 * threshold) : weights
    threshold = t
    weights = w

attemptToFire :: Perceptron -> Inputs -> Int
attemptToFire p i = if weightedInputsExceedThreshold then 1 else -1
  where
    weightedInputsExceedThreshold = wiDotProduct > 0
    wiDotProduct = foldl1 (+) (zipWith (*) weights_ inputsWBiasUnit)
    weights_ = weights p
    inputsWBiasUnit = 1 : i

adjustWeights :: Perceptron -> LabledInputs -> Perceptron
adjustWeights p li = P adjustedWeights
  where
    adjustedWeights = zipWith (+) wDeltas originalWeights
    originalWeights = weights p
    wDeltas = map (*outDiffNum) inputsWBiasUnit
    inputsWBiasUnit = 1 : inputs
    outDiffNum = fromIntegral $ outDiffInt
    outDiffInt = expectedOut - actualOut
    expectedOut = fst li
    actualOut = attemptToFire p inputs
    inputs = snd li

train :: Perceptron -> LabeledDataset -> Perceptron
train p ds = train_ p treadmill
  where
    treadmill = (dsInf, dsLen, dsLen)
    dsInf = ds ++ dsInf
    dsLen = length ds

type DatasetTreadmill= (LabeledDataset, Int, Int)
train_ :: Perceptron -> DatasetTreadmill-> Perceptron
train_ p dst | finished = p
             | otherwise = additionallyTrainedP
  where
    additionallyTrainedP = train_ p_ dst_
    p_ = adjustWeights p labeledInputs
    dst_ = (circularDataset,remaining_,total)
    finished = remaining == 0
    remaining_ = if pClassifiedCorrectly then remaining - 1 else total
    pClassifiedCorrectly = p == p_
    ((labeledInputs:circularDataset), remaining, total) = dst

