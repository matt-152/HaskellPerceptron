import Perceptron
import IrisDataset

main :: IO ()
main = do
    dataset <- irisDataset
    let linearlySeprableDS = filterOutVirginica dataset
    let perceptronDataset = applyPerceptronLabelsToDS linearlySeprableDS

    let perceptron = mkPerceptron 0.5 [0,0,0,0]
    let trainedPerceptron = train perceptron perceptronDataset
    print trainedPerceptron
    let accuracy = assessAccuracy trainedPerceptron perceptronDataset
    putStrLn ("Accuracy: " ++ (show accuracy))

filterOutVirginica = filter notVirginica
  where
    notVirginica (label,_) = label /= "Iris-virginica"

applyPerceptronLabelsToDS = map applyPerceptronLabel
  where
    applyPerceptronLabel (label,inputs) = if label == "Iris-versicolor" then (1,inputs) else (-1,inputs)

assessAccuracy p ds = avg . map (toPoint . isCorrect) $ dataset
  where
    avg = (/ lengthDataset) . sum 
    lengthDataset = fromIntegral . length $ dataset
    isCorrect (label,inputs) = label == attemptToFire perceptron inputs
    toPoint True = 1
    toPoint False = 0
    dataset = ds
    perceptron = p

