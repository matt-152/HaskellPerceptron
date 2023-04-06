import Tests.TestPerceptron
import Tests.TestIris

main :: IO ()
main = do
    putStrLn "Testing..."
    testPerceptron
    testIris
    putStrLn "Tests complete"

