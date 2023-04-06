module Tests.TestPerceptron where

import Perceptron

testPerceptron :: IO ()
testPerceptron = do
    putStrLn "Testing Perceptron..."
    test_attemptToFire
    test_adjustWeights
    test_adjustWeightsConverges
    test_adjustWeightsConsidersInput
    test_train

assert :: Bool -> IO ()
assert cond | cond == True = return ()
            | otherwise = error "Assertion failed"

test_attemptToFire :: IO ()
test_attemptToFire = do
    let p = mkPerceptron 0.5 [0.0]
    let i = [0.0]
    assert (attemptToFire p i == -1)
    let i = [1.0]
    assert (attemptToFire p i == -1)

    let p = mkPerceptron 0.5 [1.0]
    let i = [1.0]
    assert (attemptToFire p i == 1)
    let i = [0.5]
    assert (attemptToFire p i == -1)
    let i = [0.6]
    assert (attemptToFire p i == 1)

    let p = mkPerceptron 0.7 [1.0]
    let i = [0.6]
    assert (attemptToFire p i == -1)
    let i = [0.7]
    assert (attemptToFire p i == -1)
    let i = [0.8]
    assert (attemptToFire p i == 1)

    let p = mkPerceptron 0.5 [1.0, 0.0]
    let i = [1.0, 0.0]
    assert (attemptToFire p i == 1)
    let i = [0.0, 1.0]
    assert (attemptToFire p i == -1)
    let i = [1.0, 0.0]
    assert (attemptToFire p i == 1)

    let p = mkPerceptron 1.0 [0.5, 0.5]
    let i = [1.0, 1.0]
    assert (attemptToFire p i == -1)
    let i = [1.5, 1.0]
    assert (attemptToFire p i == 1)
    return ()

sumWeights = sum . weights

test_adjustWeights :: IO ()
test_adjustWeights = do
    let p = mkPerceptron 0.5 [1.0]
    let li = (1, [1.0])
    assert (adjustWeights p li == p)

    let p1 = mkPerceptron 0.5 [1.0]
    let lia = (1, [0.5])
    let p2a = adjustWeights p1 lia
    assert ((sumWeights p2a) > (sumWeights p1))
    let lib = (-1, [0.6])
    let p2b = adjustWeights p1 lib
    assert ((sumWeights p2b) < (sumWeights p1))
    return ()

test_adjustWeightsConverges :: IO ()
test_adjustWeightsConverges = do
    let p = mkPerceptron 0.5 [1.0]
    let li1 = (1, [0.5])
    let li2 = (-1, [0.49])
    let p2 = adjustWeights p li1
    let p3 = adjustWeights p2 li2
    assert ((sumWeights p3) > (sumWeights p))
    assert ((sumWeights p3) <= (sumWeights p2))
    return ()

test_adjustWeightsConsidersInput :: IO()
test_adjustWeightsConsidersInput = do
    let p = mkPerceptron 1.0 [0.0, 1.0]
    let li1 = (1, [0.0, 1.0])
    let p2 = adjustWeights p li1
    let w1 = (weights p2) !! 1
    let w2 = (weights p2) !! 2
    assert (w1 == 0)
    assert (w2 > 1.0)
    return ()

test_train :: IO()
test_train = do
    let p = mkPerceptron 0.5 [1.0]
    let ds = [(1, [1.0])]
    assert ((train p ds) == p)

    let input = [0.5]
    let out = 1
    let ds = [(out, input)]
    let pt = train p ds
    assert ((attemptToFire pt input) == out)

    let ds = [(1, [1.0]),(1, [0.5])]
    let pt = train p ds
    assert ((attemptToFire pt [0.5]) == 1)

    let ds = [(1,[0.005])]
    let pt = train p ds
    assert ((attemptToFire pt [0.005]) == 1)
    return ()

