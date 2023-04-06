module Tests.TestIris where

import IrisDataset

testIris :: IO ()
testIris = do
    putStrLn "Testing Iris Dataset..."
    test_csvToDataset

assert :: Bool -> IO ()
assert cond | cond == True = return ()
            | otherwise = error "Assertion failed"

test_csvToDataset :: IO ()
test_csvToDataset = do
    let a = ["1.0,ClassA"]
    assert (csvToDataset a == [("ClassA",[1.0])])
    let a = ["1.0,ClassB"]
    assert (csvToDataset a == [("ClassB",[1.0])])
    let a = ["1.2,ClassA"]
    assert (csvToDataset a == [("ClassA",[1.2])])
    let a = ["1.25,ClassA"]
    assert (csvToDataset a == [("ClassA",[1.25])])
    let a = ["1.0,2.0,ClassA"]
    assert (fst (csvToDataset a !! 0) == "ClassA")
    assert (snd (csvToDataset a !! 0) == [1.0,2.0])
    let a = ["1.0,ClassA","2.0,ClassB"]
    let ds = csvToDataset a
    assert (ds !! 0 == ("ClassA",[1.0]))
    assert (ds !! 1 == ("ClassB",[2.0]))

    let a = ["5.1,3.5,1.4,0.2,Iris-setosa","4.6,3.4,1.4,0.3,Iris-setosa"]
    let ds = csvToDataset a
    assert (ds !! 0 == ("Iris-setosa",[5.1,3.5,1.4,0.2]))
    assert (ds !! 1 == ("Iris-setosa",[4.6,3.4,1.4,0.3]))

