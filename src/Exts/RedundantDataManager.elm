module RedundantDataManager exposing (a, b, c, updateB, updateC)


type Data a b c
    = Data a b (a -> c) (a -> b -> a) (a -> c -> a) (b -> c -> b)


a : Data a b c -> a
a (Data a1 _ _ _ _ _) =
    a1


b : Data a b c -> b
b (Data _ b1 _ _ _ _) =
    b1


c : Data a b c -> c
c (Data a1 _ get _ _ _) =
    get a1


updateB : Data a b c -> b -> Data a b c
updateB (Data a1 _ getC setB setC1 setC2) b2 =
    let
        newA =
            setB a1 b2
    in
    Data newA b2 getC setB setC1 setC2


updateC : Data a b c -> c -> Data a b c
updateC (Data a1 b1 getC setB setC1 setC2) c1 =
    let
        newB =
            setC2 b1 c1

        newA =
            setB (setC1 a1 c1) newB
    in
    Data newA newB getC setB setC1 setC2
