module Vector exposing (..)


type alias Vector =
    { x : Float, y : Float }


type alias Point =
    { x : Int, y : Int }


showVector : Vector -> String
showVector v =
    "(" ++ String.fromFloat v.x ++ ", " ++ String.fromFloat v.y ++ ")"


add : Vector -> Vector -> Vector
add v1 v2 =
    Vector (v1.x + v2.x) (v1.y + v2.y)


addPoints : Point -> Point -> Point
addPoints p1 p2 =
    Point (p1.x + p2.x) (p1.y + p2.y)


toPoint : Vector -> Point
toPoint v =
    { x = round v.x, y = round v.y }


pointToFloat : Point -> Float
pointToFloat p =
    toFloat p.x + toFloat p.y ^ -1


toVector : Point -> Vector
toVector p =
    Vector (toFloat p.x) (toFloat p.y)


scale : Vector -> Float -> Vector
scale v f =
    Vector (v.x * f) (v.y * f)


pointEqual : Point -> Point -> Bool
pointEqual p1 p2 =
    p1.x == p2.x && p1.y == p2.y


showPoint : Point -> String
showPoint p =
    "(" ++ String.fromInt p.x ++ ", " ++ String.fromInt p.y ++ ")"


rotateLineFrom : Vector -> Float -> Float -> Vector
rotateLineFrom start radius radiant =
    add start (pointOnCircle radius radiant)


pointOnCircle : Float -> Float -> Vector
pointOnCircle radius radiant =
    let
        deltaX =
            cos (pi * radiant) * radius

        deltaY =
            sin (pi * radiant) * radius
    in
    Vector deltaX deltaY


rotate90Degree : Vector -> Vector
rotate90Degree v =
    { v | x = -v.y, y = v.x }


flipOnX : Vector -> Vector
flipOnX v =
    { v | x = -v.x }


y : Vector -> Float
y v =
    v.y


zero : Vector
zero =
    Vector 0 0


flipOnY : Vector -> Vector
flipOnY v =
    { v | y = -v.y }
