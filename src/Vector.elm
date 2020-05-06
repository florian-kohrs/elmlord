module Vector exposing (..)


type alias Vector =
    { xF : Float, yF : Float }


type alias Point =
    { x : Int, y : Int }


showVector : Vector -> String
showVector v =
    "(" ++ String.fromFloat v.xF ++ ", " ++ String.fromFloat v.yF ++ ")"


add : Vector -> Vector -> Vector
add v1 v2 =
    Vector (v1.xF + v2.xF) (v1.yF + v2.yF)


toPoint : Vector -> Point
toPoint v =
    { x = round v.xF, y = round v.yF }


toVector : Point -> Vector
toVector p =
    Vector (toFloat p.x) (toFloat p.y)


scale : Vector -> Float -> Vector
scale v f =
    Vector (v.xF * f) (v.yF * f)


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
    { v | xF = -v.yF, yF = v.xF }


flipOnX : Vector -> Vector
flipOnX v =
    { v | xF = -v.xF }


y : Vector -> Float
y v =
    v.yF


zero : Vector
zero =
    Vector 0 0


flipOnY : Vector -> Vector
flipOnY v =
    { v | yF = -v.yF }
