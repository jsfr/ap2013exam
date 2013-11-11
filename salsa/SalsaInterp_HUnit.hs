import Test.HUnit
import SalsaInterp
import SalsaAst
import Gpx

kf0 :: Test
kf0 = TestCase $ assertBool "keyframes simple.salsa" $ 
            runProg 1 [Def (Viewdef "Default" (Const 400) (Const 400)),
                       Def (Rectangle "box" (Const 10) (Const 400) (Const 20) (Const 20) Green),
                       Com (Move ["box"] (Abs (Const 10) (Const 200))),
                       Com (Move ["box"] (Rel (Const 100) (Const 0))),
                       Com (Move ["box"] (Abs (Const 110) (Const 400))),
                       Com (Move ["box"] (Rel (Minus (Const 0) (Const 100)) (Const 0)))] ==
                        ([("Default",400,400)],[[DrawRect 10 400 20 20 "Default" "Green"],
                         [DrawRect 10 200 20 20 "Default" "Green"],
                         [DrawRect 110 200 20 20 "Default" "Green"],
                         [DrawRect 110 400 20 20 "Default" "Green"],
                         [DrawRect 10 400 20 20 "Default" "Green"]])

kf1 :: Test
kf1 = TestCase $ assertBool "keyframes multi.salsa" $
        runProg 1 [Def (Viewdef "One" (Const 500) (Const 500)),
                   Def (Viewdef "Two" (Const 400) (Const 400)),
                   Def (Group "Both" ["One","Two"]),
                   Def (View "Both"),
                   Def (Rectangle "larry" (Const 10) (Const 350) (Const 20) (Const 20) Blue),
                   Def (Rectangle "fawn" (Const 300) (Const 350) (Const 15) (Const 25) Plum),
                   Def (View "Two"),
                   Com (Par (Move ["larry"] (Abs (Const 300) (Const 350))) (Move ["fawn"] (Abs (Const 10) (Const 350)))),
                   Def (View "Both"),
                   Com (Move ["larry","fawn"] (Rel (Const 0) (Minus (Const 0) (Const 300))))] ==
                    ([("One",500,500),
                     ("Two",400,400)],
                     [[DrawRect 300 350 15 25 "One" "Plum",DrawRect 10 350 20 20 "One" "Blue", 
                     DrawRect 300 350 15 25 "Two" "Plum",DrawRect 10 350 20 20 "Two" "Blue"],
                     [DrawRect 300 350 15 25 "One" "Plum",DrawRect 10 350 20 20 "One" "Blue",
                     DrawRect 10 350 15 25 "Two" "Plum",DrawRect 300 350 20 20 "Two" "Blue"],
                     [DrawRect 300 50 15 25 "One" "Plum",DrawRect 10 50 20 20 "One" "Blue",
                     DrawRect 10 50 15 25 "Two" "Plum",DrawRect 300 50 20 20 "Two" "Blue"]])

kf2 :: Test
kf2 = TestCase $ assertBool "keyframes defcom_mixed.salsa" $
    runProg 1 [Def (Viewdef "Main" (Const 200) (Const 200)),
               Def (Rectangle "a" (Const 0) (Const 0) (Const 10) (Const 10) Red),
               Com (Move ["a"] (Abs (Const 100) (Const 100))),
               Def (Rectangle "b" (Const 20) (Const 20) (Const 20) (Const 20) Blue),
               Def (Rectangle "c" (Const 80) (Const 80) (Const 30) (Const 30) Plum),
               Com (Move ["b"] (Abs (Const 50) (Const 50))),
               Def (Rectangle "d" (Const 10) (Const 20) (Const 30) (Const 40) Green)] ==
                ([("Main",200,200)],
                 [[DrawRect 0 0 10 10 "Main" "Red"],
                 [DrawRect 100 100 10 10 "Main" "Red",
                 DrawRect 20 20 20 20 "Main" "Blue",
                 DrawRect 80 80 30 30 "Main" "Plum"],
                 [DrawRect 100 100 10 10 "Main" "Red",
                 DrawRect 50 50 20 20 "Main" "Blue",
                 DrawRect 80 80 30 30 "Main" "Plum",
                 DrawRect 10 20 30 40 "Main" "Green"]])

interpolate0 :: Test
interpolate0 = TestCase $ assertBool "1 frame" $
                interpolate 1 (1,1) (10,10) == [(10,10)]

interpolate1 :: Test
interpolate1 = TestCase $ assertBool "2 frames" $
                interpolate 2 (1,1) (10,10) == [(6,6),(10,10)]

interpolate2 :: Test
interpolate2 = TestCase $ assertBool "9 frames" $
                interpolate 9 (1,1) (10,10) == [(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9),(10,10)]

interpolate3 :: Test
interpolate3 = TestCase $ assertBool "9 frames minus" $
                interpolate 9 (10,10) (1,1) == [(9,9),(8,8),(7,7),(6,6),(5,5),(4,4),(3,3),(2,2),(1,1)]

main :: IO Counts
main = runTestTT $ TestList [
       TestLabel "Keyframes" $ TestList [kf0, kf1, kf2],
       TestLabel "Interpolation" $ TestList [interpolate0, interpolate1,
                                             interpolate2, interpolate3]
       ]