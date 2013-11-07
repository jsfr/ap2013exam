import Test.HUnit
import SalsaInterp
import SalsaAst

kf0 :: Test
kf0 = TestCase $ assertBool "keyframes simple.salsa" $ 
            runProg ==

main :: IO Counts
main = runTestTT $ TestList [
       TestLabel "Keyframes" $ TestList [kf0],
       TestLabel "Two frames" $ TestList []
       ]