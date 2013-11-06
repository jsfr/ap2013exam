import Test.HUnit
import SalsaParser
import SalsaAst

def0 :: Test
def0 = TestCase $ assertBool "viewdef" $
        parseString "viewdef Id_0 10 20" ==
            Right [Def (Viewdef "Id_0" (Const 10) (Const 20))]

def1 :: Test
def1 = TestCase $ assertBool "rectangle" $
        parseString "rectangle id0 10 20 30 40 blue" ==
            Right [Def (Rectangle "id0" (Const 10) (Const 20) (Const 30) (Const 40) Blue)]

def2 :: Test
def2 = TestCase $ assertBool "circle" $
        parseString "circle id0 10 20 30 blue" ==
            Right [Def (Circle "id0" (Const 10) (Const 20) (Const 30) Blue)]

def3 :: Test
def3 = TestCase $ assertBool "view" $
        parseString "view Id0" ==
            Right [Def (View "Id0")]

def4 :: Test
def4 = TestCase $ assertBool "group" $
        parseString "group Id0 [Id1 Id2]" ==
            Right [Def (Group "Id0" ["Id1", "Id2"])]

com0 :: Test
com0 = TestCase $ assertBool "move" $
        parseString "id_0 -> (10,10)" ==
            Right [Com (Move ["id_0"] (Abs (Const 10) (Const 10)))]

com1 :: Test
com1 = TestCase $ assertBool "at" $
        parseString "id0 -> (10,10) @ Id1" ==
            Right [Com (At (Move ["id0"] (Abs (Const 10) (Const 10))) "Id1")]

com2 :: Test
com2 = TestCase $ assertBool "par" $
        parseString "id0 -> (10,10) || id1 -> (20,20)" ==
            Right [Com (Par (Move ["id0"] (Abs (Const 10) (Const 10))) (Move ["id1"] (Abs (Const 20) (Const 20))))]

com3 :: Test
com3 = TestCase $ assertBool "move list" $
        parseString "id0 id1 id2 -> (10,20)" ==
            Right [Com (Move ["id0", "id1", "id2"] (Abs (Const 10) (Const 20)))]

com4 :: Test
com4 = TestCase $ assertBool "move relative" $
        parseString "id0 -> +(10,20)" ==
            Right [Com (Move ["id0"] (Rel (Const 10) (Const 20)))]

expr0 :: Test
expr0 = TestCase $ assertBool "plus/minus" $
        parseString "id0 -> (0, 10 + 20 - 30)" ==
            Right [Com (Move ["id0"] (Abs (Const 0) (Minus (Plus (Const 10) (Const 20)) (Const 30))))]

expr1 :: Test
expr1 = TestCase $ assertBool "primitive" $
        parseString "id0 -> (0, id1.x)" ==
            Right [Com (Move ["id0"] (Abs (Const 0) (Xproj "id1")))]

expr2 :: Test
expr2 = TestCase $ assertBool "precedence at/par" $
        parseString "id0 -> (0,0) || id1 -> (0,0) @ Id2" ==
            Right [Com (Par (Move ["id0"] (Abs (Const 0) (Const 0))) (At (Move ["id1"] (Abs (Const 0) (Const 0))) "Id2"))]

ident0 :: Test
ident0 = TestCase $ assertBool "uppercase sident" $
        parseString "Id0 -> (10,10)" ==
            Left "Parser Error: Couldn't parse"

ident1 :: Test
ident1 = TestCase $ assertBool "lowercase vident" $
        parseString "view id0" ==
            Left "Parser Error: Couldn't parse"

ident2 :: Test
ident2 = TestCase $ assertBool "reserved word sident" $
        parseString "view -> (10,10)" ==
            Left "Parser Error: Couldn't parse"

ident3 :: Test
ident3 = TestCase $ assertBool "reserved word vident" $
        parseString "view View" ==
            Right [Def (View "View")]

ws0 :: Test
ws0 = TestCase $ assertBool "alphanumeric tokens, command" $
        parseString "id0\nid1\tid2 -> (0,0)" ==
            Right [Com (Move ["id0","id1","id2"] (Abs (Const 0) (Const 0)))]

ws1 :: Test
ws1 = TestCase $ assertBool "alphanumeric tokens, definition" $
        parseString "group Group [Id0\nId1\tId2]" ==
            Right [Def (Group "Group" ["Id0","Id1","Id2"])]

ws2 :: Test
ws2 = TestCase $ assertBool "symbolic tokens, command" $
        parseString "id0->+(0+ 10, 10)@ Id1" ==
            Right [Com (At (Move ["id0"] (Rel (Plus (Const 0) (Const 10)) (Const 10))) "Id1")]

main :: IO Counts
main = runTestTT $ TestList [
       TestLabel "Simple definitions" $ TestList [def0, def1, def2, def3, def4],
       TestLabel "Simple commands" $ TestList [com0, com1, com2, com3, com4],
       TestLabel "Expressions" $ TestList [expr0, expr1, expr2],
       TestLabel "Idents" $ TestList [ident0, ident1, ident2, ident3],
       TestLabel "Whitespace" $ TestList [ws0, ws1, ws2]
       ]