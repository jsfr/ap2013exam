
--
-- Skeleton for Salsa interpreter
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp
       -- (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import qualified Data.Map as M
import qualified Data.Maybe as Ma

--
-- The function interpolate
--

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
interpolate n p1 p2 = [(fst p2 - k*dx, snd p2 - k*dy) | k <- reverse [0..n-1]]
    where dx = (fst p2 - fst p1) `div` n
          dy = (snd p2 - snd p1) `div` n

--
-- Define the types Context and SalsaCommand
--

type Environment = (M.Map Ident Definition, [Ident], Integer)
type State = M.Map Ident (M.Map Ident Position) -- fst Ident = View, snd Ident = Shape
data Context = Context Environment State

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> (a, State) }

instance Monad SalsaCommand where
    return x = SalsaCommand $ \(Context _ state) -> (x, state)
    m >>= f  = SalsaCommand $ \con @ (Context env _) ->
                   let (x, newState) = runSC m con
                   in runSC (f x) (Context env newState)

-- functions for manipulating the context


--
-- Define the function command
--

ask :: SalsaCommand Context
ask = SalsaCommand $ \(Context env st) -> (Context env st, st)

putState :: State -> SalsaCommand ()
putState newState = SalsaCommand $ \(Context _ _) -> ((), newState)

move :: State -> [Ident] -> [Ident] -> Pos -> State
move st [] [] _ = st
move st [] _ _ = st
move st _ [] _ = st
move st (v:vs) ids p = case M.lookup v st of
                            Nothing -> move st vs ids p
                            Just view -> move (M.insert v (move' view ids) st) vs ids p
    where move' view [] = view
          move' view (i:is) = case (M.lookup i view, p) of
                                  (Nothing, _) -> move' view is
                                  (Just _, Abs (Const x) (Const y)) -> move' (M.insert i (x, y) view) is
                                  (Just (x, y), Rel (Const dx) (Const dy)) -> move' (M.insert i (x + dx, y + dy) view) is

expr :: Expr -> SalsaCommand Integer
expr (Plus e1 e2) = do x <- expr e1
                       y <- expr e2
                       return $ x + y
expr (Minus e1 e2) = do x <- expr e1
                        y <- expr e2
                        return $ x - y
expr (Const int) = return int
expr (Xproj ident) = do (Context _ st) <- ask
                        let Just (x,_) = minimum [ e | e <- map (M.lookup ident) (M.elems st), Ma.isJust e]
                        return x
expr (Yproj ident) = do (Context _ st) <- ask
                        let Just (_,y) = minimum [ e | e <- map (M.lookup ident) (M.elems st), Ma.isJust e]
                        return y

command :: Command -> SalsaCommand ()
command (Move idents (Abs e1 e2)) = do x <- expr e1
                                       y <- expr e2
                                       (Context (_, views, _) st) <- ask
                                       putState (move st views idents (Abs (Const x) (Const y)))
command (Move idents (Rel e1 e2)) = do dx <- expr e1
                                       dy <- expr e2
                                       (Context (_, views, _) st) <- ask
                                       putState (move st views idents (Rel (Const dx) (Const dy)))
command (Par c1 c2) = do command c1
                         command c2
--command (At c ident) = undefined check if ID is group and then do local on env


--
-- Define the type Salsa
--

data Salsa a = Salsa {runS :: Context -> (a, Context) }

instance Monad Salsa where
    return x = Salsa $ \con -> (x, con)
    m >>= f  = Salsa $ \con ->
                   let (x, newCon) = runS m con
                   in runS (f x) newCon

--
-- Define the functions liftC, definition, and defCom
--

-- functions for manipulating the context

setView :: [Ident] -> Salsa ()
setView idents = Salsa $ \(Context (defs, _, n) st) -> ((), Context (defs, idents, n) st)

insertDef :: Definition -> Salsa ()
insertDef def@(Viewdef ident _ _) = Salsa $ \(Context (defs, views, n) st) -> ((), Context (M.insert ident def defs, views, n) (M.insert ident M.empty st))
insertDef group@(Group ident idents) = Salsa $ \(Context (defs, views, n) st) -> ((), Context (M.insert ident group defs, views, n) st)
insertDef rect@(Rectangle ident (Const x) (Const y) w h col) = Salsa $ \(Context (defs, views, n) st) -> ((), Context (M.insert ident rect defs, views, n) (insertShape ident (x,y) views st))
insertDef circ@(Circle ident (Const x) (Const y) r col) = Salsa $ \(Context (defs, views, n) st) -> ((), Context (M.insert ident circ defs, views, n) (insertShape ident (x,y) views st))

insertShape :: Ident -> Position -> [Ident] -> State -> State
insertShape _ _ [] st = st
insertShape ident pos (v:vs) st = insertShape ident pos vs (M.insert v (M.insert ident pos (M.lookup v st)) st)

liftC :: SalsaCommand a -> Salsa a
liftC m = Salsa $ \con @ (Context env _) ->
    let (x, newState) = runSC m con
    in (x, Context env newState)

definition :: Definition -> Salsa ()
definition (Viewdef ident e1 e2) = do x <- liftC $ expr e1
                                      y <- liftC $ expr e2
                                      insertDef (Viewdef ident (Const x) (Const y))
                                      setView [ident]
definition (Rectangle ident e1 e2 e3 e4 col) = undefined
definition (Circle ident e1 e2 e3 col) = undefined
definition (View ident) = undefined
definition (Group ident idents) = undefined

defCom :: DefCom -> Salsa ()
defCom = undefined

--
-- Define the function runProg
--

runProg :: Integer -> Program -> Animation
runProg n prog = undefined