-- |Module for interpreting the abstract syntax from the Salsa Language parser.
-- The interpretations is done from SalsaAst to Gpx
module SalsaInterp where --(Position, interpolate, runProg) where

----------
-- Imports
--

import SalsaAst
import Gpx
import qualified Data.Map as M
import qualified Data.Maybe as Ma


--------------------------------
-- Context datatype and subtypes
--

data Environment = Environment { getDefs :: M.Map Ident Definition,
                                 getViews :: [Ident],
                                 getFramerate :: Integer } deriving Show
type State = M.Map Ident (M.Map Ident Position)
data Context = Context { getEnv :: Environment, getState :: State } deriving Show


-------------------------
-- The SalsaCommand monad
--

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> (a, State) }

instance Monad SalsaCommand where
    return x = SalsaCommand $ \con -> (x, getState con)
    m >>= f  = SalsaCommand $ \con ->
                   let (x, newState) = runSC m con
                   in runSC (f x) (Context (getEnv con) newState)


------------------
-- The Salsa monad
--

newtype Salsa a = Salsa {runS :: Context -> (a, Context) }

instance Monad Salsa where
    return x = Salsa $ \con -> (x, con)
    m >>= f  = Salsa $ \con ->
                   let (x, newCon) = runS m con
                   in runS (f x) newCon


---------------
-- External API
--

type Position = (Integer, Integer)

interpolate :: Integer -> Position -> Position -> [Position]
interpolate n p1 p2 = [(fst p2 - k*dx, snd p2 - k*dy) | k <- reverse [0..n-1]]
    where dx = (fst p2 - fst p1) `div` n
          dy = (snd p2 - snd p1) `div` n

runProg :: Integer -> Program -> Animation
runProg 0 _ = error "Framerate cannot be zero"
runProg n prog = undefined


buildContexts :: [DefCom] -> Context -> [Context] -> [Context]
buildContexts [] con cons = con:cons
buildContexts (def@(Def _):defcoms) con cons = let (_, newCon) = runS (defCom def) con in buildContexts defcoms newCon cons
buildContexts (com@(Com _):defcoms) con cons = let (_, newCon) = runS (defCom com) con in buildContexts defcoms newCon (con:cons)

animate n (env1, st1) (env2, st2) = undefined


-----------------------------
-- Functions for SalsaCommand
--

ask :: SalsaCommand Context
ask = SalsaCommand $ \con -> (con, getState con)

local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a
local f m = SalsaCommand $ \con -> let con' = f con
                                   in runSC m con'

putState :: State -> SalsaCommand ()
putState newState = SalsaCommand $ const ((), newState)

activeViews :: Ident -> SalsaCommand [Ident]
activeViews ident = SalsaCommand $ \con ->
    let idents = case M.lookup ident ((getDefs . getEnv) con) of
                     Nothing -> error "View not defined"
                     Just (Group _ ids) -> ids
                     Just (Viewdef {}) -> [ident]
                     _ -> error "Id is not a view"
    in (idents, getState con)

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
                                  _ -> error "Postion was not constants"

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
                                       con <- ask
                                       putState (move (getState con) ((getViews.getEnv) con) idents (Abs (Const x) (Const y)))
command (Move idents (Rel e1 e2)) = do dx <- expr e1
                                       dy <- expr e2
                                       con <- ask
                                       putState (move (getState con) ((getViews.getEnv) con) idents (Rel (Const dx) (Const dy)))
command (Par c1 c2) = do command c1
                         command c2
command (At c ident) = do views <- activeViews ident
                          local (\con ->
                            let defs = (getDefs . getEnv) con
                                n = (getFramerate . getEnv) con
                                st = getState con
                            in Context (Environment defs views n) st) (command c)


----------------------
-- Functions for Salsa
--

setView :: [Ident] -> Salsa ()
setView idents = Salsa $ \con -> ((), Context (Environment ((getDefs.getEnv) con) idents ((getFramerate.getEnv) con)) (getState con))

insertDef :: Definition -> Salsa ()
insertDef def@(Viewdef ident _ _) =
    Salsa $ \(Context (defs, views, n) st) ->
    ((), Context (M.insert ident def defs, views, n) (M.insert ident M.empty st))
insertDef group@(Group ident _) =
    Salsa $ \(Context (defs, views, n) st) ->
    ((), Context (M.insert ident group defs, views, n) st)
insertDef rect@(Rectangle ident (Const x) (Const y) _ _ _) =
    Salsa $ \(Context (defs, views, n) st) ->
    ((), Context (M.insert ident rect defs, views, n) (insertShape ident (x,y) views st))
insertDef circ@(Circle ident (Const x) (Const y) _ _) =
    Salsa $ \(Context (defs, views, n) st) ->
    ((), Context (M.insert ident circ defs, views, n) (insertShape ident (x,y) views st))
insertDef _ = error "Def cannot be inserted or elements are not constants"

insertShape :: Ident -> Position -> [Ident] -> State -> State
insertShape _ _ [] st = st
insertShape ident pos (v:vs) st = case M.lookup v st of
    Nothing -> error "Couldn't find active view in state"
    Just view -> insertShape ident pos vs (M.insert v (M.insert ident pos view) st)

liftC :: SalsaCommand a -> Salsa a
liftC m = Salsa $ \con @ (Context env _) ->
    let (x, newState) = runSC m con
    in (x, Context env newState)

definition :: Definition -> Salsa ()
definition (Viewdef ident e1 e2) = do x <- liftC $ expr e1
                                      y <- liftC $ expr e2
                                      insertDef (Viewdef ident (Const x) (Const y))
                                      setView [ident]
definition (Rectangle ident e1 e2 e3 e4 col) = do x <- liftC $ expr e1
                                                  y <- liftC $ expr e2
                                                  w <- liftC $ expr e3
                                                  h <- liftC $ expr e4
                                                  insertDef (Rectangle ident (Const x) (Const y) (Const w) (Const h) col)
definition (Circle ident e1 e2 e3 col) = do x <- liftC $ expr e1
                                            y <- liftC $ expr e2
                                            r <- liftC $ expr e3
                                            insertDef (Circle ident (Const x) (Const y) (Const r) col)
definition (View ident) = do idents <- liftC $ activeViews ident
                             setView idents
definition group@(Group {}) = insertDef group

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = liftC $ command com