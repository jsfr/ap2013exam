-- |Module for interpreting the abstract syntax from the Salsa Language parser.
-- The interpretations is done from SalsaAst to Gpx
module SalsaInterp(Position, interpolate, runProg) where

----------
-- Imports
--

import SalsaAst
import Gpx
import qualified Data.Map as M
import qualified Data.Maybe as Ma
import qualified Data.List as Li


--------------------------------
-- Context datatype and subtypes
--

-- |Environment is a triple with a map of viewdef and shape definitions,
-- a list of active views and the framerate as an integer.
type Environment = (M.Map Ident Definition, M.Map Ident Definition, [Ident], Integer)

-- |State is a map of maps where the first key is the view,
-- and the second is a shape giving the position of the shape on the view.
type State = M.Map Ident (M.Map Ident Position)

-- |Context is a record with an Environment and State.
data Context = Context { getEnv :: Environment, getSt :: State } deriving Show


-------------------------
-- The SalsaCommand monad
--

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> (a, State) }

instance Monad SalsaCommand where
    return x = SalsaCommand $ \con -> (x, getSt con)
    m >>= f  = SalsaCommand $ \con ->
                   let (x, newState) = runSC m con
                   in runSC (f x) $ updateSt con newState


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
runProg n prog = let contexts = buildContexts prog (emptyCon n) []
                     views = buildViews (head contexts)
                     start = animate 1 (last contexts) (last contexts)
                     frames = buildFrames n (reverse contexts) []
                 in (views,  start ++ frames)


----------------------------------------
-- Functions for manipulating the context
--

-- |Returns the viewdef and group definitions.
getViDefs :: Context -> M.Map Ident Definition
getViDefs (Context (viewdefs, _, _, _) _) = viewdefs

-- |Returns the circle and rectangle definitions.
getShDefs :: Context -> M.Map Ident Definition
getShDefs (Context (_, shapedefs, _, _) _) = shapedefs

-- |Returns the active view(s).
getViews :: Context -> [Ident]
getViews (Context (_, _, views, _) _) = views

-- |Returns the framerate.
getFr :: Context -> Integer
getFr (Context (_, _, _, fr) _) = fr

-- |Returns a new empty context.
emptyCon :: Integer -> Context
emptyCon n = Context emptyEnv emptyState
  where emptyEnv = (M.empty, M.empty, [], n)
        emptyState = M.empty

-- |Returns a context with its state changed to the given state.
updateSt :: Context -> State -> Context
updateSt con = Context (getEnv con)

-- |Returns a context with its active view(s) changed to the given view(s).
updateView :: Context -> [Ident] -> Context
updateView con views = Context (getViDefs con,
                                getShDefs con,
                                views, getFr con) (getSt con)

-- |Returns a context with its view/group definitions changed to the given.
updateViDefs :: Context -> Ident -> Definition -> Context
updateViDefs con ident def = Context (M.insert ident def (getViDefs con),
                                      getShDefs con,
                                      getViews con,
                                      getFr con) (getSt con)

-- |Returns a context with its circle/rectangle definitions changed to the given.
updateShDefs :: Context -> Ident -> Definition -> Context
updateShDefs con ident def = Context (getViDefs con,
                                      M.insert ident def (getShDefs con),
                                      getViews con,
                                      getFr con) (getSt con)

-- |Returns a 'Just definition' if it exists in the given map.
-- Returns Nothing otherwise.
lookupDef :: Ident -> M.Map Ident Definition -> Maybe Definition 
lookupDef = M.lookup

-- |Moves the shapes in the given state to the given position and returns the
-- updated state.
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
                                  (Just _, Abs (Const x) (Const y)) ->
                                      move' (M.insert i (x, y) view) is
                                  (Just (x, y), Rel (Const dx) (Const dy)) ->
                                      move' (M.insert i (x + dx, y + dy) view) is
                                  _ -> error "Postion was not constants"

-- |Inserts a shape on the given view(s) and returns the updated state.
insertShape :: Ident -> Position -> [Ident] -> State -> State
insertShape _ _ [] st = st
insertShape ident pos (v:vs) st = case M.lookup v st of
    Nothing -> error "Active view is missing in the state"
    Just view -> insertShape ident pos vs (M.insert v (M.insert ident pos view) st)


-----------------------------
-- Functions for SalsaCommand
--

askCon :: SalsaCommand Context
askCon = SalsaCommand $ \con -> (con, getSt con)

askView :: Ident -> SalsaCommand [Ident]
askView ident = SalsaCommand $ \con ->
    let idents = case lookupDef ident (getViDefs con) of
                     Nothing -> error "View not defined"
                     Just (Group _ ids) -> ids
                     Just (Viewdef {}) -> [ident]
                     _ -> error "Id is not a view"
    in (idents, getSt con)

local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a
local f m = SalsaCommand $ \con -> let con' = f con
                                   in runSC m con'

putState :: State -> SalsaCommand ()
putState newState = SalsaCommand $ const ((), newState)

expr :: Expr -> SalsaCommand Integer
expr (Plus e1 e2) = do x <- expr e1
                       y <- expr e2
                       return $ x + y
expr (Minus e1 e2) = do x <- expr e1
                        y <- expr e2
                        return $ x - y
expr (Const int) = return int
expr (Xproj ident) = do con <- askCon
                        let Just (x,_) = minimum [ e | e <- map (M.lookup ident)
                                                                (M.elems $ getSt con),
                                                                Ma.isJust e]
                        return x
expr (Yproj ident) = do con <- askCon
                        let Just (_,y) = minimum [ e | e <- map (M.lookup ident)
                                                                (M.elems $ getSt con),
                                                                Ma.isJust e]
                        return y

command :: Command -> SalsaCommand ()
command (Move idents (Abs e1 e2)) = do x <- expr e1
                                       y <- expr e2
                                       con <- askCon
                                       putState (move (getSt con) (getViews con)
                                                 idents (Abs (Const x) (Const y)))
command (Move idents (Rel e1 e2)) = do dx <- expr e1
                                       dy <- expr e2
                                       con <- askCon
                                       putState (move (getSt con) (getViews con)
                                                 idents (Rel (Const dx) (Const dy)))
command (Par c1 c2) = do command c1
                         command c2
command (At c ident) = do views <- askView ident
                          local (`updateView` views) (command c)


----------------------
-- Functions for Salsa
--

putView :: [Ident] -> Salsa ()
putView idents = Salsa $ \con -> ((), updateView con idents)

insertDef :: Definition -> Salsa ()
insertDef def@(Viewdef ident _ _) = Salsa $
    \con -> let st = M.insert ident M.empty (getSt con)
                newCon1 = updateViDefs con ident def
                newCon2 = updateSt newCon1 st
            in ((), newCon2)
insertDef def@(Group ident _) = Salsa $
    \con -> ((), updateViDefs con ident def)
insertDef def@(Rectangle ident (Const x) (Const y) _ _ _) = Salsa $
    \con -> let st = insertShape ident (x,y) (getViews con) (getSt con)
                newCon = updateSt (updateShDefs con ident def) st
            in ((), newCon)
insertDef def@(Circle ident (Const x) (Const y) _ _) = Salsa $
    \con -> let st = insertShape ident (x,y) (getViews con) (getSt con)
                newCon = updateSt (updateShDefs con ident def) st
            in ((), newCon)
insertDef _ = error "Def cannot be inserted or elements are not constants"

liftC :: SalsaCommand a -> Salsa a
liftC m = Salsa $ \con ->
    let (x, newState) = runSC m con
    in (x, updateSt con newState)

definition :: Definition -> Salsa ()
definition (Viewdef ident e1 e2) =
    do x <- liftC $ expr e1
       y <- liftC $ expr e2
       insertDef (Viewdef ident (Const x) (Const y))
       putView [ident]
definition (Rectangle ident e1 e2 e3 e4 col) =
    do x <- liftC $ expr e1
       y <- liftC $ expr e2
       w <- liftC $ expr e3
       h <- liftC $ expr e4
       insertDef (Rectangle ident (Const x) (Const y) (Const w) (Const h) col)
definition (Circle ident e1 e2 e3 col) =
    do x <- liftC $ expr e1
       y <- liftC $ expr e2
       r <- liftC $ expr e3
       insertDef (Circle ident (Const x) (Const y) (Const r) col)
definition (View ident) =
    do idents <- liftC $ askView ident
       putView idents
definition group@(Group {}) = insertDef group

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = liftC $ command com


---------------------------------------
-- Functions for building the animation
--

buildContexts :: [DefCom] -> Context -> [Context] -> [Context]
buildContexts [] con cons = con:cons
buildContexts (def@(Def _):defcoms) con cons =
    let (_, newCon) = runS (defCom def) con
    in buildContexts defcoms newCon cons
buildContexts (com@(Com _):defcoms) con cons =
    let (_, newCon) = runS (defCom com) con
    in buildContexts defcoms newCon (con:cons)

buildViews :: Context -> [(ViewName, Integer, Integer)]
buildViews con = M.elems (M.map (\(Viewdef ident (Const x) (Const y)) ->
                          (ident, x, y)) (M.filter (\def ->
                                                    case def of
                                                        Viewdef {} -> True
                                                        _ -> False)
                          (getViDefs con)))

buildFrames :: Integer -> [Context] -> [Frame] -> [Frame]
buildFrames _ [] frames = frames
buildFrames _ [_] frames = frames
buildFrames n (con1:con2:cons) frames = buildFrames n (con2:cons) (frames ++ animate n con1 con2)


animate :: Integer -> Context -> Context -> [Frame]
animate n con1 con2 =
    let shapes = M.intersectionWith (M.intersectionWith (interpolate n)) (getSt con1) (getSt con2)
        instrs = M.mapWithKey (\vident view -> M.mapWithKey (\sident plist -> map (\pos -> draw pos (M.lookup sident $ getShDefs con1) vident ) plist ) view) shapes
        frames = map Li.concat $ Li.transpose $ M.elems $ M.map (Li.transpose . M.elems) instrs
    in frames

draw :: (Integer, Integer) -> Maybe Definition -> ViewName -> GpxInstr
draw (x,y) (Just (Rectangle _ _ _ (Const w) (Const h) col)) view = DrawRect x y w h view (show col)
draw (x,y) (Just (Circle _ _ _ (Const r) col)) view = DrawCirc x y r view (show col)
draw _ Nothing _ = error "Shape from state was not in environment"
draw _ (Just _) _ = error "Not a shape or shape was malformed, e.g. with not constant parameters"
