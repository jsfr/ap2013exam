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
import qualified Control.Arrow as Ar


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
                   in runSC (f x) $ Context (getEnv con) newState


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
                     firstCon = last contexts
                     start = buildAnimation 1 firstCon firstCon
                     frames = buildFrames n (reverse contexts) []
                 in (views,  start ++ frames)


----------------------------------------
-- Functions for manipulating the context
--

-- |Returns the viewdef and group definitions.
getVDefs :: Context -> M.Map Ident Definition
getVDefs (Context (viewdefs, _, _, _) _) = viewdefs

-- |Returns the circle and rectangle definitions.
getSDefs :: Context -> M.Map Ident Definition
getSDefs (Context (_, shapedefs, _, _) _) = shapedefs

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

-- |Returns a context with its active view(s) changed to the given view(s).
updateView :: Context -> [Ident] -> Context
updateView con views = Context (getVDefs con,
                                getSDefs con,
                                views, getFr con) (getSt con)

-----------------------------
-- Functions for SalsaCommand
--

ask :: SalsaCommand Context
ask = SalsaCommand $ \con -> (con, getSt con)

askSt :: SalsaCommand State
askSt = SalsaCommand $ getSt Ar.&&& getSt

askEnv :: SalsaCommand Environment
askEnv = SalsaCommand $ getEnv Ar.&&& getSt

askVDef :: Ident -> SalsaCommand [Ident]
askVDef ident = SalsaCommand $ \con ->
    let idents = case M.lookup ident (getVDefs con) of
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

move :: [Ident] -> [Ident] -> Pos -> SalsaCommand ()
move [] [] _ = return ()
move [] _ _ = return ()
move _ [] _ = return ()
move (v:vs) idents p = do st <- askSt
                          case M.lookup v st of
                              Nothing -> move vs idents p
                              Just view -> do let view' = move' view idents
                                              putState (M.insert v view' st)
                                              move vs idents p
    where move' view [] = view
          move' view (i:is) = case (M.lookup i view, p) of
                                  (Nothing, _) ->
                                      move' view is
                                  (Just _, Abs (Const x) (Const y)) ->
                                      move' (M.insert i (x, y) view) is
                                  (Just (x, y), Rel (Const dx) (Const dy)) ->
                                      move' (M.insert i (x+dx, y+dy) view) is
                                  (Just _, _) -> error "Position not evaluated first."

expr :: Expr -> SalsaCommand Integer
expr (Plus e1 e2) = do x <- expr e1
                       y <- expr e2
                       return $ x + y
expr (Minus e1 e2) = do x <- expr e1
                        y <- expr e2
                        return $ x - y
expr (Const int) = return int
expr (Xproj ident) = do con <- ask
                        let Just (x,_) = minimum [ e | e <- map (M.lookup ident)
                                                                (M.elems $ getSt con),
                                                                Ma.isJust e]
                        return x
expr (Yproj ident) = do con <- ask
                        let Just (_,y) = minimum [ e | e <- map (M.lookup ident)
                                                                (M.elems $ getSt con),
                                                                Ma.isJust e]
                        return y

command :: Command -> SalsaCommand ()
command (Move idents (Abs e1 e2)) = do x <- expr e1
                                       y <- expr e2
                                       con <- ask
                                       move (getViews con) idents (Abs (Const x) (Const y))
command (Move idents (Rel e1 e2)) = do dx <- expr e1
                                       dy <- expr e2
                                       con <- ask
                                       move (getViews con) idents (Rel (Const dx) (Const dy))
command (Par c1 c2) = do command c1
                         command c2
command (At c ident) = do views <- askVDef ident
                          local (`updateView` views) (command c)


----------------------
-- Functions for Salsa
--

liftC :: SalsaCommand a -> Salsa a
liftC m = Salsa $ \con ->
    let (x, newState) = runSC m con
    in (x, Context (getEnv con) newState)

putEnv :: Environment -> Salsa ()
putEnv env = Salsa $ \con -> ((), Context env (getSt con))

putView :: [Ident] -> Salsa ()
putView idents = Salsa $ \con -> ((), updateView con idents)

putShape :: [Ident] -> Ident -> Position -> Salsa ()
putShape [] _ _ = return ()
putShape (v:vs) ident pos =
    do st <- liftC askSt
       case M.lookup v st of
           Nothing -> error "Active view is missing in the state"
           Just view -> do liftC $ putState (M.insert v (M.insert ident pos view) st)
                           putShape vs ident pos

putDef :: Definition -> Salsa ()
putDef def@(Viewdef ident _ _) =
    do st <- liftC askSt
       (vdefs, sdefs, views, fr) <- liftC askEnv
       liftC $ putState (M.insert ident M.empty st)
       putEnv (M.insert ident def vdefs, sdefs, views, fr)
putDef def@(Group ident _) =
    do (vdefs, sdefs, views, fr) <- liftC askEnv
       putEnv (M.insert ident def vdefs, sdefs, views, fr)
putDef rect@(Rectangle ident (Const x) (Const y) _ _ _) =
    do (vdefs, sdefs, views, fr) <- liftC askEnv
       putShape views ident (x,y)
       putEnv (vdefs, M.insert ident rect sdefs, views, fr)
putDef circ@(Circle ident (Const x) (Const y) _ _) =
    do (vdefs, sdefs, views, fr) <- liftC askEnv
       putShape views ident (x,y)
       putEnv (vdefs, M.insert ident circ sdefs, views, fr)
putDef _ = error "Def has not been evaluated or should not be inserted."

definition :: Definition -> Salsa ()
definition (Viewdef ident e1 e2) =
    do x <- liftC $ expr e1
       y <- liftC $ expr e2
       putDef (Viewdef ident (Const x) (Const y))
       putView [ident]
definition (Rectangle ident e1 e2 e3 e4 col) =
    do x <- liftC $ expr e1
       y <- liftC $ expr e2
       w <- liftC $ expr e3
       h <- liftC $ expr e4
       putDef (Rectangle ident (Const x) (Const y) (Const w) (Const h) col)
definition (Circle ident e1 e2 e3 col) =
    do x <- liftC $ expr e1
       y <- liftC $ expr e2
       r <- liftC $ expr e3
       putDef (Circle ident (Const x) (Const y) (Const r) col)
definition (View ident) =
    do idents <- liftC $ askVDef ident
       putView idents
definition group@(Group {}) = putDef group

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
                          (getVDefs con)))

buildFrames :: Integer -> [Context] -> [Frame] -> [Frame]
buildFrames _ [] frames = frames
buildFrames _ (_:[]) frames = frames
buildFrames n (con1:con2:cons) frames = buildFrames n (con2:cons) (frames ++ buildAnimation n con1 con2)

buildAnimation :: Integer -> Context -> Context -> [Frame]
buildAnimation n con1 con2 =
    let positions = M.intersectionWith (M.intersectionWith (interpolate n)) (getSt con1) (getSt con2)
        newShapes = M.intersectionWith (flip M.difference) (getSt con1) (getSt con2)
        instrs = M.mapWithKey (\vident view ->
                     M.mapWithKey (\sident plist ->
                         map (\pos ->
                             draw pos (M.lookup sident $ getSDefs con1) vident )
                         plist )
                     view)
                 positions
        frames = map Li.concat $ Li.transpose $ M.elems $ M.map (Li.transpose . M.elems) instrs
        lastframe = concatMap M.elems $ M.elems $ M.mapWithKey (\vident view ->
                        M.mapWithKey (\sident pos ->
                            draw pos (M.lookup sident $ getSDefs con2) vident )
                        view)
                    newShapes
    in init frames ++ [last frames ++ lastframe]

draw :: (Integer, Integer) -> Maybe Definition -> ViewName -> GpxInstr
draw (x,y) (Just (Rectangle _ _ _ (Const w) (Const h) col)) view = DrawRect x y w h view (show col)
draw (x,y) (Just (Circle _ _ _ (Const r) col)) view = DrawCirc x y r view (show col)
draw _ Nothing _ = error "Shape from state was not in environment"
draw _ (Just _) _ = error "Not a shape or shape was malformed, e.g. with not constant parameters"