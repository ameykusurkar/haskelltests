import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp key pairs = fromJust (lookup key pairs)

states :: LTS -> [State]
states = nub . (0:) . (map (snd.fst))

transitions :: State -> LTS -> [Transition]
transitions n lts = filter ((==n) . fst . fst) lts

alphabet :: LTS -> Alphabet
alphabet = nub . (map snd)

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions pr = nub (actions' pr)
  where
    actions' (Prefix iden pr) = iden : (actions' pr)
    actions' (Choice prs)     = concatMap actions' prs
    actions' _                = []

accepts :: [Id] -> [ProcessDef] -> Bool
accepts as ps = accepts' as (snd (head ps))
  where
    accepts' []     _            = True
    accepts' as     STOP         = False
    accepts' as     (Ref p)      = accepts' as (lookUp p ps)
    accepts' as     (Choice prs) = any (accepts' as) prs
    accepts' (a:as) (Prefix a' p)
      | a == a'   = accepts' as p
      | otherwise = False

------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                   -> Alphabet -> Alphabet 
                   -> StateMap 
                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), a) ((s', t'), a') a1 a2 m
  | a == a'                     = [c3]
  | (elem a' a1) && (elem a a2) = []
  | elem a' a1                  = [c1]
  | elem a a2                   = [c2]
  | otherwise                   = [c1, c2]
    where
      ss' = (lookUp (s, s') m)
      c3  = ((ss', (lookUp (t, t') m)), a)
      c1  = ((ss', (lookUp (t, s') m)), a)
      c2  = ((ss', (lookUp (s, t') m)), a')


m = [((0,0),0),((0,1),1),((1,0),2),((1,1),3)]


pruneTransitions :: [Transition] -> LTS
pruneTransitions ts = nub (visit 0 [])
  where
    visit s visited
      | elem s visited = []
      | otherwise      = concatMap f (transitions s ts)
        where
          f t@((s' ,s''), a) = t : visit s'' (s : visited)

------------------------------------------------------
-- PART IV
{-
compose :: LTS -> LTS -> LTS
compose lts lts'
  = pruneTransitions newlts
  where 
    alph        = alphabet lts
    alph'       = alphabet lts'
    newStates   = [(s,s') | s <- states lts, s' <- states lts']
    stateMap    = zip newStates [0..] 
    newlts      = [ composeTransitions t t' alph alph' stateMap |
                  (s,s') <- newStates,
                  (t,t') <- prod (transitions s lts) (transitions s' lts')]
    prod ts ts' = [(tr,tr') | tr <- ts, tr' <- ts']
-}

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]

