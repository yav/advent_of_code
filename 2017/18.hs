import Data.Char
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector


newtype Reg = Reg Char
  deriving (Show, Eq, Ord)

data Expr   = Lit Integer | Var Reg
  deriving Show

data Instruction =
    Snd Expr
  | Set Reg Expr
  | Add Reg Expr
  | Mul Reg Expr
  | Mod Reg Expr
  | Rcv Reg
  | Jgz Expr Expr -- ^ test, offset
  deriving Show



--------------------------------------------------------------------------------
-- Parser

data Tok = TokKW String | TokReg Reg | TokInt Integer
  deriving Show

token :: String -> Tok
token str
  | [(n,"")] <- reads str = TokInt n
  | [c] <- str, isAlpha c = TokReg (Reg c)
  | otherwise             = TokKW str

tokenize :: String -> [Tok]
tokenize = map token . words

parseExpr :: [Tok] -> Maybe (Expr, [Tok])
parseExpr ts =
  case ts of
    TokReg r : more -> pure (Var r, more)
    TokInt n : more -> pure (Lit n, more)
    _               -> Nothing

parseReg :: [Tok] -> Maybe (Reg, [Tok])
parseReg ts =
  case ts of
    TokReg r : more -> pure (r, more)
    _               -> Nothing

parseInstruction :: [Tok] -> Maybe (Instruction, [Tok])
parseInstruction ts =
  msum [ unary "snd" Snd parseExpr
       , unary "rcv" Rcv parseReg
       , bin   "jgz" Jgz parseExpr parseExpr
       , binOp "set" Set
       , binOp "add" Add
       , binOp "mul" Mul
       , binOp "mod" Mod
       ]
  where
  unary x f p = case ts of
                  TokKW y : ts1 | x == y -> do (r,ts2) <- p ts1
                                               pure (f r, ts2)
                  _ -> Nothing

  binOp x f = bin x f parseReg parseExpr

  bin x f p1 p2 = case ts of
                    TokKW y : ts1 | x == y -> do (r,ts2) <- p1 ts1
                                                 (e,ts3) <- p2 ts2
                                                 pure (f r e, ts3)
                    _ -> Nothing

parseProgram :: [Tok] -> Maybe [Instruction]
parseProgram xs =
  case xs of
     [] -> Just []
     _  -> do (i,ys) <- parseInstruction xs
              is     <- parseProgram ys
              pure (i:is)

parseInput :: String -> Maybe (Vector Instruction)
parseInput = fmap Vector.fromList . parseProgram . tokenize

--------------------------------------------------------------------------------

data State = State
  { code    :: !(Vector Instruction) -- ^ Immutable
  , progId  :: !Integer              -- ^ ImmuabLe
  , regs    :: !(Map Reg Integer)
  , pc      :: !Int
  , inputs  :: !(Q Integer)
  , counter :: !Int
  }

initState :: Integer -> Vector Instruction -> State
initState pid is = State { code = is
                         , progId = pid
                         , regs = Map.singleton (Reg 'p') pid
                         , pc = 0
                         , inputs = emptyQ, counter = 0 }

readReg :: Reg -> State -> Integer
readReg r s = Map.findWithDefault 0 r (regs s)

setReg :: Reg -> Integer -> State -> State
setReg r n s = s { regs = newRegs }
  where
  oldRegs = regs s
  newRegs = if n == 0 then Map.delete r oldRegs else Map.insert r n oldRegs

bumpCounter :: State -> State
bumpCounter s = s { counter = counter s + 1 }

advancePC :: State -> State
advancePC s = s { pc = pc s + 1 }

setPC :: Integer -> State -> State
setPC n s = s { pc = fromInteger n }

addInput :: Integer -> State -> State
addInput i s = s { inputs = enQ i (inputs s) }

getInput :: State -> Maybe (Integer, State)
getInput s = do (i,q) <- deQ (inputs s)
                pure (i, s { inputs = q })

getInstr :: State -> Maybe Instruction
getInstr s = code s Vector.!? pc s


evalExpr :: Expr -> State -> Integer
evalExpr expr s =
  case expr of
    Lit n -> n
    Var r -> readReg r s

step :: State -> Maybe (Maybe Integer, State)
step s =
  do instr <- getInstr s
     case instr of
      Snd e    -> pure (Just (evalExpr e s), advancePC (bumpCounter s))
      Set r e  -> noOut (advancePC (setReg r (evalExpr e s) s))
      Add r e  -> bin (+) r e
      Mul r e  -> bin (*) r e
      Mod r e  -> bin mod r e
      Rcv r    -> do (i,s1) <- getInput s
                     noOut $ advancePC $ setReg r i s1
      Jgz test off ->
        noOut $
        if evalExpr test s > 0
           then setPC (fromIntegral (pc s) + evalExpr off s) s
           else advancePC s
  where
  noOut x   = pure (Nothing, x)
  bin f r e = noOut (advancePC (setReg r (f (readReg r s) (evalExpr e s)) s))


--------------------------------------------------------------------------------

data Q a = Q [a] [a]

emptyQ :: Q a
emptyQ = Q [] []

enQ :: a -> Q a -> Q a
enQ a (Q xs ys) = Q xs (a : ys)

deQ :: Q a -> Maybe (a, Q a)
deQ q =
  case q of
    Q [] []       -> Nothing
    Q (x : xs) ys -> Just (x, Q xs ys)
    Q [] ys       -> deQ (Q (reverse ys) [])

--------------------------------------------------------------------------------

runProgs :: State -> State -> (State,State)
runProgs s1 s2 =
  case tryStep s1 s2 of
    Nothing ->
      case tryStep s2 s1 of
        Nothing -> (s1,s2)
        Just (s2',s1') -> runProgs s2' s1'
    Just (s1',s2') -> runProgs s2' s1'
  where
  tryStep s other =
    do (mbOut,newS) <- step s
       pure $ case mbOut of
                Nothing -> (newS,other)
                Just o  -> (newS, addInput o other)

main :: IO ()
main =
  do txt <- readFile "input/18b.txt"
     case parseInput txt of
       Just is ->
         let s1 = initState 0 is
             s2 = initState 1 is
             (s1',s2') = runProgs s1 s2
         in print $ counter $ if progId s1' == 1 then s1' else s2'
       Nothing -> fail "Failed to parse input"

