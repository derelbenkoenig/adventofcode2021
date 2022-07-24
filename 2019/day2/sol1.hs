import Data.Array.IArray
import Data.List
import Data.Maybe

solve :: Machine -> Int
solve startingMachine = arr ! 0 where
    Machine arr _ = last $ machineStates (restoreState startingMachine)


data Command = CmdAdd | CmdMul | CmdHalt

commandFromInt n = case n of
    1 -> Just CmdAdd
    2 -> Just CmdMul
    99 -> Just CmdHalt
    otherwise -> Nothing

data MachineState = Running Int | Halted
    deriving Show

data Machine = Machine (Array Int Int) MachineState
    deriving Show

nextState :: Machine -> Maybe Machine
nextState machine@(Machine arr st) = case st of
    Halted -> Nothing
    Running pos -> performOp arr pos

performOp arr pos = go <$> (commandFromInt (arr ! pos)) where
    doBinOp op = let a1 = arr ! (arr ! (pos + 1))
                     a2 = arr ! (arr ! (pos + 2))
                     dest = arr ! (pos + 3)
                     in Machine (arr // [(dest, op a1 a2)]) (Running (pos + 4))
    go cmd = case cmd of
        CmdAdd -> doBinOp (+)
        CmdMul -> doBinOp (*)
        CmdHalt -> Machine arr Halted

takeWhileJust xs = case xs of
    [] -> []
    Nothing:_ -> []
    (Just x):xs' -> x:(takeWhileJust xs')

machineStates machine = takeWhileJust $ iterate (>>= nextState) (Just machine)

splitBy c cs = case dropWhile ((==) c) cs of
    [] -> []
    xs -> word1 : splitBy c rest where
        (word1, rest) = span ((/=) c) xs

restoreState (Machine arr s) = Machine (arr // [(1, 12), (2, 2)]) s

readInput :: String -> Machine
readInput input = Machine arr (Running 0) where
    arr = listArray (0, length xs - 1) xs
    xs = map read $ splitBy ',' input

main :: IO ()
main = interact ((++ "\n") . show . solve . readInput)
