data Instruction = Add | Mul | Pop deriving (Eq,Ord,Show)
type Stack = [Int]
type SMProg = [Instruction]

-- return combinations of instructions that produce a maximal result on a given input stack
findMaxReducers :: Stack -> [SMProg]
findMaxReducers stack = [ prog | prog <- instCombos , head (evalInst stack prog) == maxResult stack instCombos ]
  where instCombos = allInstCombos ((length stack)-1)

-- find the maximum result that given instruction sequences can produce on a stack
maxResult :: Stack -> [SMProg] -> Int
maxResult stack progs = maximum [ head (evalInst stack prog) | prog <- progs ]

-- return all possible instruction sequences of a given length
allInstCombos :: Int -> [SMProg]
allInstCombos 0 = [[]]
allInstCombos n = map ([Add]++) (allInstCombos (n-1)) ++ map ([Mul]++) (allInstCombos (n-1)) ++ map ([Pop]++) (allInstCombos (n-1))

-- evaluate all the instructions in an instruction list on a stack
evalInst :: Stack -> SMProg -> Stack
evalInst [] prog         = error "insufficient stack"
evalInst [x] (Add:prog)  = error "insufficient stack"
evalInst [x] (Mul:prog)  = error "insufficient stack"
evalInst stack []        = stack
evalInst stack (x:prog) 
  | x == Add = evalInst (addInst stack) prog
  | x == Mul = evalInst (mulInst stack) prog
  | x == Pop = evalInst (popInst stack) prog

-- evaluate add instruction 
addInst :: Stack -> Stack
addInst (x:y:stack) = x+y : stack

-- evaluate multiply instruction 
mulInst :: Stack -> Stack
mulInst (x:y:stack) = x*y : stack

-- evaluate pop instruction 
popInst :: Stack -> Stack
popInst (x:stack) = stack