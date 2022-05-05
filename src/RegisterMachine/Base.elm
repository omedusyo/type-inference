module RegisterMachine.Base exposing
    ( Constant(..)
    , Instruction(..)
    , InstructionAddress
    , Label
    , MemoryAddress
    , OperationApplication(..)
    , OperationArgument(..)
    , OperationArity
    , OperationName
    , Register
    , Value(..)
    )

-- sequence of instructions should start with a label


type alias Label =
    String


type alias Register =
    String


type alias MemoryAddress =
    Int


type Value
    = ConstantValue Constant
    | Pair MemoryAddress
    | InstructionAddress InstructionAddress
    | Uninitialized
    | Collected MemoryAddress


type Constant
    = Num Int
    | Nil


type alias InstructionAddress =
    Int


type alias OperationName =
    String



-- TODO: Is this going to be used somewhere? Atleast for basic type checking?


type alias OperationArity =
    Int



-- TODO: Change the third argument to `Operation` to from `Register` to `OperationArgument`
-- TODO: What about labels as arguments to operations?


type OperationArgument
    = Register Register
    | Constant Constant


type OperationApplication
    = Operation OperationName (List OperationArgument)


type
    Instruction
    -- b <- $a
    -- b <- :foo
    -- a <- op($x, $y)
    -- a <- constant
    -- jump :foo
    -- jump $a
    -- if $a jump :foo
    -- if $a jump $b // is a register that contains a labelb
    -- halt
    -- push $a
    -- a <- stack
    --
    -- p <- pair $a  $b
    -- a <- first $p
    -- b <- second $p
    -- set-first $p $a
    -- set-second $p $b
    = -- assignment
      AssignRegister Register Register
    | AssignLabel Register Label
    | AssignOperation Register OperationApplication
    | AssignConstant Register Constant
      -- jumping
    | JumpToLabel Label
    | JumpToLabelAtRegister Register
    | JumpToLabelIf Register Label
    | JumpToLabelAtRegisterIf Register Register -- first register is the test register, second register is the register containing the label
    | Halt
      -- stack
    | PushRegister Register
    | PushConstant Constant
    | PushLabel Label
    | Pop Register
      -- calling procedure
    | AssignCallAtLabel Register Label
    | AssignCallAtRegister Register Register
      -- memory
    | ConstructPair Register OperationArgument OperationArgument
    | First Register Register
    | Second Register Register
    | SetFirst Register OperationArgument
    | SetSecond Register OperationArgument
