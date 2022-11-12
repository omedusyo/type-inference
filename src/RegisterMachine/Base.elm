module RegisterMachine.Base exposing
    ( AssignCallAtLabelInput
    , AssignCallAtRegisterInput
    , AssignConstantInput
    , AssignLabelInput
    , AssignOperationInput
    , AssignRegisterInput
    , Constant(..)
    , ConstructPairInput
    , DualFirstInput
    , DualSecondInput
    , DualSetFirstInput
    , DualSetSecondInput
    , FirstInput
    , HaltInput
    , Instruction(..)
    , InstructionPointer
    , JumpToLabelAtRegisterIfInput
    , JumpToLabelAtRegisterInput
    , JumpToLabelIfInput
    , JumpToLabelInput
    , Label
    , MarkAsMovedInput
    , MemoryPointer
    , MoveToDualInput
    , OperationApplication
    , OperationArgument(..)
    , OperationArity
    , OperationName
    , PopInput
    , PushConstantInput
    , PushLabelInput
    , PushRegisterInput
    , Register
    , SecondInput
    , SetFirstInput
    , SetSecondInput
    , SwapMemoryInput
    , Value(..)
    )

-- sequence of instructions should start with a label


type alias Label =
    String


type alias Register =
    String


type alias MemoryPointer =
    Int


type Value
    = ConstantValue Constant
    | Pair MemoryPointer
    | InstructionPointer InstructionPointer
    | Uninitialized
    | Moved


type Constant
    = Num Int
    | Nil


type alias InstructionPointer =
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


type alias OperationApplication =
    { name : OperationName, arguments : List OperationArgument }


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
    -- push constant
    -- push $a
    -- a <- stack
    --
    -- p <- pair $a  $b
    -- a <- first $p
    -- b <- second $p
    -- set-first $p $a
    -- set-second $p $b
    --
    -- q <- move-to-dual $p // p is a register holding an pointer in current memory.
    --                      // This will move the pair pointed by p to the other memory.
    --                      // Register q will hold the new pointer.
    --                      // The
    -- swap-memory          // Switches the roles of memories.
    = -- assignment
      AssignRegister AssignRegisterInput
    | AssignLabel AssignLabelInput
    | AssignOperation AssignOperationInput
    | AssignConstant AssignConstantInput
      -- jumping
    | JumpToLabel JumpToLabelInput
    | JumpToLabelAtRegister JumpToLabelAtRegisterInput
    | JumpToLabelIf JumpToLabelIfInput
    | JumpToLabelAtRegisterIf JumpToLabelAtRegisterIfInput
    | Halt HaltInput
      -- stack
    | PushRegister PushRegisterInput
    | PushConstant PushConstantInput
    | PushLabel PushLabelInput
    | Pop PopInput
      -- calling procedure
    | AssignCallAtLabel AssignCallAtLabelInput
    | AssignCallAtRegister AssignCallAtRegisterInput
      -- memory
    | ConstructPair ConstructPairInput
    | First FirstInput
    | Second SecondInput
    | SetFirst SetFirstInput
    | SetSecond SetSecondInput
      -- dual memory
    | DualFirst DualFirstInput
    | DualSecond DualSecondInput
    | DualSetFirst DualSetFirstInput
    | DualSetSecond DualSetSecondInput
      -- garbage collection
    | MoveToDual MoveToDualInput
    | MarkAsMoved MarkAsMovedInput
    | SwapMemory SwapMemoryInput



-- assignment


type alias AssignRegisterInput =
    { targetRegister : Register, sourceRegister : Register }


type alias AssignLabelInput =
    { targetRegister : Register, label : Label }


type alias AssignOperationInput =
    { targetRegister : Register, operationApplication : OperationApplication }


type alias AssignConstantInput =
    { targetRegister : Register, constant : Constant }



-- jumping


type alias JumpToLabelInput =
    { label : Label }


type alias JumpToLabelAtRegisterInput =
    { labelRegister : Register }


type alias JumpToLabelIfInput =
    { testRegister : Register, label : Label }


type alias JumpToLabelAtRegisterIfInput =
    { testRegister : Register, labelRegister : Register }


type alias HaltInput =
    {}



--   -- stack


type alias PushRegisterInput =
    { sourceRegister : Register }


type alias PushConstantInput =
    { constant : Constant }


type alias PushLabelInput =
    { label : Label }


type alias PopInput =
    { targetRegister : Register }



--   -- calling procedure


type alias AssignCallAtLabelInput =
    { targetRegister : Register, label : Label }


type alias AssignCallAtRegisterInput =
    { targetRegister : Register, labelRegister : Register }



-- memory


type alias ConstructPairInput =
    { targetRegister : Register, operationArgument0 : OperationArgument, operationArgument1 : OperationArgument }


type alias FirstInput =
    { targetRegister : Register, sourceRegister : Register }


type alias SecondInput =
    { targetRegister : Register, sourceRegister : Register }


type alias SetFirstInput =
    { targetRegister : Register, operationArgument : OperationArgument }


type alias SetSecondInput =
    { targetRegister : Register, operationArgument : OperationArgument }



-- dual memory


type alias DualFirstInput =
    { targetRegister : Register, sourceRegister : Register }


type alias DualSecondInput =
    { targetRegister : Register, sourceRegister : Register }


type alias DualSetFirstInput =
    { targetRegister : Register, operationArgument : OperationArgument }


type alias DualSetSecondInput =
    { targetRegister : Register, operationArgument : OperationArgument }



-- garbage collection


type alias MoveToDualInput =
    { targetRegister : Register, sourceRegister : Register }


type alias MarkAsMovedInput =
    -- p <- move-to-dual $q   :=   MoveToDual "p" "q"
    { toBeCollectedFromRegister : Register, referenceToDualMemoryRegister : Register }


type alias SwapMemoryInput =
    -- MarkAsMoved memory_pointer_to_be_collected memory_pointer_to_dual_memory
    {}
