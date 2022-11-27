module Ui.Tab.PublicRegisterMachineMsg exposing (..)

-- These are public messages that are understood by the Runtime module.
-- You can trigger them from the parent of the Runtime module.
--
-- Currently this is used by the `Editor` module. The common parent of `Editor` and `Runtime` is the `RegisterMachine`.
-- Note that `RegisterMachine` gives `Editor` the ability to trigger `Runtime`'s public messages.


type PublicRegisterMachineMsg
    = Step
    | StepUntilNextJump
    | StepUntilHalted
    | Reset
