module VMState where

import qualified Control.Monad.State.Lazy as S
import           Data.Functor                   ((<&>))
import qualified Data.Maybe               as MB
import qualified Data.Map                 as M
import           Data.Word                      (Word16)

type Value        = Word16
type Address      = Value
type Register     = Value
type Memory       = M.Map Address Value

data VM = VM {
    memory  :: Memory
  , stack   :: [Value]
  , pc      :: Address
  , r0      :: Value
  , r1      :: Value
  , r2      :: Value
  , r3      :: Value
  , r4      :: Value
  , r5      :: Value
  , r6      :: Value
  , r7      :: Value
  }

type VMState = S.StateT VM IO

-- | Create a VM with an initial state. The initial memory is passed in.
create :: Memory -> VM
create memory = VM {
    memory  = memory
  , stack   = []
  , pc      = 0
  , r0      = 0
  , r1      = 0
  , r2      = 0
  , r3      = 0
  , r4      = 0
  , r5      = 0
  , r6      = 0
  , r7      = 0
  }

-- | Get the next instructions in the VM from memory, starting from the PC.
nextInstructions :: VMState [Value]
nextInstructions = S.get <&> go
  where
    go :: VM -> [Value]
    go vm = case M.lookup (pc vm) (memory vm) of
              Just x  -> x : go (vm { pc = pc vm + 1})
              Nothing -> []

-- | Evaluate a value as a literal or register value.
getValue :: Value -> VMState Value
getValue x
  | x <= 32767 = pure x
  | otherwise  = readRegister x

-- | Read a value from a memory address.
readMemory :: Address -> VMState Value
readMemory x
  | x <= 32767 = do S.get <&> MB.fromMaybe 0 . M.lookup x . memory
  | otherwise  = error "Invalid memory address (read)."

-- | Set a memory address to a value.
setMemory :: Address -> Value -> VMState ()
setMemory x v
  | x <= 32767 = do vm <- S.get
                    S.put $ vm { memory = M.insert x v $ memory vm }
  | otherwise  = error "Invalid memory address (write)."

-- | Get the current PC.
getPC :: VMState Address
getPC = S.get <&> pc

-- | Increment the program counter by `x` and run the new operation.
incrPC :: Value -> VMState ()
incrPC x =
  do vm <- S.get
     S.put $ vm { pc = pc vm + x }

-- | Set the program counter to `x` and run the new operation.
setPC :: Address -> VMState ()
setPC x =
  do vm <- S.get
     S.put $ vm { pc = x }

-- | Push a value onto the stack.
pushStack :: Value -> VMState ()
pushStack x =
  do vm <- S.get
     S.put $ vm { stack = x : stack vm }

-- | Pop a value off the stack.
popStack :: VMState Value
popStack =
  do vm <- S.get
     case stack vm of
       []   -> error "Cannot pop off empty stack!"
       x:xs -> do S.put $ vm { stack = xs }
                  pure x

-- | Get the value stored in a register.
readRegister :: Register -> VMState Value
readRegister r =
  do vm <- S.get
     case r - 32768 of
       0 -> pure $ r0 vm
       1 -> pure $ r1 vm
       2 -> pure $ r2 vm
       3 -> pure $ r3 vm
       4 -> pure $ r4 vm
       5 -> pure $ r5 vm
       6 -> pure $ r6 vm
       7 -> pure $ r7 vm
       _ -> error $ "Invalid register " <> show r

-- | Set a register to a value.
setRegister :: Register -> Value -> VMState ()
setRegister r v =
  do vm <- S.get
     case r - 32768 of
       0 -> S.put $ vm { r0 = v }
       1 -> S.put $ vm { r1 = v }
       2 -> S.put $ vm { r2 = v }
       3 -> S.put $ vm { r3 = v }
       4 -> S.put $ vm { r4 = v }
       5 -> S.put $ vm { r5 = v }
       6 -> S.put $ vm { r6 = v }
       7 -> S.put $ vm { r7 = v }
       _ -> error $ "Invalid register " <> show r
