module Main where

import qualified Control.Monad.State.Lazy as S
import           Data.Bits                      ((.&.), (.|.), xor)
import qualified Data.ByteString.Lazy     as B
import qualified Data.Binary.Get          as G
import qualified Data.Char                as C
import           Data.Functor                   ((<&>))
import qualified Data.Maybe               as MB
import qualified Data.Map                 as M
import           Data.Word                      (Word16)

type Value        = Word16
type Address      = Value
type Register     = Value
type Instructions = [Value]
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

main :: IO ()
main =
  do bin <- B.getContents
     let vm = createVM . loadProgramIntoMemory . G.runGet bytesToWord16s $ bin
     S.evalStateT runVM vm

-- | Convert a bytestring to a program (list of little-endian 16-bit integers).
bytesToWord16s :: G.Get Instructions
bytesToWord16s =
  do empty <- G.isEmpty
     if empty
        then pure []
        else do x  <- G.getWord16le
                xs <- bytesToWord16s
                pure (x:xs)

-- | Given a program (list of Word16s), create our map of the system memory.
loadProgramIntoMemory :: Instructions -> Memory
loadProgramIntoMemory = M.fromList . zip [0..]

-- | Create a VM with an initial state. The initial memory is passed in.
createVM :: Memory -> VM
createVM memory = VM {
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

-- | Execute a program in our VM.
runVM :: VMState ()
runVM =
  do vm <- S.get
     runOpcode $ getNextInstructions vm

-- | Get the next instructions in the VM from memory, starting from the PC.
getNextInstructions :: VM -> Instructions
getNextInstructions vm =
  case M.lookup (pc vm) (memory vm) of
    Just x  -> x : getNextInstructions (vm { pc = pc vm + 1})
    Nothing -> []

-- | Execute an opcode of our program. Returns a state function parametrized by the next
-- instruction(s).
runOpcode :: [Value] -> VMState ()
-- Opcode 0:  Halt the VM.
runOpcode (0:_) =
  do pure ()
-- Opcode 1:  Set register.
runOpcode (1:x:y:_) =
  do getValue y >>= setRegister x
     incrPCAndRun 3
-- Opcode 2:  Push stack.
runOpcode (2:x:_) =
  do getValue x >>= pushStack
     incrPCAndRun 2
-- Opcode 3:  Pop stack.
runOpcode (3:x:_) =
  do popStack >>= setRegister x
     incrPCAndRun 2
-- Opcode 4:  EQ comparison.
runOpcode (4:x:y:z:_) =
  do arg1 <- getValue y
     arg2 <- getValue z
     setRegister x (if arg1 == arg2 then 1 else 0)
     incrPCAndRun 4
-- Opcode 5:  GT comparison.
runOpcode (5:x:y:z:_) =
  do arg1 <- getValue y
     arg2 <- getValue z
     setRegister x (if arg1 > arg2 then 1 else 0)
     incrPCAndRun 4
-- Opcode 6:  Jump.
runOpcode (6:x:_) =
  do getValue x >>= setPCAndRun
-- Opcode 7:  Jump non-zero.
runOpcode (7:x:y:_) =
  do val <- getValue x
     if val == 0
        then incrPCAndRun 3
        else setPCAndRun y
-- Opcode 8:  Jump zero.
runOpcode (8:x:y:_) =
  do val <- getValue x
     if val == 0
        then setPCAndRun y
        else incrPCAndRun 3
-- Opcode 9:  Addition.
runOpcode (9:x:y:z:_) =
  do arg1 <- getValue y
     arg2 <- getValue z
     setRegister x $ (arg1 + arg2) `mod` 32768
     incrPCAndRun 4
-- Opcode 10: Multiplication.
runOpcode (10:x:y:z:_) =
  do arg1 <- getValue y
     arg2 <- getValue z
     setRegister x $ (arg1 * arg2) `mod` 32768
     incrPCAndRun 4
-- Opcode 11: Mod/Remainder.
runOpcode (11:x:y:z:_) =
  do arg1 <- getValue y
     arg2 <- getValue z
     setRegister x $ (arg1 `rem` arg2) `mod` 32768
     incrPCAndRun 4
-- Opcode 12: Bitwise and operator.
runOpcode (12:x:y:z:_) =
  do arg1 <- getValue y
     arg2 <- getValue z
     setRegister x (arg1 .&. arg2)
     incrPCAndRun 4
-- Opcode 13: Bitwise or operator.
runOpcode (13:x:y:z:_) =
  do arg1 <- getValue y
     arg2 <- getValue z
     setRegister x (arg1 .|. arg2)
     incrPCAndRun 4
-- Opcode 14: Bitwise not/inverse operator.
runOpcode (14:x:y:_) =
  do arg1 <- getValue y
     setRegister x (arg1 `xor` 0x7FFF)
     incrPCAndRun 3
-- Opcode 15: Read memory from 2nd arg and write to 1st arg.
runOpcode (15:x:y:_) =
  do getValue y >>= readMemory >>= setRegister x
     incrPCAndRun 3
-- Opcode 16: Get value of 2nd arg and write to memory at 1st arg.
runOpcode (16:x:y:_) =
  do addr <- getValue x
     getValue y >>= setMemory addr
     incrPCAndRun 3
-- Opcode 17: Call.
runOpcode (17:x:_) =
  do pc <- getPC
     pushStack $ pc + 2
     getValue x >>= setPCAndRun
-- Opcode 18: Return.
runOpcode (18:_) =
  do popStack >>= setPCAndRun
-- Opcode 19: Print char.
runOpcode (19:x:_) =
  do getValue x >>= S.liftIO . printChar
     incrPCAndRun 2
-- Opcode 20: Input.
runOpcode (20:x:_) =
  do S.liftIO getChar >>= setRegister x . fromIntegral . C.ord
     incrPCAndRun 2
-- Opcode 21: No operation.
runOpcode (21:_) =
  do incrPCAndRun 1
-- Unknown opcode: Error!
runOpcode (x:xs) =
  do error $ "Found unknown opcode "
             <> show x
             <> " with arguments "
             <> show (take 3 xs)
-- No opcode: Error!
runOpcode [] =
  do error "Instruction list is empty but program has not halted!"

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

getPC :: VMState Address
getPC =
  do S.get <&> pc

-- | Increment the program counter by `x` and run the new operation.
incrPCAndRun :: Value -> VMState ()
incrPCAndRun x =
  do vm <- S.get
     S.put $ vm { pc = pc vm + x }
     runVM

-- | Set the program counter to `x` and run the new operation.
setPCAndRun :: Address -> VMState ()
setPCAndRun x =
  do vm <- S.get
     S.put $ vm { pc = x }
     runVM

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

-- | Print a 16-bit word's ASCII character value.
printChar :: Value -> IO ()
printChar = putChar . C.chr . fromIntegral
