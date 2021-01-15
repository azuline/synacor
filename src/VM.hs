module VM where

import qualified Control.Monad            as M
import qualified Control.Monad.State.Lazy as S
import           Data.Bits                      ((.&.), (.|.), xor)
import qualified Data.Char                as C

import qualified VMState                  as V

-- | Execute a program in our VM.
run :: V.VMState ()
run =
  do ops <- V.nextInstructions
     case ops of
       (0 :_   ) -> pure ()
       (1 :args) -> set  args >> continue 3
       (2 :args) -> push args >> continue 2
       (3 :args) -> pop  args >> continue 2
       (4 :args) -> eq   args >> continue 4
       (5 :args) -> gt   args >> continue 4
       (6 :args) -> jmp  args >> continue 0
       (7 :args) -> jt   args >> continue 0
       (8 :args) -> jf   args >> continue 0
       (9 :args) -> add  args >> continue 4
       (10:args) -> mult args >> continue 4
       (11:args) -> mod' args >> continue 4
       (12:args) -> and' args >> continue 4
       (13:args) -> or'  args >> continue 4
       (14:args) -> not' args >> continue 3
       (15:args) -> rmem args >> continue 3
       (16:args) -> wmem args >> continue 3
       (17:args) -> call args >> continue 0
       (18:args) -> ret  args >> continue 0
       (19:args) -> out  args >> continue 2
       (20:args) -> in'  args >> continue 2
       (21:args) -> noop args >> continue 1
       (x :xs  ) -> error $ "Found unknown opcode "
                            <> show x
                            <> " with arguments "
                            <> show (take 3 xs)
       []        -> error "Instruction list is empty but program has not halted!"

-- | Increment the PC by the provided value and continue execution.
continue :: V.Value -> V.VMState ()
continue x = V.incrPC x >> run

-- | Opcode 1:  Set register.
set :: [V.Value] -> V.VMState ()
set (x:y:_) = V.getValue y >>= V.setRegister x

-- | Opcode 2:  Push stack.
push :: [V.Value] -> V.VMState ()
push (x:_) = V.getValue x >>= V.pushStack

-- | Opcode 3:  Pop stack.
pop :: [V.Value] -> V.VMState ()
pop (x:_) = V.popStack >>= V.setRegister x

-- | Opcode 4:  EQ comparison.
eq :: [V.Value] -> V.VMState ()
eq (x:y:z:_) =
  do a1 <- V.getValue y
     a2 <- V.getValue z
     V.setRegister x (if a1 == a2 then 1 else 0)

-- | Opcode 5:  GT comparison.
gt :: [V.Value] -> V.VMState ()
gt (x:y:z:_) =
  do a1 <- V.getValue y
     a2 <- V.getValue z
     V.setRegister x (if a1 > a2 then 1 else 0)

-- | Opcode 6:  Jump.
jmp :: [V.Value] -> V.VMState ()
jmp (x:_) = V.getValue x >>= V.setPC

-- | Opcode 7:  Jump non-zero.
jt :: [V.Value] -> V.VMState ()
jt (x:y:_) =
  do val <- V.getValue x
     if val == 0
        then V.incrPC 3
        else V.setPC y

-- | Opcode 8:  Jump zero.
jf :: [V.Value] -> V.VMState ()
jf (x:y:_) =
  do val <- V.getValue x
     if val == 0
        then V.setPC y
        else V.incrPC 3

-- | Opcode 9:  Addition.
add :: [V.Value] -> V.VMState ()
add (x:y:z:_) =
  do a1 <- V.getValue y
     a2 <- V.getValue z
     V.setRegister x $ (a1 + a2) `mod` 32768

-- | Opcode 10: Multiplication.
mult :: [V.Value] -> V.VMState ()
mult (x:y:z:_) =
  do a1 <- V.getValue y
     a2 <- V.getValue z
     V.setRegister x $ (a1 * a2) `mod` 32768

-- | Opcode 11: Mod/Remainder.
mod' :: [V.Value] -> V.VMState ()
mod' (x:y:z:_) =
  do a1 <- V.getValue y
     a2 <- V.getValue z
     V.setRegister x $ (a1 `rem` a2) `mod` 32768

-- | Opcode 12: Bitwise and operator.
and' :: [V.Value] -> V.VMState ()
and' (x:y:z:_) =
  do a1 <- V.getValue y
     a2 <- V.getValue z
     V.setRegister x (a1 .&. a2)

-- | Opcode 13: Bitwise or operator.
or' :: [V.Value] -> V.VMState ()
or' (x:y:z:_) =
  do a1 <- V.getValue y
     a2 <- V.getValue z
     V.setRegister x (a1 .|. a2)

-- | Opcode 14: Bitwise not/inverse operator.
not' :: [V.Value] -> V.VMState ()
not' (x:y:_) = V.getValue y >>= V.setRegister x . (`xor` 0x7FFF)

-- | Opcode 15: Read memory from 2nd arg and write to 1st arg.
rmem :: [V.Value] -> V.VMState ()
rmem (x:y:_) = V.getValue y >>= V.readMemory >>= V.setRegister x

-- | Opcode 16: Get value of 2nd arg and write to memory at 1st arg.
wmem :: [V.Value] -> V.VMState ()
wmem (x:y:_) = M.join $ V.setMemory <$> V.getValue x <*> V.getValue y

-- | Opcode 17: Call.
call :: [V.Value] -> V.VMState ()
call (x:_) =
  do V.getPC >>= V.pushStack . (+2)
     V.getValue x >>= V.setPC

-- | Opcode 18: Return.
ret :: [V.Value] -> V.VMState ()
ret _ = V.popStack >>= V.setPC

-- | Opcode 19: Print char.
out :: [V.Value] -> V.VMState ()
out (x:_) = V.getValue x >>= S.liftIO . putChar . C.chr . fromIntegral

-- | Opcode 20: Input.
in' :: [V.Value] -> V.VMState ()
in' (x:_) = S.liftIO getChar >>= V.setRegister x . fromIntegral . C.ord

-- | Opcode 21: No operation.
noop :: [V.Value] -> V.VMState ()
noop _ = pure ()
