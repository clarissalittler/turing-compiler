module StackInterpreter where

import Turing

{- 
  Now we come to the part of the show where we figure out how to design a TM that can emulate this stack machine. 

  We'll do this by considering there to be a code region where we'll get all the instructions. When we're done executing an instruction, we'll blank it out as a way of recording where we are in the program. For while loops, we'll have a separate section where we copy over the instructions and run them until they're done. We'll also have a portion of the tape that will act as our data stack. This will be the portion of tape that extends beyond the code section(s). The stack will grow to the right, and popping from the stack will mean erasing the entry from the right most side. Oh, shoot, what about the variables? Well those will go between the code and the stack.

  Now, we'll be cheating a bit in that we won't be writing our transition function as one giant case statement! No, we'll be shortcutting things a bit and using >, <, and = instead. Now, given that we could implement all those things /in/ a case statement I think that's completely okay.

-}

{- so we'll be defining our stackInterpreter as a Delta Int -}

stackInterpreter :: Delta Int
{- this is basically the "prototype" for the rest of our operations
stackInterpreter q w = (q',w',d)
-}

{- state 0: so our first step is going to be reading the next instruction, this means we go as far left as we can until we get to the end of the tape, then go right to read the next instruction. Let's just call this sate 0, because we can. -}
stackInterpreter 0 (Just x) = (0, Just x, L)
stackInterpreter 0 Nothing = (1,Nothing, R)

{- state 1: this will be the bulk of our control flow, the code that reads each instruction and decides what to do with it 

   golly, I guess I need to make choices now about how I'm encoding things. Let's assume everywhere that we're using the full 32 (ish) bit integers in our Turing machine because /why not/

   Since the actual /data/ is going to only be 8bits let's let the 9th bit be what decides which instruction it is.
-} 