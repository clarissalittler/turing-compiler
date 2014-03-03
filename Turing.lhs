> module Turing where
>

Here I want to try and develop clean, simple, code about executing a Turing machine for purposes of showing students how Turing machines work and how they can be programmed.

In this model, which will be more about simplicity than efficiency. We'll represent the tape as an infinitely long list, and we'll use a zipper construction to describe the current focus of the tape head. Remember, that a Turing machine is really just a special kind of state machine. For simplicity sake, we'll take all the positive numbers up to some cutoff 'n' as our states. This is a parameter of the Turing machine. For our alphabet, we'll leave it parameterized by a type variable 'a', and since spaces on a tape can be blank let's just represent each element of the tape as a Maybe 'a'. Sound good?

As a digression, if we were using dependent types we could represent our set of states more clearly as a Fin n type, that is the type of finite sets. This would allow us to avoid the runtime checks you'll see in our code to make sure that states are valid.

We'll represent our directions with a simple algebraic datatype for clarity. Of course we could just use booleans, but this is I think a little simpler.

> blankTape = repeat Nothing

> data Move = L | R
> type TapeState a = ([Maybe a],Maybe a,[Maybe a])

> type Delta a = Int -> Maybe a -> (Int,Maybe a,Move)

Now a few words will need to be said about how we're representing the current position of the tape. For ease of defining configurations and for sake of following the book, we'll use the one-sided tape that Sipser does. Now, in the triple ([Maybe a], Maybe a, [Maybe a]) the first component is the (reverse) of the component of the tape to the left of the current position, the second componenent is our current tape head, and the third component is the right of the tape.

> updateTape :: Maybe a -> Move -> TapeState a -> TapeState a
> updateTape m L (l:ls,_,rs) = (ls,l,m:rs) 
> updateTape m L ([],_,rs) = ([],m,rs)
> updateTape m R (ls,_,r:rs) =(m:ls,r,rs)
> updateTape m R (_,_,[]) = error "This shouldn't ever happen"

We could also include a starting state in the TMConfig, but without loss of generality let's just let 0 always be the starting state.

> data TMConfig a = TM {initTape :: [Maybe a], delta :: Delta a, states :: Int, accept :: Int, reject :: Int}

> stepTM :: Delta a -> Int -> TapeState a -> (Int,TapeState a)
> stepTM d q (ls,c,rs) = let (q',c',m) = d q c                             
>                            ts = updateTape c' m (ls,c,rs)
>                        in (q',ts)
>
> runTM :: TMConfig a -> TapeState a
> runTM tm = runTM' tm (toTapeState (initTape tm)) 0
>     where toTapeState (x : xs) = ([],x,xs)
> runTM' :: TMConfig a -> TapeState a -> Int -> TapeState a
> runTM' tm tp q | q == (accept tm) = tp 
>                | q == (reject tm) = error "The turing machine failed"
>                | otherwise = let (q',tp') = stepTM (delta tm) q tp
>                              in runTM' tm tp' q'