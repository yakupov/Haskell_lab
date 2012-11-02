{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State (\realWorld ->
		let currStdIn = stdIn realWorld
		    newWorld = RealWorld (tail currStdIn) (stdOut realWorld) (exitCode realWorld)
		in (newWorld, head currStdIn)
	)

putNat :: Nat -> IO ()
putNat x = State (\realWorld ->
		let newWorld = RealWorld (stdIn realWorld) (Cons x (stdOut realWorld)) (exitCode realWorld)
		in (newWorld, ())
	)

setExitCode :: Nat -> IO ()
setExitCode x = State (\realWorld ->
		let newWorld = RealWorld (stdIn realWorld) (stdOut realWorld) x
		in (newWorld, ())
	)
