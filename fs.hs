--module Fs where

import Data.Tree

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return a = State (\s -> (s, a))
    (>>=) someStateS f = State  (\s ->  let (s', a) = runState someStateS s
					in runState (f a) s'
				)

if' True a b = a
if' False a b = b


data FS a = Nil | Dir String [FS a] | File String a -- Empty | Dir name subdirs | File filename filedata

testData = Dir "root" [(Dir "sub1" [File "HAHAHAHAHAHHAHAH" ""]), (File "someFile" "x")]
--testData = Dir "sub1" [File "HAHAHAHAHAHHAHAH" ""]

ls :: State ([FS a], FS a) String 
ls = State (lss)

getName :: FS a -> String
getName Nil = "Nil"
getName (File name _) = name
getName (Dir name _) = name

lsf :: FS a -> String
lsf Nil = getName Nil
lsf n@(File name _) = (getName n)
lsf n@(Dir name subs) = (getName n) ++ " : " ++ (lsd subs)
	where
	lsd :: [FS a] -> String
	lsd [] = ""
	lsd (x:xs) = (getName x) ++ " " ++ (lsd xs)

lss :: ([FS a], FS a) -> (([FS a], FS a), String)
lss (hist, curr) = ((hist, curr), (lsf curr))


cdf :: [FS a] -> FS a -> String -> ([FS a], FS a)
cdf (h:hs) _ ".." = (hs, h)
cdf hist curr "." = (hist, curr)
cdf hist curr@(Dir currName subs) targetName = (curr:hist, newDir)
	where
	newDir = getDirByName subs targetName
		where
		getDirByName :: [FS a] -> String -> FS a
		getDirByName [] _ = Nil
		getDirByName (d:ds) name = if' ((getName d) == name) d (getDirByName ds name)

cdf2 :: String -> ([FS a], FS a) -> (([FS a], FS a), String)
cdf2 name (hist, curr) = ((cdf hist curr name), getName curr)

cd :: String -> (State ([FS a], FS a) String)
cd s = State (cdf2 s)


someExtract :: (([FS a], FS a), String) -> FS a
someExtract ((a, b), s) = b

lsExtract :: (([FS a], FS a), String) -> String
lsExtract ((a, b), s) = s


--main = print (lsf testData)
--main = print (lsf (someExtract (cdf [] testData "sub1")))

--s = (cd "sub1")
--dat = ([], testData)
--x = runState s dat
--main = putStrLn (lsf (someExtract (runState s dat)))

{--
cState = ls
dir = ([], testData)
main = putStrLn (lsExtract(runState cState dir))
--}

cState = runState (do 
	ls
	ls
	cd "sub1"
	ls
	)

str = lsExtract(cState ([], testData))

main = putStrLn str

