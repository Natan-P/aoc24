module Types (ToRun(..), ifFst, ifSnd) where

data ToRun = ToRun {fstDay :: Bool, sndDay :: Bool}

ifFst :: ToRun -> a -> a
ifFst runs x = if fstDay runs then x else undefined
ifSnd :: ToRun -> a -> a
ifSnd runs x = if sndDay runs then x else undefined
