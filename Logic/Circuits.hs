module Logic.Circuits where

import Prelude hiding (not, and, or)
import Logic.Types
import Logic.Gates

halfAdder :: Bit -> Bit -> [Bit]
halfAdder x y = (xor x y) : (and x y) : []


fullAdder :: Bit -> Bit -> Bit -> [Bit]
fullAdder x y cin =
    let
        [s1, c1]   = halfAdder x y
        [sout, c2] = halfAdder s1 cin
        cout       = or c1 c2
    in
        [sout, cout]


rippleAdd :: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> [Bit]
rippleAdd cin x1 y1 x2 y2 x3 y3 x4 y4 = 
    let
        [s1, c1]   = fullAdder x1 y1 cin
        [s2, c2]   = fullAdder x2 y2 c1
        [s3, c3]   = fullAdder x3 y3 c2
        [s4, cout] = fullAdder x4 y4 c3
    in
        [s1, s2, s3, s4, cout]


rippleAddN :: Bit -> [Bit] -> [Bit] -> [Bit]
rippleAddN c [] (y:ys)     = rippleAddN c [Zero] (y:ys)
rippleAddN c (x:xs) []     = rippleAddN c (x:xs) [Zero]
rippleAddN c [] []         = [c]
rippleAddN c (x:xs) (y:ys) =
    let
        [sN, cN] = fullAdder x y c
        recurse  = rippleAddN cN xs ys
    in
        sN : recurse


addSub :: Bit -> [Bit] -> [Bit] -> [Bit]
addSub mode x y = rippleAddN mode x modifiedY
    where
        modifiedY = map (\bit -> xor bit mode) y


mux :: Bit -> Bit -> Bit -> Bit
mux x y s = 
    let
        and1 = and x $ not s
        and2 = and s y
    in
        or and1 and2


crossbar :: Bit -> Bit -> Bit -> (Bit, Bit)
crossbar x1 x2 s = 
    let
        y1 = mux x1 x2 s
        y2 = mux x2 x1 s
    in
        (y1, y2)