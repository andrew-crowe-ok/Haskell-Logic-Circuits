{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Layoutz
import Byte    ( Byte(..), byteToIntUnsigned, byteToIntSigned, int2byteSigned )
import Bit     ( Bit(..) )
import Classes ( Arithmetic(add) )
import System.IO (hSetEncoding, hSetBuffering, hSetEcho, stdout, stdin, utf8, BufferMode(..))
import System.Exit (exitFailure)
import qualified System.Console.Terminal.Size as Term (size, Window(width, height))

--------------------------------------------------------------------------------
-- 1. MODEL
--------------------------------------------------------------------------------

data AppMode = UnsignedAdd | SignedAdd deriving (Eq, Show, Enum, Bounded)

data Focus = NumA | SwA Int | NumB | SwB Int deriving (Eq, Show)

data AppModel = AppModel
    { byteA          :: Byte
    , byteB          :: Byte
    , strA           :: String
    , strB           :: String
    , mode           :: AppMode
    , focus          :: Focus
    , frame          :: Int
    }

checkOverflow :: AppMode -> Byte -> Byte -> Bool
checkOverflow UnsignedAdd a b = 
    (byteToIntUnsigned a + byteToIntUnsigned b) > 255
checkOverflow SignedAdd a b = 
    let valA = byteToIntSigned a
        valB = byteToIntSigned b
        sumVal = valA + valB
    in sumVal < -128 || sumVal > 127

getNumericValue :: AppMode -> Byte -> Int
getNumericValue UnsignedAdd b = byteToIntUnsigned b
getNumericValue SignedAdd b = byteToIntSigned b

syncFromStr :: String -> Byte
syncFromStr str = 
    case reads str :: [(Int, String)] of
        [(n, "")] -> int2byteSigned n
        _         -> int2byteSigned 0

syncFromByte :: AppMode -> Byte -> String
syncFromByte m b = show (getNumericValue m b)

setBitAt :: Int -> Bit -> Byte -> Byte
setBitAt idx newBit (Byte bits) =
    case splitAt idx bits of
        (pre, _:post) -> Byte (pre ++ newBit : post)
        _             -> Byte bits

--------------------------------------------------------------------------------
-- 2. ACTIONS
--------------------------------------------------------------------------------

data AppAction
    = TypeChar Char
    | Backspace
    | MoveCursor Int
    | AdjustValue Int
    | ToggleMode
    | AnimTick
    | Quit

--------------------------------------------------------------------------------
-- 3. UPDATE
--------------------------------------------------------------------------------

handleAction :: AppAction -> AppModel -> (AppModel, Cmd AppAction)
handleAction action model = case action of
    Quit             -> (model, CmdExit)
    AnimTick         -> (model { frame = frame model + 1 }, CmdNone)

    MoveCursor dir   ->
        let nextFocus = case (focus model, dir) of
                (NumA, -1)  -> SwA 7
                (SwA i, -1) -> if i > 0 then SwA (i-1) else NumB
                (NumB, -1)  -> SwB 7
                (SwB i, -1) -> if i > 0 then SwB (i-1) else NumA
                (NumA, 1)   -> SwB 0
                (SwB i, 1)  -> if i < 7 then SwB (i+1) else NumB
                (NumB, 1)   -> SwA 0
                (SwA i, 1)  -> if i < 7 then SwA (i+1) else NumA
                _           -> focus model
        in (model { focus = nextFocus }, CmdNone)

    AdjustValue delta ->
        let m' = case focus model of
                NumA -> let current = getNumericValue (mode model) (byteA model)
                            newVal = current + delta
                            clamped = case mode model of
                                UnsignedAdd -> max 0 (min 255 newVal)
                                SignedAdd   -> max (-128) (min 127 newVal)
                            newByte = int2byteSigned clamped
                        in model { byteA = newByte, strA = syncFromByte (mode model) newByte }
                NumB -> let current = getNumericValue (mode model) (byteB model)
                            newVal = current + delta
                            clamped = case mode model of
                                UnsignedAdd -> max 0 (min 255 newVal)
                                SignedAdd   -> max (-128) (min 127 newVal)
                            newByte = int2byteSigned clamped
                        in model { byteB = newByte, strB = syncFromByte (mode model) newByte }
                SwA i -> let targetBit = if delta > 0 then One else Zero
                             newByte = setBitAt i targetBit (byteA model)
                         in model { byteA = newByte, strA = syncFromByte (mode model) newByte }
                SwB i -> let targetBit = if delta > 0 then One else Zero
                             newByte = setBitAt i targetBit (byteB model)
                         in model { byteB = newByte, strB = syncFromByte (mode model) newByte }
        in (m', CmdNone)

    TypeChar c -> 
        let m' = case focus model of
                NumA -> let ns = strA model ++ [c] in model { strA = ns, byteA = syncFromStr ns }
                NumB -> let ns = strB model ++ [c] in model { strB = ns, byteB = syncFromStr ns }
                _ -> model
        in (m', CmdNone)
        
    Backspace -> 
        let m' = case focus model of
                NumA -> let ns = if null (strA model) then "" else init (strA model)
                        in model { strA = ns, byteA = syncFromStr ns }
                NumB -> let ns = if null (strB model) then "" else init (strB model)
                        in model { strB = ns, byteB = syncFromStr ns }
                _ -> model
        in (m', CmdNone)

    ToggleMode -> 
        let newMode = if mode model == UnsignedAdd then SignedAdd else UnsignedAdd
            m' = model { mode = newMode, strA = syncFromByte newMode (byteA model), strB = syncFromByte newMode (byteB model) }
        in (m', CmdNone)

--------------------------------------------------------------------------------
-- 4. SUBSCRIPTIONS
--------------------------------------------------------------------------------

handleSubs :: AppModel -> Sub AppAction
handleSubs _ = subBatch
    [ subKeyPress $ \key -> case key of
        KeyEscape    -> Just Quit 
        KeyChar 'm'  -> Just ToggleMode
        KeyChar c | c `elem` ("0123456789-" :: String) -> Just (TypeChar c)
        KeyBackspace -> Just Backspace
        KeyLeft      -> Just (MoveCursor 1)   
        KeyRight     -> Just (MoveCursor (-1)) 
        KeyUp        -> Just (AdjustValue 1)
        KeyDown      -> Just (AdjustValue (-1))
        _            -> Nothing
    , subEveryMs 100 AnimTick
    ]

--------------------------------------------------------------------------------
-- 5. VIEW
--------------------------------------------------------------------------------

renderBits :: Int -> [Bit] -> L
renderBits f bits = tightRow $ map draw (reverse bits)
  where
    pulse :: Double
    pulse = sin (fromIntegral f * 0.3)
    rVal  :: Int
    rVal  = round $ 177 + (78 * pulse) 
    draw One  = withColor (ColorTrue rVal 0 0) $ text "  ●   "
    draw Zero = withColor (ColorTrue 40 0 0)   $ text "  ●   " 

renderSwitches :: Int -> Focus -> String -> [Bit] -> L
renderSwitches f foc target bits = 
    let activeIdx = case foc of { SwA i | target == "A" -> i; SwB i | target == "B" -> i; _ -> -1 }
    in tightRow [ draw activeIdx i b | (i, b) <- zip [7,6..0] (reverse bits) ]
  where
    pulse :: Double
    pulse = sin (fromIntegral f * 0.4)
    glowVal :: Int
    glowVal = round $ 180 + (75 * pulse)
    draw activeIdx i b = 
        let isON = b == One
            char = if isON then "█" else "▄"
            col  = if isON then ColorTrue 0 glowVal glowVal else ColorTrue 60 60 60
            txt  = "  " ++ char ++ "   "
        in if activeIdx == i 
           then withStyle (StyleBold <> StyleUnderline) $ withColor ColorBrightWhite $ text txt
           else withColor col $ text txt

renderView :: AppModel -> L
renderView model = 
    let (Byte bitsA) = byteA model
        (Byte bitsB) = byteB model
        resByte = add (byteA model) (byteB model)
        (Byte resBits) = resByte
        isOV = checkOverflow (mode model) (byteA model) (byteB model)
        
        w = 60 
        centerTxt s = let p = max 0 (w - length s) `div` 2 in replicate p ' ' ++ s ++ replicate (w - length s - p) ' '
        padLine s = s ++ replicate (max 0 (w - length s)) ' '
    in layout
    [ pad 1 $ withBorder BorderDouble $ box " Haskell Logic Circuit Simulator "
        [ withStyle StyleBold $ text $ centerTxt "Mode:"
        , withStyle StyleBold $ text $ centerTxt (show (mode model))
        , withColor ColorBrightBlack $ text (replicate w '-')
        
        , withColor ColorBrightGreen $ text $ padLine "[ Register A ]"
        , row [ text "  LEDs:     ", renderBits (frame model) bitsA ]
        , row [ text "  Switches: ", renderSwitches (frame model) (focus model) "A" bitsA ]
        , row [ text $ if focus model == NumA then "  Numeric: >" else "  Numeric:  "
              , let val = strA model ++ (if focus model == NumA then "_" else "")
                in (if focus model == NumA then withColor ColorBrightCyan else id) (text val)
              ]
        , text (replicate w ' ')
        
        , withColor ColorBrightGreen $ text $ padLine "[ Register B ]"
        , row [ text "  LEDs:     ", renderBits (frame model) bitsB ]
        , row [ text "  Switches: ", renderSwitches (frame model) (focus model) "B" bitsB ]
        , row [ text $ if focus model == NumB then "  Numeric: >" else "  Numeric:  "
              , let val = strB model ++ (if focus model == NumB then "_" else "")
                in (if focus model == NumB then withColor ColorBrightCyan else id) (text val)
              ]
        , withColor ColorBrightBlack $ text (replicate w '-')
        
        , withColor ColorBrightRed $ text $ padLine "[ ALU Output ]"
        , row [ text "  LEDs:     ", renderBits (frame model) resBits ]
        , row [ text "  Overflow: ", if isOV then withColor (ColorTrue 255 0 0) (text " ● ") else withColor (ColorTrue 40 0 0) (text " ● ") ]
        , text $ padLine ("  Numeric:  " ++ show (getNumericValue (mode model) resByte))
        ]
    , br
    , withColor ColorBrightBlack $ ul 
         [ "Controls:"
         , "  [Arrows]      Cycle Focus / Adjust Value"
         , "  [m]           Change Mode"
         , "  [ESC]         Quit Simulator"
         ]
    ]

--------------------------------------------------------------------------------
-- 6. MAIN APP RUNNER
--------------------------------------------------------------------------------

-- Safely queries the terminal size. Returns a default if it fails (e.g., in cabal run).
getInitialSize :: IO (Int, Int)
getInitialSize = do
    sz <- Term.size
    case sz of
        Just w  -> return (Term.width w, Term.height w)
        Nothing -> return (80, 24)

-- Added explicit type signature to resolve warning
logicSimApp :: LayoutzApp AppModel AppAction
logicSimApp = LayoutzApp
    { appInit = (AppModel (int2byteSigned 0) (int2byteSigned 0) "0" "0" UnsignedAdd NumA 0, CmdNone)
    , appUpdate = handleAction, appSubscriptions = handleSubs, appView = renderView
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout LineBuffering 
    hSetEcho stdin False

    -- Check terminal size once at launch
    (w, _) <- getInitialSize

    if w < 64
        then do
            putStrLn $ "Error: Terminal is too narrow (" ++ show w ++ " columns)."
            putStrLn "The Logic Circuit Simulator requires at least 64 columns."
            putStrLn "Please zoom out or expand your window and restart."
            exitFailure
        else do
            putStr "\ESC[?25l"  -- Hide cursor
            runApp logicSimApp
            putStr "\ESC[?25h"  -- Show cursor
