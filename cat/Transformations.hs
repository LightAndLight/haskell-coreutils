module Transformations (
    removeMultiNewlines
    , showAll
    , showAllLineNumbers
    , showControlChars
    , showEnds
    , showExtendedAscii
    , showNonBlankLineNumbers
    , showTabs
    , Transformation
    ) where

import Data.Bits                (clearBit,testBit)
import Data.Char                (chr,isControl,ord)
    
type Transformation = [String] -> [String]

addLineNumber :: Int -> String -> String
addLineNumber n s = '\t' : show n ++ " " ++ s

showNonBlankLineNumbers :: Transformation
showNonBlankLineNumbers = nblns 1 
    where nblns _ [] = []
          nblns n ("":xs) = "\t  " : nblns n xs
          nblns n (x:xs) = addLineNumber n x : nblns (n + 1) xs

showAllLineNumbers :: Transformation
showAllLineNumbers xs = zipWith addLineNumber [1..length xs] xs

toCaretNotation :: Char -> String
toCaretNotation '\DEL' = "^?"
toCaretNotation c = '^' : chr (ord '@' + ord c) : []

showControlChars :: Transformation
showControlChars = map (concatMap replaceControls)
    where replaceControls c = if and [isControl c, c /= '\t', c /= '\n']
            then toCaretNotation c
            else return c

showExtendedAscii :: Transformation
showExtendedAscii = map (concatMap replaceExtended)
    where replaceExtended c = if testBit (ord c) 7
            then "M-" ++ (return . chr $ clearBit (ord c) 7)
            else return c

removeMultiNewlines :: Transformation
removeMultiNewlines = rmnl False
    where rmnl _ [] = []
          rmnl b ("":xs) = if b
            then rmnl True xs
            else "" : rmnl True xs
          rmnl _ (x:xs) = x : rmnl False xs

showTabs :: Transformation
showTabs = map replaceTabs
    where replaceTabs [] = []
          replaceTabs ('\t':xs) = '^' : 'I' : replaceTabs xs
          replaceTabs (x:xs) = x : replaceTabs xs

showEnds :: Transformation
showEnds = map (++"$")

showAll :: Transformation
showAll = showControlChars . showExtendedAscii
