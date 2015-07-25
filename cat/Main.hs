import Control.Applicative      ((<$>),(<*>),(<|>),many)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Bits                (clearBit,testBit)
import Data.Char                (chr,isControl,ord)
import Data.Foldable            (traverse_)
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))
import Data.Traversable         (traverse)
import Options.Applicative
import System.IO                (Handle,hGetContents,hSetEncoding,IOMode(ReadMode),
                                latin1,openFile,stdin)

type Transformation = [String] -> [String]

data Args = Args {
    filePaths :: [FilePath]
    , showAll :: Bool
    , numberNonBlank :: Bool
    , eFlag :: Bool
    , showEnds :: Bool
    , numberAll :: Bool
    , squeezeBlank :: Bool
    , tFlag :: Bool
    , showTabs :: Bool
    , uFlag :: Bool
    , showNonPrinting :: Bool
}

argsParser :: Parser Args
argsParser = Args
    <$> many (strArgument 
        ( metavar "FILENAMES.." ))
    <*> switch
        ( long "show-all"
        <> short 'A'
        <> help "Equivalent to -vET" )
    <*> switch
        ( long "number-nonblank"
        <> short 'b'
        <> help "Number all nonempty output lines, starting with 1" )
    <*> switch
        ( short 'e'
        <> help "Equivalent to -vE" )
    <*> switch
        ( long "show-ends"
        <> short 'E'
        <> help "Display a '$' after the end of each line" )
    <*> switch
        ( long "number"
        <> short 'n'
        <> help "Number all output lines, starting with 1. This option is \
                \ignored if -b is in effect" )
    <*> switch
        ( long "squeeze-blank"
        <> short 's'
        <> help "Suppress repeated adjacent empty lines; output just one empty line instead of several" )
    <*> switch
        ( short 't'
        <> help "Equivalent to -vT" )
    <*> switch
        ( long "show-tabs"
        <> short 'T'
        <> help "Display TAB characters as '^I'" )
    <*> switch
        ( short 'u'
        <> help "Ignored; for POSIX compatibility" )
    <*> switch
        ( long "show-nonprinting"
        <> short 'v'
        <> help "Display control characters except for LF and TAB \
        \using '^' notation and precede characters that have the \
        \high bit set with 'M-'" )

opts :: ParserInfo Args
opts = info (helper <*> argsParser)
    ( fullDesc
    <> progDesc "Copy FILENAME (or '-' for stdin) to stdout"
    <> header "cat - copies contents of files to stdout" )

printAll :: [String] -> IO ()
printAll = traverse_ putStrLn

getStdin :: IO [String]
getStdin = fmap lines $ hSetEncoding stdin latin1 >> getContents

getFile :: FilePath -> IO [String]
getFile "-" = getStdin
getFile fp = fmap lines $ do
    h <- openFile fp ReadMode
    hSetEncoding h latin1
    hGetContents h

getFiles :: [FilePath] -> IO [String]
getFiles fps = fmap concat $ traverse getFile fps

addLineNumber :: Int -> String -> String
addLineNumber n s = '\t' : show n ++ " " ++ s

nonBlankLineNumbers :: Transformation
nonBlankLineNumbers = nblns 1 
    where nblns _ [] = []
          nblns n ("":xs) = "\t  " : nblns n xs
          nblns n (x:xs) = addLineNumber n x : nblns (n + 1) xs

allLineNumbers :: Transformation
allLineNumbers xs = zipWith addLineNumber [1..length xs] xs

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

displayTabs :: Transformation
displayTabs = map replaceTabs
    where replaceTabs [] = []
          replaceTabs ('\t':xs) = '^' : 'I' : replaceTabs xs
          replaceTabs xs = xs

displayEnds :: Transformation
displayEnds = map (++"$")

displayAll :: Transformation
displayAll = showControlChars . showExtendedAscii

genSwitch :: (Args -> Bool) -> Transformation -> ReaderT Args Maybe Transformation
genSwitch getter transform = do
    args <- ask
    if getter args 
        then return transform 
        else lift Nothing 

showAllSwitch :: ReaderT Args Maybe Transformation
showAllSwitch = genSwitch showAll (displayAll . displayEnds . displayTabs)

numberNonBlankSwitch :: ReaderT Args Maybe Transformation
numberNonBlankSwitch = genSwitch numberNonBlank nonBlankLineNumbers

eSwitch :: ReaderT Args Maybe Transformation
eSwitch = genSwitch eFlag (displayAll . displayEnds)

showEndsSwitch :: ReaderT Args Maybe Transformation
showEndsSwitch = genSwitch showEnds displayEnds

numberAllSwitch :: ReaderT Args Maybe Transformation
numberAllSwitch = genSwitch numberAll allLineNumbers

squeezeBlankSwitch :: ReaderT Args Maybe Transformation
squeezeBlankSwitch = genSwitch squeezeBlank removeMultiNewlines

tSwitch :: ReaderT Args Maybe Transformation
tSwitch = genSwitch tFlag (displayAll . displayTabs)

showTabsSwitch :: ReaderT Args Maybe Transformation
showTabsSwitch = genSwitch showTabs displayTabs

showNonPrintingSwitch :: ReaderT Args Maybe Transformation
showNonPrintingSwitch = genSwitch showNonPrinting displayAll

transformFunctions :: [ReaderT Args Maybe Transformation]
transformFunctions = [
    showTabsSwitch
    , numberNonBlankSwitch <|> numberAllSwitch
    , showAllSwitch
    , eSwitch
    , showEndsSwitch
    , squeezeBlankSwitch
    , tSwitch
    , showNonPrintingSwitch
    ]

transformLines :: ReaderT Args Maybe Transformation
transformLines = foldl getTransformations (return id) transformFunctions
    where getTransformations g f = (.) <$> (f <|> pure id) <*> g

cat :: Args -> [String] -> [String]
cat args xs = transformFunction xs
    where transformFunction = fromMaybe id $ runReaderT transformLines args
 
main = do
    args <- execParser opts
    let fps = filePaths args
    contents <- getFiles fps 
    printAll $ cat args contents
