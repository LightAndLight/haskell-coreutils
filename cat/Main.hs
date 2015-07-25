import Control.Applicative      ((<$>),(<*>),(<|>),many)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Foldable            (traverse_)
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))
import Data.Traversable         (traverse)
import Options.Applicative
import System.IO                (Handle,hGetContents,hSetEncoding,IOMode(ReadMode),
                                latin1,openFile,stdin)

import Transformations

type TransformOption = ReaderT Args Maybe Transformation

data Args = Args {
    filePaths :: [FilePath]
    , showAllFlag :: Bool
    , numberNonBlankFlag :: Bool
    , eFlag :: Bool
    , showEndsFlag :: Bool
    , numberAllFlag :: Bool
    , squeezeBlankFlag :: Bool
    , tFlag :: Bool
    , showTabsFlag :: Bool
    , uFlag :: Bool
    , showNonPrintingFlag :: Bool
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
getFiles [] = getStdin
getFiles fps = fmap concat $ traverse getFile fps

createOption :: (Args -> Bool) -> Transformation -> TransformOption
createOption getter transform = do
    args <- ask
    if getter args 
        then return transform 
        else lift Nothing 

showAllOption :: TransformOption
showAllOption = createOption showAllFlag (showAll . showEnds . showTabs)

numberNonBlankOption :: TransformOption
numberNonBlankOption = createOption numberNonBlankFlag showNonBlankLineNumbers

eOption :: TransformOption
eOption = createOption eFlag (showAll . showEnds)

showEndsOption :: TransformOption
showEndsOption = createOption showEndsFlag showEnds

numberAllOption :: TransformOption
numberAllOption = createOption numberAllFlag showAllLineNumbers

squeezeBlankOption :: TransformOption
squeezeBlankOption = createOption squeezeBlankFlag removeMultiNewlines

tOption :: TransformOption
tOption = createOption tFlag (showAll . showTabs)

showTabsOption :: TransformOption
showTabsOption = createOption showTabsFlag showTabs

showNonPrintingOption :: TransformOption
showNonPrintingOption = createOption showNonPrintingFlag showAll

transformOptiones :: [TransformOption]
transformOptiones = [
    showTabsOption
    , tOption
    , numberNonBlankOption <|> numberAllOption
    , showAllOption
    , eOption
    , showEndsOption
    , squeezeBlankOption
    , showNonPrintingOption
    ]

combinedOptiones :: TransformOption
combinedOptiones = foldl composeTransformations (return id) transformOptiones
    where composeTransformations g f = (.) <$> (f <|> pure id) <*> g

cat :: Args -> [String] -> [String]
cat args xs = transformFunction xs
    where transformFunction = fromMaybe id $ runReaderT combinedOptiones args
 
main = do
    args <- execParser opts
    let fps = filePaths args
    contents <- getFiles fps 
    printAll $ cat args contents
