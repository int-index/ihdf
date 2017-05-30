module Main where

import Prelude hiding (FilePath)

import Lens.Micro.Platform
import Network.URI
import Data.Optional
import Turtle

import Data.Map (Map)
import qualified Data.Text as Text
import qualified Control.Foldl as Fold

import BookRender
import BookParser
import BookStructure

data Opts =
  Opts {
    _optsSource :: FilePath,
    _optsResources :: URI,
    _optsOutput :: FilePath
  } deriving ()

makeLenses ''Opts

optURI :: ArgName -> ShortName -> Optional HelpMessage -> Parser URI
optURI = opt (parseURI . Text.unpack)

pOpts :: Parser Opts
pOpts =
  Opts <$>
    optPath "src" 's' "Directory with .ihdf source files" <*>
    optURI  "res" 'r' "URL to resources" <*>
    optPath "out" 'o' "Directory with .html output files"

main :: IO ()
main = do
  opts <- options "Build \"Intermediate Haskell\"" pOpts
  tableOfContents <- readTableOfContents (opts ^. optsSource)
  chapters <- readChapters
    (opts ^. optsResources)
    (opts ^. optsSource)
    tableOfContents
  writeBook (opts ^. optsOutput) $ Book tableOfContents chapters

readTableOfContents :: FilePath -> IO TableOfContents
readTableOfContents srcPath = do
  let
    tableOfContentsFileName = "table-of-contents"
    tableOfContentsPath = srcPath </> tableOfContentsFileName
  tableOfContentsFileContent <- readTextFile tableOfContentsPath
  either handleParseError return $
    parseTableOfContents tableOfContentsFileName tableOfContentsFileContent

readChapters :: URI -> FilePath -> TableOfContents -> IO (Map ChapterId Section)
readChapters resURI srcPath tableOfContents =
  flip Turtle.fold Fold.map $ do
    chapterId <- select $ tableOfContents ^. tocChapters
    (chapterId,) <$> do
      let
        chapterFileName = fromText (unChapterId chapterId) <> ".ihdf"
        chapterPath = srcPath </> chapterFileName
      chapterFileContent <- liftIO $ readTextFile chapterPath
      (section, warnings) <- either handleParseError return $
        parseChapter tableOfContents resURI chapterPath chapterFileContent
      traverse (printf (makeFormat renderWarning % "\n")) warnings
      return section

handleParseError :: MonadIO io => ParseErr -> io a
handleParseError parseError = do
  printf (makeFormat renderParseError) parseError
  exit $ ExitFailure 3

writeBook :: FilePath -> Book -> IO ()
writeBook outPath book = sh $ do
  mktree outPath
  RenderedPage outFileName outFileContent <- select $ renderBook book
  let outFilePath = outPath </> fromText (outFileName <> ".html")
  liftIO $ writeTextFile outFilePath outFileContent
