module Main where

import Prelude hiding (FilePath)

import Control.Lens
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
    _optsOutput :: FilePath
  } deriving ()

makeLenses ''Opts

pOpts :: Parser Opts
pOpts =
  Opts <$>
    optPath "src" 's' "Directory with .ihdf source files" <*>
    optPath "out" 'o' "Directory with .html output files"

main :: IO ()
main = do
  opts <- options "Build \"Intermediate Haskell\"" pOpts
  tableOfContents <- readTableOfContents (opts ^. optsSource)
  chapters <- readChapters (opts ^. optsSource) tableOfContents
  writeBook (opts ^. optsOutput) $ Book tableOfContents chapters

readTableOfContents :: FilePath -> IO TableOfContents
readTableOfContents srcPath = do
  let
    tableOfContentsFileName = "table-of-contents"
    tableOfContentsPath = srcPath </> tableOfContentsFileName
  tableOfContentsFileContent <- readTextFile tableOfContentsPath
  either handleParseError return $
    parseTableOfContents tableOfContentsFileName tableOfContentsFileContent

readChapters :: FilePath -> TableOfContents -> IO (Map ChapterId Chapter)
readChapters srcPath tableOfContents =
  flip Turtle.fold Fold.map $ do
    chapterId <- select $ tableOfContents ^. tocChapters
    (chapterId,) <$> do
      let
        chapterFileName = fromText ((chapterId ^. _ChapterId) <> ".ihdf")
        chapterPath = srcPath </> chapterFileName
      chapterFileContent <- liftIO $ readTextFile chapterPath
      either handleParseError return $
        parseChapter chapterPath chapterFileContent

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
