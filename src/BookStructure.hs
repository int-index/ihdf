module BookStructure where

import Data.Map (Map)
import Data.Text
import Network.URI
import Control.Lens

data Span =
  Span Text |
  Mono Text |
  Spans [Span] |
  Strong Span |
  Emphasis Span |
  GlobalLink URI Span
  deriving (Eq, Show)

data Snippet = Snippet Text
  deriving (Eq, Show)

data Paragraph = Paragraph Span
  deriving (Eq, Show)

data Table =
  Table {
    _tableComment :: Span,
    _tableHeader :: [Span],
    _tableRows :: [[Span]]
  } deriving (Eq, Show)

data Picture =
  Picture {
    _pictureLink :: Text,
    _pictureComment :: Span
  } deriving (Eq, Show)

data Unit =
  UnitParagraph Paragraph |
  UnitTodo Paragraph |
  UnitNote Paragraph |
  UnitSnippet Snippet |
  UnitTable Table |
  UnitPicture Picture
  deriving (Eq, Show)

data Section =
  Section {
    _sectionHeader :: Span,
    _sectionUnits :: [Unit]
  } deriving (Eq, Show)

data Chapter =
  Chapter {
    _chapterHeader :: Span,
    _chapterSections :: [Section]
  } deriving (Eq, Show)

newtype ChapterId = ChapterId Text
  deriving (Eq, Ord, Show)

makePrisms ''ChapterId

data TableOfContents =
  TableOfContents {
    _tocChapters :: [ChapterId]
  } deriving (Eq, Show)

makeLenses ''TableOfContents

data Book =
  Book {
    _bookTableOfContents :: TableOfContents,
    _bookChapters        :: Map ChapterId Chapter
  } deriving (Eq, Show)

makeLenses ''Book
