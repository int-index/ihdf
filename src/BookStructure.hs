module BookStructure where

import Data.Char (toUpper)
import Data.Data
import Data.Map (Map)
import Data.Text (Text)
import Lens.Micro.Platform
import Network.URI

import qualified Data.Text as Text

newtype Depth = Depth Int

incDepth :: Depth -> Depth
incDepth (Depth n) = Depth (n+1)

newtype ChapterId = ChapterId { unChapterId :: Text }
  deriving (Eq, Ord, Show, Data)

-- TODO: not export SectionId constructor to not accidentally use it instead of
-- 'mkSectionId' smart constructor.
newtype SectionId = SectionId { unSectionId :: Text }
  deriving (Eq, Ord, Show, Data)

mkSectionId :: Text -> SectionId
mkSectionId = SectionId . Text.intercalate "-" . Text.words . Text.toLower

sectionIdToHeader :: SectionId -> Text
sectionIdToHeader = Text.unwords . map (over _head toUpper) . Text.split (== '-') . unSectionId

data Span =
  Span Text |
  Mono Text |
  Math Text |
  Parens Span |
  Spans [Span] |
  Emphasis Span |
  SectionRef SectionId |
  ChapterRef ChapterId |
  PackageRef Text |
  ModuleRef Text |
  Link URI (Maybe Span)
  deriving (Eq, Show, Data)

data Snippet = Snippet Text
  deriving (Eq, Show, Data)

data Paragraph = Paragraph Span
  deriving (Eq, Show, Data)

data Picture =
  Picture {
    _pictureLink    :: URI,
    _pictureComment :: Maybe Text
  } deriving (Eq, Show, Data)

makeLenses ''Picture

data TableRow =
  TableSubsectionRow Unit |
  TableRegularRow [Unit]
  deriving (Eq, Show, Data)

data Table = Table [Unit] [TableRow]
  deriving (Eq, Show, Data)

data Unit =
  UnitParagraph Paragraph |
  UnitTodo Unit |
  UnitNote Unit |
  UnitTip Unit |
  UnitExercise Unit |
  UnitSolution Unit |
  UnitSnippet Snippet |
  UnitList [Unit] |
  UnitTable Table |
  UnitPicture Picture |
  Units [Unit]
  deriving (Eq, Show, Data)

data Section =
  Section {
    _sectionHeader      :: Span,
    _sectionUnits       :: [Unit],
    _sectionSubsections :: [Section]
  } deriving (Eq, Show, Data)

makeLenses ''Section

data TableOfContents =
  TableOfContents {
    _tocChapters :: [ChapterId]
  } deriving (Eq, Show, Data)

makeLenses ''TableOfContents

data Book =
  Book {
    _bookTableOfContents :: TableOfContents,
    _bookChapters        :: Map ChapterId Section
  } deriving (Eq, Show, Data)

makeLenses ''Book
