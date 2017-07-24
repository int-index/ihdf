module BookParser where

import Prelude hiding (FilePath, span)

import Data.Monoid
import Data.Foldable
import Data.Maybe
import Data.Reflection
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Control.Applicative
import Data.Traversable
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State
import Text.Megaparsec
import Text.Megaparsec.Prim
import Data.Text (Text)
import Network.URI

import qualified Text.Megaparsec.Lexer as L
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Turtle

import BookStructure

newtype ResourcesURI = ResourcesURI URI

type ParseErr = ParseError Char Dec

data Warning =
  WUnrecognizedProp Text |
  WUnrecognizedUnit Text |
  WInvalidExtension |
  WInvalidPackage |
  WInvalidModule |
  WInvalidChapter |
  WInvalidPictureCaption |
  WInconsistentRowLength

warn :: MonadState [Warning] m => Warning -> m ()
warn w = modify (w:)

spaceConsumer :: MonadParsec e String m => m ()
spaceConsumer = L.space
  (void spaceChar)
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")

lexeme :: MonadParsec e String m => m a -> m a
lexeme = L.lexeme spaceConsumer

renderParseError :: ParseErr -> Text
renderParseError = Text.pack . parseErrorPretty

renderWarning :: Warning -> Text
renderWarning = \case
  WUnrecognizedProp t -> "Could not recognize prop " <> Text.pack (show t)
  WUnrecognizedUnit t -> "Unknown unit type " <> Text.pack (show t)
  WInvalidExtension -> "Invalid extension name"
  WInvalidPackage -> "Invalid package name"
  WInvalidModule -> "Invalid module name"
  WInvalidChapter -> "Invalid chapter name"
  WInvalidPictureCaption -> "Invalid picture caption"
  WInconsistentRowLength -> "Inconsistent row length"

matchString :: String -> String -> Maybe String
matchString model actual = case splitAt (length model) actual of
  (model', rest) | model == model' -> Just rest
  _ -> Nothing

pTextSpan :: MonadParsec e String m => m Span
pTextSpan = do
  Span . preprocessContent . Text.pack <$> some (noneOf @[] "\n[](){}`$")
  where
    preprocessContent =
      Text.replace "<<" "“" .
      Text.replace ">>" "”" .
      Text.replace "---" "\x2014" .
      Text.replace "--" "\x2013"

pMonoSpan :: MonadParsec e String m => m Span
pMonoSpan = do
  chars <- between (string "`") (string "`") (many pMonoChar)
  return $ Mono (Text.pack chars)
  where
    pMonoChar =
      '\\' <$ string "\\\\" <|>
      '`'  <$ string "\\`"  <|>
      noneOf @[] "`\\"

pMathSpan :: MonadParsec e String m => m Span
pMathSpan = do
  chars <- between (string "$") (string "$") (many pMathChar)
  return $ Math (Text.pack chars)
  where
    pMathChar =
      '\\' <$ string "\\\\" <|>
      '$'  <$ string "\\$"  <|>
      noneOf @[] "$"

pHeader :: (MonadState [Warning] m, MonadReader Depth m, MonadParsec e String m) => (Given ResourcesURI, Given TableOfContents) => m Span
pHeader = do
  Depth n <- ask
  string (replicate n '#' ++ " ") *> pSpan <* many newline

pSpanProp :: (MonadState [Warning] m, MonadParsec e String m, MonadState [Warning] n, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m (Span -> n Span)
pSpanProp = between (string "[") (string "]") $ do
  propStr <- some (noneOf @[] "] ")
  case propStr of
    "def" -> return $ return . Emphasis
    "emph" -> return $ return . Emphasis
    "ext" -> return preprocessExtension
    "package" -> return preprocessPackage
    "module" -> return preprocessModule
    "chapter" -> return preprocessChapter
    (matchString "res=" -> Just resPath) -> do
      let ResourcesURI resURI = given
      return $ preprocessLink (resURI { uriPath = uriPath resURI ++ resPath })
    (matchString "link=" -> Just link) -> do
      uri <- maybe (fail "Could not parse a URI") return $ parseURI link
      return $ preprocessLink uri
    _ -> do
      warn (WUnrecognizedProp $ Text.pack propStr)
      return return

preprocessLink :: Monad n => URI -> Span -> n Span
preprocessLink uri = return . Link uri . \case
  Span "" -> Nothing
  s       -> Just s

preprocessExtension :: MonadState [Warning] n => Span -> n Span
preprocessExtension = \case
  Span extName ->
    return $ Mono ("-X" <> extName)
  s -> do
    warn WInvalidExtension
    return s

preprocessPackage :: MonadState [Warning] n => Span -> n Span
preprocessPackage = \case
  Span packageName -> do
    let
      tUri = "https://hackage.haskell.org/package/" <> packageName
      mUri = parseURI (Text.unpack tUri)
    case mUri of
      Just uri ->
        return $ Link uri (Just $ PackageRef packageName)
      _ -> do
        warn WInvalidPackage
        return $ PackageRef packageName
  s -> do
    warn WInvalidPackage
    return s

preprocessModule :: MonadState [Warning] n => Span -> n Span
preprocessModule = \case
  Span t -> do
    case Text.splitOn ":" t of
      [packageName, moduleName] |
        not (Text.null packageName),
        not (Text.null moduleName) -> do
          let
            tUri =
              "https://hackage.haskell.org/package/" <>
              packageName <>
              "/docs/" <>
              Text.replace "." "-" moduleName <>
              ".html"
            mUri = parseURI (Text.unpack tUri)
          case mUri of
            Just uri ->
              return $ Link uri (Just $ ModuleRef moduleName)
            _ -> do
              warn WInvalidModule
              return $ ModuleRef moduleName
      _ -> fail $ "Could not parse module/package: " ++ show t
  s -> do
    warn WInvalidModule
    return s

preprocessChapter :: Given TableOfContents => MonadState [Warning] n => Span -> n Span
preprocessChapter = \case
  Span chapterId -> do
    let
      TableOfContents validChapterIds = given
      chapterId' = ChapterId chapterId
    if List.elem chapterId' validChapterIds
      then return $ ChapterRef chapterId'
      else do
        warn WInvalidChapter
        return $ Span chapterId
  s -> do
    warn WInvalidChapter
    return s

pAnnSpan :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Span
pAnnSpan = do
  props1 <- many pSpanProp
  span <- pParenSpan
  props2 <- many pSpanProp
  case props1 ++ props2 of
    [] -> return (Parens span)
    props -> foldr (<=<) return props span

pParenSpan :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Span
pParenSpan =
  fromMaybe (Span "") <$>
    between (string "(") (string ")") (optional pSpan)

pSpan1 :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Span
pSpan1 = asum
  [ try pAnnSpan,
    pMonoSpan,
    pMathSpan,
    pTextSpan ]

pSpan :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Span
pSpan = lexeme $ do
  spans <- some pSpan1
  return $ case spans of
    [span] -> span
    spans' -> Spans spans'

pParagraph :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Paragraph
pParagraph = Paragraph <$> pSpan

pPicture :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Picture
pPicture = do
  s <- lexeme pAnnSpan
  case s of
    Link link comment -> case comment of
      Nothing        -> return $ Picture link Nothing
      Just (Span s') -> return $ Picture link (Just s')
      _      -> do
        warn WInvalidPictureCaption
        return $ Picture link Nothing
    _ -> fail "Pictures are represented as links"

pAnnUnit :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Unit
pAnnUnit = do
  unitTy <- try $ lexeme (some upperChar <* char ':')
  case unitTy of
    "TABLE" -> UnitTable <$> pTable
    "PICTURE" -> UnitPicture <$> pPicture
    _ -> do
      unitWrap <- case unitTy of
        "TODO" -> return UnitTodo
        "NOTE" -> return UnitNote
        "TIP" -> return UnitTip
        _ -> do
          warn $ WUnrecognizedUnit (Text.pack unitTy)
          return id
      unitWrap <$> pUnit

data TableSep =
  TableSepHeader |
  TableSepRow |
  TableSepSubsection

data TableItem =
  TableSep TableSep |
  TableUnit Unit

data TableSection = TableSection [Unit] TableSep

pTableItem :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m TableItem
pTableItem =
  TableSep TableSepHeader <$ lexeme (string "====") <|>
  TableSep TableSepRow <$ lexeme (string "----") <|>
  TableSep TableSepSubsection <$ lexeme (string "++++") <|>
  TableUnit <$> (lexeme (string "|") *> pUnit)

pTable :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Table
pTable = do
  tableItems <- some pTableItem
  case toTableSections tableItems of
    Left () -> fail "Table must end with a separator to determine section type"
    Right tableSections -> case nonEmpty tableSections of
      Nothing -> fail "Table is empty"
      Just (firstTableSection :| tableSections') -> do
        case firstTableSection of
          TableSection headerUnits TableSepHeader -> do
            tableRows <- for tableSections' $ \case
              TableSection _ TableSepHeader ->
                fail "Table header occurs after regular rows"
              TableSection us TableSepSubsection -> do
                case us of
                  [u] -> return (TableSubsectionRow u)
                  _   -> fail "Table subsection must have exactly one row"
              TableSection us TableSepRow -> do
                unless (length us == length headerUnits) $
                  warn WInconsistentRowLength
                return (TableRegularRow us)
            return $ Table headerUnits tableRows
          _ -> fail "Table has no header"
  where
    -- Get the next table section: a sequence of table units ending with a
    -- table separator.
    chopTableSection :: [TableItem] -> Maybe (TableSection, [TableItem])
    chopTableSection tableItems = do
      tableItem :| tableItems' <- nonEmpty tableItems
      case tableItem of
        TableSep tabSep -> Just (TableSection [] tabSep, tableItems')
        TableUnit u -> do
          (TableSection us tabSep, tableItems'') <- chopTableSection tableItems'
          Just (TableSection (u:us) tabSep, tableItems'')

    -- Break a table into sections.
    toTableSections :: [TableItem] -> Either () [TableSection]
    toTableSections [] = Right []
    toTableSections tableItems = do
      case chopTableSection tableItems of
        Nothing -> Left ()
        Just (tabSection, tableItems') -> do
          (tabSection:) <$> toTableSections tableItems'

pSnippet :: (MonadParsec e String m) => m Snippet
pSnippet = lexeme $ do
  void $ string "```\n"
  cs <- manyTill anyChar (string "\n```")
  return $ Snippet (Text.pack cs)

pList :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m [Unit]
pList = some $ lexeme (string "*") *> pUnit

pUnits :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m [Unit]
pUnits = lexeme $ between (lexeme (string "{")) (string "}") (many pUnit)

pUnit :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Unit
pUnit = do
  notFollowedBy (char '#')
  asum
    [ Units <$> pUnits,
      pAnnUnit,
      UnitSnippet <$> pSnippet,
      UnitList <$> pList,
      UnitParagraph <$> pParagraph ]

pSection :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => (Given ResourcesURI, Given TableOfContents) => m Section
pSection = do
  header <- pHeader
  units <- many pUnit
  subsections <- local incDepth (many pSection)
  return $ Section header units subsections

pChapter :: (MonadState [Warning] m, MonadParsec e String m) => (Given ResourcesURI, Given TableOfContents) => m Section
pChapter = runReaderT pSection (Depth 1) <* eof

pChapterId :: MonadParsec e String m => m ChapterId
pChapterId = lexeme $ ChapterId . Text.pack <$> do
  liftA2 (:)
    alphaNumChar
    (many (alphaNumChar <|> char '-'))

pTableOfContents :: MonadParsec e String m => m TableOfContents
pTableOfContents =
  spaceConsumer *> do
    TableOfContents <$>
      many pChapterId <* eof

parseTableOfContents ::
  Turtle.FilePath ->
  Text ->
  Either ParseErr TableOfContents
parseTableOfContents filePath s =
  parse pTableOfContents
    (Text.unpack (Turtle.format Turtle.fp filePath))
    (Text.unpack s)

parseChapter ::
  TableOfContents ->
  URI ->
  Turtle.FilePath ->
  Text ->
  Either ParseErr (Section, [Warning])
parseChapter toc resURI filePath s =
  parse (runStateT (give toc (give (ResourcesURI resURI) pChapter)) [])
    (Text.unpack (Turtle.format Turtle.fp filePath))
    (Text.unpack s)
