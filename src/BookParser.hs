module BookParser where

import Prelude hiding (FilePath, span)

import Data.Monoid
import Data.Foldable
import Data.Maybe
import Data.Bifunctor
import Control.Applicative
import Data.Traversable
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import Data.Text (Text)
import Network.URI

import qualified Text.Megaparsec.Lexer as L
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
import qualified Turtle

import BookStructure

type ParseErr = ParseError Char Dec

data Warning =
  WUnrecognizedProp Text |
  WUnrecognizedUnit Text |
  WInvalidPackage |
  WInvalidModule |
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
  WInvalidPackage -> "Invalid package name"
  WInvalidModule -> "Invalid module name"
  WInconsistentRowLength -> "Inconsistent row length"

matchString :: String -> String -> Maybe String
matchString model actual = case splitAt (length model) actual of
  (model', rest) | model == model' -> Just rest
  _ -> Nothing

pTextSpan :: MonadParsec e String m => m Span
pTextSpan = do
  Span . preprocessContent . Text.pack <$> some (noneOf @[] "\n[](){}`#$")
  where
    preprocessContent =
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
      noneOf @[] "`"

pMathSpan :: MonadParsec e String m => m Span
pMathSpan = do
  chars <- between (string "$") (string "$") (many pMathChar)
  return $ Math (Text.pack chars)
  where
    pMathChar =
      '\\' <$ string "\\\\" <|>
      '$'  <$ string "\\$"  <|>
      noneOf @[] "$"

pHeader :: (MonadState [Warning] m, MonadReader Depth m, MonadParsec e String m) => m Span
pHeader = do
  Depth n <- ask
  string (replicate n '#' ++ " ") *> pSpan <* many newline

pSpanProp :: (MonadState [Warning] m, MonadParsec e String m, MonadState [Warning] n) => m (Span -> n Span)
pSpanProp = between (string "[") (string "]") $ do
  propStr <- some (noneOf @[] "] ")
  case propStr of
    "def" -> return $ pure . Emphasis
    "emph" -> return $ pure . Emphasis
    "package" -> return preprocessPackage
    "module" -> return preprocessModule
    (matchString "link=" -> Just link) | Just uri <- parseURI link ->
      return $ pure . GlobalLink uri . \case
        Span "" -> Nothing
        s       -> Just s
    _ -> do
      warn (WUnrecognizedProp $ Text.pack propStr)
      return pure

preprocessPackage :: MonadState [Warning] n => Span -> n Span
preprocessPackage = \case
  Span packageName -> do
    let
      tUri = "https://hackage.haskell.org/package/" <> packageName
      mUri = parseURI (Text.unpack tUri)
    case mUri of
      Just uri ->
        return $ GlobalLink uri (Just $ Mono packageName)
      _ -> do
        warn WInvalidPackage
        return $ Mono packageName
  s      -> do
    warn WInvalidPackage
    pure s

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
              return $ GlobalLink uri (Just $ Mono packageName)
            _ -> do
              warn WInvalidModule
              return $ Mono packageName
      _ -> fail $ "Could not parse module/package: " ++ show t
  s      -> do
    warn WInvalidModule
    pure s

pAnnSpan :: (MonadState [Warning] m, MonadParsec e String m) => m Span
pAnnSpan = do
  props1 <- many pSpanProp
  span <- pParenSpan
  props2 <- many pSpanProp
  case props1 ++ props2 of
    [] -> return (Parens span)
    props -> foldr (<=<) return props span

pParenSpan :: (MonadState [Warning] m, MonadParsec e String m) => m Span
pParenSpan =
  fromMaybe (Span "") <$>
    between (string "(") (string ")") (optional pSpan)

pSpan1 :: (MonadState [Warning] m, MonadParsec e String m) => m Span
pSpan1 = asum
  [ try pAnnSpan,
    pMonoSpan,
    pMathSpan,
    pTextSpan ]

pSpan :: (MonadState [Warning] m, MonadParsec e String m) => m Span
pSpan = lexeme $ do
  spans <- some pSpan1
  return $ case spans of
    [span] -> span
    spans' -> Spans spans'

pParagraph :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => m Paragraph
pParagraph = Paragraph <$> pSpan

pAnnUnit :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => m Unit
pAnnUnit = do
  unitTy <- try $ lexeme (some upperChar <* char ':')
  case unitTy of
    "TABLE" -> pTable
    _ -> do
      unitWrap <- case unitTy of
        "TODO" -> pure UnitTodo
        "NOTE" -> pure UnitNote
        "TIP" -> pure UnitTip
        _ -> do
          warn $ WUnrecognizedUnit (Text.pack unitTy)
          pure id
      unitWrap <$> pUnit

data TableItem = TableSepHeader | TableSepRow | TableUnit Unit

isTableSepHeader :: TableItem -> Bool
isTableSepHeader = \case
  TableSepHeader -> True
  _              -> False

isTableSepRow :: TableItem -> Bool
isTableSepRow = \case
  TableSepRow -> True
  _           -> False

pTableItem :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => m TableItem
pTableItem =
  TableSepHeader <$ lexeme (string "====") <|>
  TableSepRow <$ lexeme (string "----") <|>
  TableUnit <$> (lexeme (string "|") *> pUnit)

pTable :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => m Unit
pTable = do
  tableItems <- some pTableItem
  case List.splitWhen isTableSepHeader tableItems of
    [headerItems, otherItems] -> do
      headerUnits <- for headerItems $ \case
        TableUnit u -> return u
        _           -> fail "Bad delimiter in table header"
      let otherItems' = List.splitWhen isTableSepRow otherItems
      otherUnits <- for otherItems' $ \rowItems ->
        for rowItems $ \case
          TableUnit u -> return u
          _           -> error "Parser bug: remaining delimiter"
      let headerLength = List.length headerUnits
      unless (all ((==headerLength) . List.length) otherUnits) $ do
        warn WInconsistentRowLength
      return $ UnitTable headerUnits otherUnits
    _ -> fail "Tables with zero or multiple headers not supported"

pSnippet :: (MonadParsec e String m, MonadReader Depth m) => m Snippet
pSnippet = lexeme $ do
  string "```\n"
  cs <- manyTill anyChar (string "\n```")
  return $ Snippet (Text.pack cs)

pList :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => m [Unit]
pList = some $ lexeme (string "*") *> pUnit

pUnits :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => m [Unit]
pUnits = lexeme $ between (lexeme (string "{")) (string "}") (some pUnit)

pUnit :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => m Unit
pUnit = asum
  [ Units <$> pUnits,
    pAnnUnit,
    UnitSnippet <$> pSnippet,
    UnitList <$> pList,
    UnitParagraph <$> pParagraph ]
  -- TODO: other units

pSection :: (MonadState [Warning] m, MonadParsec e String m, MonadReader Depth m) => m Section
pSection = do
  header <- pHeader
  units <- many pUnit
  subsections <- local incDepth (many pSection)
  return $ Section header units subsections

pChapter :: (MonadState [Warning] m, MonadParsec e String m) => m Section
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
  Turtle.FilePath ->
  Text ->
  Either ParseErr (Section, [Warning])
parseChapter filePath s =
  parse (runStateT pChapter [])
    (Text.unpack (Turtle.format Turtle.fp filePath))
    (Text.unpack s)
