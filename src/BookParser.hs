module BookParser where

import Prelude hiding (FilePath, span)

import Data.Monoid
import Data.Foldable
import Data.Maybe
import Data.Bifunctor
import Control.Applicative
import Control.Monad (void)
import Control.Monad.Reader
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import Data.Text (Text)
import Network.URI

import qualified Text.Megaparsec.Lexer as L
import qualified Data.List.Split as List (chunksOf)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
import qualified Turtle

import BookStructure

spaceConsumer :: MonadParsec e String m => m ()
spaceConsumer = L.space
  (void spaceChar)
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")

lexeme :: MonadParsec e String m => m a -> m a
lexeme = L.lexeme spaceConsumer

type ParseErr = ParseError Char Dec

renderParseError :: ParseErr -> Text
renderParseError = Text.pack . parseErrorPretty

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

pHeader :: (MonadReader Depth m, MonadParsec e String m) => m Span
pHeader = do
  Depth n <- ask
  string (replicate n '#' ++ " ") *> pSpan <* many newline

pSpanProp :: MonadParsec e String m => m (Span -> Span)
pSpanProp = between (string "[") (string "]") $ do
  propStr <- some (noneOf @[] "] ")
  case propStr of
    -- TODO: add more properties
    "def" ->
      return Emphasis
    (matchString "link=" -> Just link) | Just uri <- parseURI link ->
      return $ GlobalLink uri . \case
        Span "" -> Nothing
        s       -> Just s
    _ -> unexpected (Label (NonEmpty.fromList propStr))

pAnnSpan :: MonadParsec e String m => m Span
pAnnSpan = do
  props <- lexeme $ some pSpanProp
  mspan <- between (string "(") (string ")") (optional pSpan)
  let span = fromMaybe (Span "") mspan
  return $ foldr (.) id props span

pParenSpan :: MonadParsec e String m => m Span
pParenSpan = Parens <$> between (string "(") (string ")") pSpan

pSpan1 :: MonadParsec e String m => m Span
pSpan1 = asum
  [ pAnnSpan,
    pParenSpan,
    pMonoSpan,
    pMathSpan,
    pTextSpan ]

pSpan :: MonadParsec e String m => m Span
pSpan = lexeme $ do
  spans <- some pSpan1
  return $ case spans of
    [span] -> span
    spans' -> Spans spans'

pParagraph :: (MonadParsec e String m, MonadReader Depth m) => m Paragraph
pParagraph = Paragraph <$> pSpan

pTodo :: (MonadParsec e String m, MonadReader Depth m) => m Unit
pTodo = lexeme (string "TODO:") *> pUnit

pNote :: (MonadParsec e String m, MonadReader Depth m) => m Unit
pNote = lexeme (string "NOTE:") *> pUnit

pSnippet :: (MonadParsec e String m, MonadReader Depth m) => m Snippet
pSnippet = lexeme $ do
  string "```\n"
  cs <- manyTill anyChar (string "\n```")
  return $ Snippet (Text.pack cs)

pList :: (MonadParsec e String m, MonadReader Depth m) => m [Unit]
pList = some $ lexeme (string "*") *> pUnit

pUnits :: (MonadParsec e String m, MonadReader Depth m) => m [Unit]
pUnits = lexeme $ between (lexeme (string "{")) (string "}") (some pUnit)

pUnit :: (MonadParsec e String m, MonadReader Depth m) => m Unit
pUnit = asum
  [ Units <$> pUnits,
    UnitTodo <$> pTodo,
    UnitNote <$> pNote,
    UnitSnippet <$> pSnippet,
    UnitList <$> pList,
    UnitParagraph <$> pParagraph ]
  -- TODO: other units

pSection :: (MonadParsec e String m, MonadReader Depth m) => m Section
pSection = do
  header <- pHeader
  units <- many pUnit
  subsections <- local incDepth (many pSection)
  return $ Section header units subsections

pChapter :: MonadParsec e String m => m Section
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
  Either ParseErr Section
parseChapter filePath s =
  parse pChapter
    (Text.unpack (Turtle.format Turtle.fp filePath))
    (Text.unpack s)
