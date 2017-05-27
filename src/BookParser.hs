module BookParser where

import Prelude hiding (FilePath, span)

import Data.Monoid
import Data.Foldable
import Data.Bifunctor
import Control.Applicative
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import Data.Text (Text)

import qualified Text.Megaparsec.Lexer as L
import qualified Data.List.Split as List (chunksOf)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
import qualified Turtle

import BookStructure

spaceConsumer :: Parser ()
spaceConsumer = L.space
  (void spaceChar)
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

type ParseErr = ParseError Char Dec

renderParseError :: ParseErr -> Text
renderParseError = Text.pack . parseErrorPretty

pChapter :: Parser Chapter
pChapter =
  -- TODO
  return $ Chapter (Spans []) []

pChapterId :: Parser ChapterId
pChapterId = lexeme $ ChapterId . Text.pack <$> do
  liftA2 (:)
    alphaNumChar
    (many (alphaNumChar <|> char '-'))

pTableOfContents :: Parser TableOfContents
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
  Either ParseErr Chapter
parseChapter filePath s =
  parse pChapter
    (Text.unpack (Turtle.format Turtle.fp filePath))
    (Text.unpack s)
