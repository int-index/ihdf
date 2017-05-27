module BookRender where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens
import qualified Data.Map as Map

import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?), (-:))
import qualified Clay as C

import BookStructure

renderTableOfContents :: TableOfContents -> H.Html
renderTableOfContents _ = return () -- TODO

renderChapter :: Chapter -> H.Html
renderChapter _ = return () -- TODO

data RenderedPage =
  RenderedPage {
    _renderedPageName :: Text,
    _renderedPageContent :: Text
  } deriving (Eq, Show)

renderPageContent :: H.Html -> Text
renderPageContent = Text.pack . renderHtml . H.docTypeHtml

renderBook :: Book -> [RenderedPage]
renderBook book = rTableOfContents : rChapters
  where
    rTableOfContents =
      RenderedPage "table-of-contents"
        (renderPageContent . renderTableOfContents $
          book ^. bookTableOfContents)
    rChapters =
      Map.toList (book ^. bookChapters) <&> \(chapterId, chapter) ->
        RenderedPage (chapterId ^. _ChapterId)
          (renderPageContent . renderChapter $ chapter)
