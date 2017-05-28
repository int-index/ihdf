module BookRender where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens
import qualified Data.Map as Map
import Network.URI
import Data.Monoid

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?), (-:))
import qualified Clay as C

import BookStructure

linkCss :: H.AttributeValue -> H.Html
linkCss href = do
  H.link
    ! A.href href
    ! A.rel "stylesheet"
    ! A.type_ "text/css"

metaPreamble :: H.Html
metaPreamble = do
  H.meta ! A.charset "UTF-8"
  H.meta
    ! A.name "viewport"
    ! A.content "width=device-width, initial-scale=1.0"

renderTableOfContents :: TableOfContents -> H.Html
renderTableOfContents _ = return () -- TODO

renderCss :: C.Css -> H.Html
renderCss = H.style . H.preEscapedToHtml . C.renderWith C.compact []

renderChapter :: Section -> H.Html
renderChapter s = do
  H.head $ do
    metaPreamble
    H.title . renderSpan $ s ^. sectionHeader
    H.script ! A.type_ "text/x-mathjax-config" $ do
      "  MathJax.Hub.Config({ \
      \    jax: [\"input/TeX\", \"output/HTML-CSS\"], \
      \    \"HTML-CSS\": { availableFonts: [\"TeX\"] } \
      \  }); "
    H.script ""
      ! A.type_ "text/javascript"
      ! A.src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js"
    linkCss "https://fonts.googleapis.com/css?family=PT+Serif:400,700,400italic,700italic&subset=latin,cyrillic"
    renderCss cssChapter
  H.body $ do
    H.main $ do
      renderSection (Depth 1) s

skyblue :: C.Color
skyblue = C.rgb 0x3f 0x9b 0xbe

cssChapter :: C.Css
cssChapter = do
  C.html ? do
    C.boxSizing C.borderBox
    C.sym C.margin C.nil
  C.star ? C.boxSizing C.inherit
  C.body ? do
    C.sym C.margin C.auto
    C.position C.relative
    C.fontFamily ["PT Serif"] [C.serif]
    C.width (C.em 50)
  ".note" ? do
    C.paddingLeft (C.em 2)
    C.paddingRight (C.em 1)
    C.border C.solid (C.px 1) skyblue
  ".todo" ? do
    C.paddingLeft (C.em 2)
    C.paddingRight (C.em 1)
    C.border C.solid (C.px 1) C.red

renderSection :: Depth -> Section -> H.Html
renderSection d s = do
  renderHeader d (s ^. sectionHeader)
  foldMap renderUnit (s ^. sectionUnits)
  foldMap (renderSection (incDepth d)) (s ^. sectionSubsections)

renderHeader :: Depth -> Span -> H.Html
renderHeader (Depth d) s = h $ renderSpan s
  where
    h = case d of
      1 -> H.h1
      2 -> H.h2
      3 -> H.h3
      4 -> H.h4
      5 -> H.h5
      6 -> H.h6
      _ -> error "Invalid header depth"

renderUnit :: Unit -> H.Html
renderUnit = \case
  UnitParagraph (Paragraph s) -> H.p $ renderSpan s
  UnitTodo u -> H.div ! A.class_ "todo" $ renderUnit u
  UnitNote u -> H.div ! A.class_ "note" $ renderUnit u
  UnitSnippet (Snippet t) -> (H.code . H.pre) (H.toHtml t)
  UnitList us -> H.ul (foldMap (H.li . renderUnit) us)
  Units us -> foldMap renderUnit us

renderURI :: URI -> Text
renderURI = Text.pack . ($"") . uriToString id

renderSpan :: Span -> H.Html
renderSpan = \case
  Span t -> H.toHtml t
  Mono t -> H.code (H.toHtml t)
  Math t -> H.script ! A.type_ "math/tex" $ H.preEscapedToHtml t
  Parens s -> "(" <> renderSpan s <> ")"
  Spans ss -> foldMap renderSpan ss
  Emphasis s -> H.em (renderSpan s)
  GlobalLink uri ms ->
    let t = renderURI uri
    in H.a (maybe (H.toHtml t) renderSpan ms) ! A.href (H.toValue t)

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
