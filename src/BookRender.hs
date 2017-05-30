module BookRender where

import Data.Text (Text)
import qualified Data.Text as Text
import Lens.Micro.Platform
import qualified Data.Map as Map
import Network.URI
import Data.Monoid
import Data.String
import Data.Foldable
import qualified Data.List as List
import Data.Reflection

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

renderTableOfContents :: Given Book => TableOfContents -> H.Html
renderTableOfContents (TableOfContents chapterIds) = do
  H.head $ do
    metaPreamble
    H.title "Intermediate Haskell"
    renderCss cssTableOfContents
  H.body $ do
    H.ol $ foldMap (H.li . renderChapterRef False) chapterIds

renderCss :: C.Css -> H.Html
renderCss = H.style . H.preEscapedToHtml . C.renderWith C.compact []

renderChapter :: Given Book => Section -> H.Html
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
  C.code ? do
    C.whiteSpace C.nowrap
  (".note" <> ".todo" <> ".tip") ? do
    C.paddingLeft (C.em 2)
    C.paddingRight (C.em 1)
    C.marginBottom (C.em 1)
  ".note" ? do
    C.border C.solid (C.px 1) skyblue
  ".todo" ? do
    C.border C.solid (C.px 1) C.red
  ".tip" ? do
    C.border C.solid (C.px 1) C.green
  C.table ? do
    (C.marginTop <> C.marginLeft) (C.em 1)
    C.borderCollapse C.collapse
  C.thead ? do
    C.borderBottom C.solid (C.px 1) (C.grayish 0xcd)
  (C.td <> C.th) ? do
    (C.paddingLeft <> C.paddingRight) (C.em 0.5)
  ".subsection" ? do
    C.paddingTop (C.em 0.5)
    C.textAlign C.center

cssTableOfContents :: C.Css
cssTableOfContents = do
  C.a ? do
    C.textDecoration C.none

renderSection :: Given Book => Depth -> Section -> H.Html
renderSection d s = do
  renderHeader d (s ^. sectionHeader)
  foldMap renderUnit (s ^. sectionUnits)
  foldMap (renderSection (incDepth d)) (s ^. sectionSubsections)

renderHeader :: Given Book => Depth -> Span -> H.Html
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

renderUnit :: Given Book => Unit -> H.Html
renderUnit = \case
  UnitParagraph (Paragraph s) -> H.p $ renderSpan s
  UnitTodo u -> H.div ! A.class_ "todo" $ renderUnit u
  UnitNote u -> H.div ! A.class_ "note" $ renderUnit u
  UnitTip u -> H.div ! A.class_ "tip" $ renderUnit u
  UnitSnippet (Snippet t) -> (H.code . H.pre) (H.toHtml t)
  UnitList us -> H.ul (foldMap (H.li . renderUnit) us)
  UnitTable tbl -> renderTable tbl
  UnitPicture pic -> renderPicture pic
  Units us -> foldMap renderUnit us

renderPicture :: Picture -> H.Html
renderPicture pic =
  addAltText $
    H.img ! A.src (fromString . Text.unpack . renderURI $ pic ^. pictureLink)
  where
    addAltText = case pic ^. pictureComment of
      Nothing -> id
      Just t -> (! A.alt (fromString $ Text.unpack t))


renderTable :: Given Book => Table -> H.Html
renderTable (Table headerUnits rows) = H.table $ do
  H.thead $ foldMap (H.th . renderHeaderUnit True) headerUnits
  H.tbody $ foldMap (H.tr . renderRow) rows
  where
    renderRow :: TableRow -> H.Html
    renderRow = \case
      TableSubsectionRow u ->
        let
          sTableWidth = (fromString . show . length) headerUnits
        in
          H.td
            ! A.class_ "subsection"
            ! A.colspan sTableWidth
            $ renderHeaderUnit True u
      TableRegularRow us -> foldMap (H.td . renderTableUnit) us

renderTableUnit :: Given Book => Unit -> H.Html
renderTableUnit = \case
  Units (u:us) ->
    fold .  List.intersperse H.br $
    renderTableUnit u : fmap renderTableUnit us
  UnitParagraph (Paragraph s) -> renderSpan s
  u -> renderUnit u

renderHeaderUnit :: Given Book => Bool -> Unit -> H.Html
renderHeaderUnit b = \case
  Units (u:us) ->
    fold . List.intersperse H.br $
    renderHeaderUnit b u : fmap (renderHeaderUnit False) us
  UnitParagraph (Paragraph s) ->
    strong' $ renderSpan s
  u ->
    strong' $ renderUnit u
  where
    strong' = if b then H.strong else id

renderURI :: URI -> Text
renderURI = Text.pack . ($"") . uriToString id

renderChapterRef :: Given Book => Bool -> ChapterId -> H.Html
renderChapterRef withQuotes chapterId = do
  let mChapterId = given ^. bookChapters . at chapterId
  case mChapterId of
    Nothing -> error "Invalid chapter id. Couldn't have passed validation in the parser!"
    Just section ->
      let
        span = section ^. sectionHeader
        tUri = "./" <> unChapterId chapterId <> ".html"
        addQuotes
          | withQuotes = \a -> "“" <> a <> "”"
          | otherwise  = id
      in
        addQuotes $ H.a ! A.href (H.toValue tUri) $ renderSpan span

renderSpan :: Given Book => Span -> H.Html
renderSpan = \case
  Span t -> H.toHtml t
  Mono t -> H.code (H.toHtml t)
  Math t -> H.script ! A.type_ "math/tex" $ H.preEscapedToHtml t
  Parens s -> "(" <> renderSpan s <> ")"
  Spans ss -> foldMap renderSpan ss
  Emphasis s -> H.em (renderSpan s)
  ChapterRef cId -> renderChapterRef True cId
  Link uri ms ->
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
        (renderPageContent . give book renderTableOfContents $
          book ^. bookTableOfContents)
    rChapters =
      Map.toList (book ^. bookChapters) <&> \(chapterId, chapter) ->
        RenderedPage (unChapterId chapterId)
          (renderPageContent . give book renderChapter $ chapter)
