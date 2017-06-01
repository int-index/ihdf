module BookRender where

import Numeric
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
import Data.Generics.Uniplate.Data
import Control.Monad

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?), (|>), (#))
import qualified Clay as C
import qualified Clay.Text as C.T

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
    linkCss "https://fonts.googleapis.com/css?family=PT+Serif:400,700,400italic,700italic&subset=latin,cyrillic"
    renderCss cssTableOfContents
  H.body $ do
    H.ol $ foldMap renderTocEntry chapterIds
  where
    renderTocEntry chapterId = H.li $
      renderChapterRef False chapterId <>
      renderChapterProgress chapterId

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

colorNote :: C.Color
colorNote = C.rgb 0x3f 0x9b 0xbe

colorTodo :: C.Color
colorTodo = C.red

colorTip :: C.Color
colorTip = C.rgb 0x26 0xc7 0x26

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
    C.outline C.solid (C.px 1) (C.grayish 0xcd)
    C.sym2 C.padding (C.em 0.1) (C.em 0.3)
  (".package-name" <> ".module-name" <> C.code) ? do
    C.fontFamily ["Ubuntu Mono"] [C.monospace]
  C.legend ? do
    C.marginBottom (C.em (-0.5))
    C.textAlign C.center
    C.fontVariant C.smallCaps
  ".snippet" ? do
    C.display C.block
    C.outlineStyle C.none
    C.borderLeft C.solid (C.px 2) (C.grayish 0xcd)
    C.whiteSpace C.T.pre
    C.padding (C.em 0.5) (C.em 0.5) (C.em 0.5) (C.em 1)
    C.marginTop (C.em 1)
    C.marginBottom (C.em 1)
  (".note" <> ".todo" <> ".tip") ? do
    C.paddingTop (C.em 0)
    C.paddingBottom (C.em 0)
    C.paddingLeft (C.em 2)
    C.paddingRight (C.em 1)
    C.marginTop (C.em 1)
    C.marginBottom (C.em 1)
  ".note" ? do
    C.border C.solid (C.px 1) colorNote
  ".note" |> C.legend ?
    C.color colorNote
  ".todo" ? do
    C.border C.solid (C.px 1) colorTodo
  ".todo" |> C.legend ?
    C.color colorTodo
  ".tip" ? do
    C.border C.solid (C.px 1) colorTip
  ".tip" |> C.legend ?
    C.color colorTip
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
  C.body ? do
    C.sym C.margin C.auto
    C.position C.relative
    C.fontFamily ["PT Serif"] [C.serif]
    C.width (C.em 45)
  C.a ? do
    C.textDecoration C.none
  C.li ? do
    C.marginTop (C.px 8)
    C.paddingLeft (C.px 5)
  ".progress" ? do
    C.float C.floatRight
    C.fontSize (C.em 0.9)
    C.fontFamily ["Ubuntu Mono"] [C.monospace]
  ".explanation" ? do
    C.display C.inlineFlex
    C.justifyContent C.flexEnd
    C.marginRight (C.em 1)
  ".todo" <> ".kb-size" ? do
    C.width (C.em 4.5)
    C.textAlign (C.alignSide C.sideRight)
  C.meter ? do
    C.outline C.solid (C.px 1) (C.grayish 0xcd)
  -- Firefox
  C.meter # "::-moz-meter-bar" ? do
    C.background (C.rgb 0x78 0xca 0x8d)
  C.meter ? do
    C.background C.white
  -- Chrome
  C.meter # "::-webkit-meter-optimum-value" ? do
    C.background (C.rgb 0x78 0xca 0x8d)
  C.meter # "::-webkit-meter-bar" ? do
    C.background C.white

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
  UnitTodo u -> H.fieldset ! A.class_ "todo" $ do
    H.legend "Todo"
    H.span $ renderUnit u
  UnitNote u -> H.fieldset ! A.class_ "note" $ do
    H.legend "Note"
    H.span $ renderUnit u
  UnitTip u -> H.fieldset ! A.class_ "tip" $ do
    H.legend "Tip"
    H.span $ renderUnit u
  UnitSnippet (Snippet t) -> H.code ! A.class_ "snippet" $ H.toHtml t
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

-- Will 'error' if the chapter doesn't exist
getChapterContent :: Given Book => ChapterId -> Section
getChapterContent chapterId =
  case given ^. bookChapters . at chapterId of
    Just s  -> s
    Nothing -> error "Invalid chapter id. Couldn't have passed \
                     \validation in the parser!"

renderChapterRef :: Given Book => Bool -> ChapterId -> H.Html
renderChapterRef withQuotes chapterId =
  let
    section = getChapterContent chapterId
    header = section ^. sectionHeader
    tUri = "./" <> unChapterId chapterId <> ".html"
    addQuotes
      | withQuotes = \a -> "“" <> a <> "”"
      | otherwise  = id
  in
    addQuotes $ H.a ! A.href (H.toValue tUri) $ renderSpan header

renderChapterProgress :: Given Book => ChapterId -> H.Html
renderChapterProgress chapterId =
  let
    section = getChapterContent chapterId
    size, relativeSize :: Double
    size = fromIntegral (sum $ map Text.length $ universeBi section) / 1024
    kbSize =
      showFFloat (Just (if size < 10 then 1 else 0)) size " kB"
    relativeSize = sqrt (size / 100)
    todos = length [u | UnitTodo u <- universeBi section]
  in
    H.span ! A.class_ "progress" $ do
      H.span ! A.class_ "explanation" $ do
        when (todos > 0) $ H.span ! A.class_ "todo" $
          H.toHtml (shows todos " TODO")
        H.span ! A.class_ "kb-size" $
          H.toHtml kbSize
      H.meter ! A.value (H.toValue relativeSize) $ ""


renderSpan :: Given Book => Span -> H.Html
renderSpan = \case
  Span t -> H.toHtml t
  Mono t -> H.code (H.toHtml t)
  Math t -> H.script ! A.type_ "math/tex" $ H.preEscapedToHtml t
  Parens s -> "(" <> renderSpan s <> ")"
  Spans ss -> foldMap renderSpan ss
  Emphasis s -> H.em (renderSpan s)
  ChapterRef cId -> renderChapterRef True cId
  PackageRef packageName -> H.span
    ! A.class_ "package-name"
    $ H.toHtml packageName
  ModuleRef moduleName -> H.span
    ! A.class_ "module-name"
    $ H.toHtml moduleName
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
