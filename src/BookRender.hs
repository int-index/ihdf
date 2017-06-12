module BookRender where

import Numeric
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text (toStrict)
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
import Data.FileEmbed

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?), (|>), (#))
import qualified Clay as C
import qualified Clay.Text as C.T

import BookStructure

data Theme = LightMode | DarkMode

metaPreamble :: H.Html
metaPreamble = do
  H.meta ! A.charset "UTF-8"
  H.meta
    ! A.name "viewport"
    ! A.content "width=device-width, initial-scale=1.0"
  H.meta
    ! A.name "robots"
    ! A.content "noindex"

linkCss :: H.AttributeValue -> H.Html
linkCss ref = do
  H.link
    ! A.href ref
    ! A.rel "stylesheet"
    ! A.type_ "text/css"

linkFontawesomeCss :: H.Html
linkFontawesomeCss = do
  H.link
    ! A.href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    ! A.rel "stylesheet"
    ! A.type_ "text/css"

renderIcon :: H.AttributeValue -> H.Html
renderIcon attr = H.i ! A.class_ attr $ ""

renderTableOfContents :: Given Book => TableOfContents -> H.Html
renderTableOfContents (TableOfContents chapterIds) = do
  H.head $ do
    metaPreamble
    linkCss "./toc-light.css" ! A.id "theme-link"
    H.title "Intermediate Haskell"
    H.script ""
      ! A.type_ "text/javascript"
      ! A.src "./toc.js"
  H.body $ do
    H.ol $ foldMap renderTocEntry chapterIds
  where
    renderTocEntry chapterId = H.li $
      renderChapterRef False chapterId <>
      renderChapterProgress chapterId

renderNav :: H.Html
renderNav = H.nav $ do
  H.a ! A.href "./table-of-contents.html" $
    renderIcon "fa fa-home icon-left" <> "Table of Contents"
  H.a ! A.href "#" ! A.onclick "dark();" ! A.class_ "theme-button theme-button-to-dark" $
    renderIcon "fa fa-circle-o icon-left" <> "Dark Mode"
  H.a ! A.href "#" ! A.onclick "light();" ! A.class_ "theme-button theme-button-to-light" $
    renderIcon "fa fa-dot-circle-o icon-left" <> "Light Mode"

renderChapter :: Given Book => Section -> H.Html
renderChapter s = do
  H.head $ do
    metaPreamble
    linkCss "./chapter.css"
    linkCss "./chapter-default.css" ! A.id "theme-link"
    linkFontawesomeCss
    H.title . renderSpan $ s ^. sectionHeader
    H.script ! A.type_ "text/x-mathjax-config" $ do
      "  MathJax.Hub.Config({ \
      \    jax: [\"input/TeX\", \"output/HTML-CSS\"], \
      \    \"HTML-CSS\": { availableFonts: [\"TeX\"] } \
      \  }); "
    H.script ""
      ! A.type_ "text/javascript"
      ! A.src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js"
    H.script ""
      ! A.type_ "text/javascript"
      ! A.src "./chapter.js"
  H.body $ do
    renderNav
    H.main $ renderSection (Depth 1) s
    renderNav

colorBackground :: Given Theme => C.Color
colorBackground = case given @Theme of
  LightMode -> C.white
  DarkMode -> C.grayish 0x31

colorText :: Given Theme => C.Color
colorText = case given @Theme of
  LightMode -> C.black
  DarkMode -> C.grayish 0xc0

colorOutline :: Given Theme => C.Color
colorOutline = case given @Theme of
  LightMode -> C.grayish 0xcd
  DarkMode -> C.grayish 0x53

colorNote :: Given Theme => C.Color
colorNote = case given @Theme of
  LightMode -> C.rgb 0x3f 0x9b 0xbe
  DarkMode -> C.rgb 0x74 0xa4 0xb7

colorTodo :: Given Theme => C.Color
colorTodo = case given @Theme of
  LightMode -> C.red
  DarkMode -> C.rgb 0xb9 0x74 0x74

colorTip :: Given Theme => C.Color
colorTip = case given @Theme of
  LightMode -> C.rgb 0x26 0xc7 0x26
  DarkMode -> C.rgb 0x6f 0xa1 0x6f

cssFonts :: Given Theme => (C.Css, C.Css)
cssFonts = (baseFont theme, monoFont)
  where
    theme = given @Theme
    baseFont LightMode = C.fontFamily ["PT Serif"] [C.serif]
    baseFont DarkMode  = C.fontFamily ["Ubuntu"] [C.sansSerif]
    monoFont =
      C.fontFamily ["Ubuntu Mono"] [C.monospace]

cssFontColor :: Given Theme => C.Css
cssFontColor = case given @Theme of
  LightMode -> C.color C.black
  DarkMode -> C.color (C.grayish 0xc0)

cssLinks :: Given Theme => C.Css
cssLinks = do
  C.a ? C.textDecoration C.none
  case given @Theme of
    LightMode -> return ()
    DarkMode -> do
      C.a # ":link" ? C.color (C.rgb 0x8e 0x9d 0xb4)
      C.a # ":visited" ? C.color (C.rgb 0x9b 0x84 0xad)

cssThemeButton :: Given Theme => C.Css
cssThemeButton = case given @Theme of
  LightMode -> ".theme-button-to-light" ? C.display C.none
  DarkMode  -> ".theme-button-to-dark" ? C.display C.none

cssGoogleFonts :: C.Css
cssGoogleFonts = C.importUrl
  "https://fonts.googleapis.com/css?\
  \family=PT+Serif:400,400i,700|\
  \family=Ubuntu:400,400i,700|\
  \Ubuntu+Mono:400,400i,700;"

cssChapterCommon :: C.Css
cssChapterCommon = do
  cssGoogleFonts
  C.html ? do
    C.boxSizing C.borderBox
    C.sym C.margin C.nil
  C.star ? C.boxSizing C.inherit
  C.nav ? do
    C.display C.flex
    C.justifyContent C.spaceBetween
    C.sym C.padding (C.em 1)
    C.a # ":link" <> C.a # ":visited" ? C.color C.inherit
  C.body ? do
    C.sym C.margin C.auto
    C.paddingTop (C.em 2)
    C.lineHeight (C.unitless 1.6)
    C.width (C.em 50)
  C.code ? do
    C.whiteSpace C.nowrap
    C.lineHeight (C.unitless 1.3)
    C.sym2 C.padding (C.em 0.1) (C.em 0.3)
  C.legend ? do
    C.marginBottom (C.em (-0.5))
    C.textAlign C.center
    C.fontVariant C.smallCaps
  ".snippet" ? do
    C.display C.block
    C.outlineStyle C.none
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
  ".subsection" ? do
    C.paddingTop (C.em 0.5)
    C.textAlign C.center
  ".icon-left" ? do
    C.marginRight (C.em 0.5)

cssChapterDefault :: C.Css
cssChapterDefault = do
  C.importUrl "./chapter-light.css"
  ".theme-button" ? C.display C.none

cssChapter :: Given Theme => C.Css
cssChapter = do
  let (cssBaseFont, cssMonoFont) = cssFonts
  cssThemeButton
  C.nav ? do
    C.border C.solid (C.px 1) colorOutline
  C.body ? do
    cssBaseFont
    C.background colorBackground
    C.color colorText
  cssLinks
  C.code ? do
    C.outline C.solid (C.px 1) colorOutline
  (".package-name" <> ".module-name" <> C.code) ? do
    cssMonoFont
  ".snippet" ? do
    C.borderLeft C.solid (C.px 2) colorOutline
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
    C.borderBottom C.solid (C.px 1) colorOutline
  (C.td <> C.th) ? do
    (C.paddingLeft <> C.paddingRight) (C.em 0.5)

colorMeter :: Given Theme => C.Color
colorMeter = case given @Theme of
  LightMode -> C.rgb 0x78 0xca 0x8d
  DarkMode -> C.rgb 0x78 0xca 0x8d

cssMeter :: Given Theme => C.Css
cssMeter = do
  C.meter ? do
    C.outline C.solid (C.px 1) colorOutline
  -- Firefox
  C.meter # "::-moz-meter-bar" ? do
    C.background colorMeter
  C.meter ? do
    C.background colorBackground
  -- Chrome
  C.meter # "::-webkit-meter-optimum-value" ? do
    C.background colorMeter
  C.meter # "::-webkit-meter-bar" ? do
    C.background colorBackground

cssTableOfContents :: Given Theme => C.Css
cssTableOfContents = do
  let (cssBaseFont, cssMonoFont) = cssFonts
  cssGoogleFonts
  C.body ? do
    C.sym C.margin C.auto
    C.position C.relative
    cssBaseFont
    C.background colorBackground
    C.color colorText
    C.width (C.em 45)
  cssLinks
  C.li ? do
    C.marginTop (C.px 8)
    C.paddingLeft (C.px 5)
  ".progress" ? do
    C.float C.floatRight
    C.fontSize (C.em 0.9)
    cssMonoFont
  ".explanation" ? do
    C.display C.inlineFlex
    C.justifyContent C.flexEnd
    C.marginRight (C.em 1)
  ".todo" <> ".kb-size" ? do
    C.width (C.em 4.5)
    C.textAlign (C.alignSide C.sideRight)
  cssMeter

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

renderCss :: C.Css -> Text
renderCss = Text.toStrict . C.renderWith C.compact []

renderThemeCss :: Theme -> (Given Theme => C.Css) -> Text
renderThemeCss theme css = renderCss $ give theme css

data Rendered =
  Rendered {
    _renderedExt :: Text,
    _renderedName :: Text,
    _renderedContent :: Text
  } deriving (Eq, Show)

renderPageContent :: H.Html -> Text
renderPageContent = Text.pack . renderHtml . H.docTypeHtml

renderBook :: Book -> [Rendered]
renderBook book = rTableOfContents : rChapters ++ rStaticResources
  where

    rTableOfContents :: Rendered
    rTableOfContents =
      Rendered "html" "table-of-contents"
        (renderPageContent . give book renderTableOfContents $
          book ^. bookTableOfContents)

    rChapters :: [Rendered]
    rChapters =
      Map.toList (book ^. bookChapters) <&> \(chapterId, chapter) ->
        Rendered "html" (unChapterId chapterId)
          (renderPageContent . give book renderChapter $ chapter)

    rStaticResources :: [Rendered]
    rStaticResources =
      [ Rendered "js" "toc" $(embedStringFile "src/toc.js"),
        Rendered "js" "chapter" $(embedStringFile "src/chapter.js"),
        Rendered "css" "toc-light" (renderThemeCss LightMode cssTableOfContents),
        Rendered "css" "toc-dark" (renderThemeCss DarkMode cssTableOfContents),
        Rendered "css" "chapter" (renderCss cssChapterCommon),
        Rendered "css" "chapter-default" (renderCss cssChapterDefault),
        Rendered "css" "chapter-light" (renderThemeCss LightMode cssChapter),
        Rendered "css" "chapter-dark" (renderThemeCss DarkMode cssChapter) ]
