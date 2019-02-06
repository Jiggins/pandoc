{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.XWiki where

import Control.Monad.Reader
-- import Control.Monad.State.Strict

import Data.Default
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import Data.Monoid ((<>))

import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging( LogMessage(..) )
import Text.Pandoc.Options
-- import Text.Pandoc.Pretty (render)
import Text.Pandoc.Shared (linesToPara)
import Text.Pandoc.XML (escapeStringForXML)

-- Used for testing
import Text.Pandoc.Class (PandocIO, runIOorExplode)
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.Native
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)

import qualified Data.Text as T
import qualified Data.Text.IO as T

data WriterState = WriterState {
    stOptions :: WriterOptions
}

data WriterReader = WriterReader {
    options :: WriterOptions
  , listLevel :: String
  , useTags :: Bool
}

type XWikiWriter m = ReaderT WriterReader (StateT WriterState m)

writeXWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeXWiki opts document =
  let initialState = WriterState { stOptions = opts }
      env = WriterReader { options = opts, listLevel = "*", useTags = False }
    in evalStateT (runReaderT (pandocToXWiki document) env) initialState

pandocToXWiki :: PandocMonad m => Pandoc -> XWikiWriter m Text
pandocToXWiki (Pandoc meta blocks) = do
  opts <- asks options
  body <- blockListToXWiki blocks
  pack <$> case writerTemplate opts of
             Nothing  -> return body
             Just template -> error "Template not yet created"

-- | Debug
toWhatever :: Default def => (def -> Pandoc -> PandocIO Text) -> Text -> IO ()
toWhatever writer str = do
  let markdown = readMarkdown def str
      written  = markdown >>= writer def :: PandocIO Text
  runIOorExplode written  >>= putStrLn . unpack

toXWiki :: Text -> IO ()
toXWiki = toWhatever writeXWiki

toNative :: Text -> IO ()
toNative = toWhatever writeNative

-- writeWhatever :: PandocMonad m => WriterOptions -> XWikiWriter m Text -> m Text
writeWhatever :: XWikiWriter PandocIO Text -> PandocIO Text
writeWhatever what =
  let initialState = WriterState { stOptions = def }
      env = WriterReader { options = def, listLevel = [], useTags = False }
    in evalStateT (runReaderT what env) initialState

table :: Block
table = Table [] [AlignDefault,AlignCenter,AlignRight] [0.0,0.0,0.0]
        [[Plain [Str "Tables"]]
        ,[Plain [Str "Are"]]
        ,[Plain [Str "Cool"]]]
        [[[Plain [Str "col",Space,Str "3",Space,Str "is"]]
         ,[Plain [Str "right-aligned"]]
         ,[Plain [Str "$1600"]]]
        ,[[Plain [Str "col",Space,Str "2",Space,Str "is"]]
         ,[Plain [Str "centered"]]
         ,[Plain [Str "$12"]]]
        ,[[Plain [Str "zebra",Space,Str "stripes"]]
         ,[Plain [Str "are",Space,Str "neat"]]
         ,[Plain [Str "$1"]]]]

list :: Block
list = BulletList
       [[Plain [Str "Item",Space,Str "1"]]
       ,[Plain [Str "Item",Space,Str "2"]
         ,BulletList
         [[Plain [Str "Sub",Space,Str "Item",Space,Str "1"]]
         ,[Plain [Str "Sub",Space,Str "Item",Space,Str "2"]]]]]

listBlocks :: [[Block]]
listBlocks = [[Plain [Str "Item",Space,Str "1"]]
             ,[Plain [Str "Item",Space,Str "2"]
               ,BulletList
               [[Plain [Str "Sub",Space,Str "Item",Space,Str "1"]]
               ,[Plain [Str "Sub",Space,Str "Item",Space,Str "2"]]]]]


splittab :: Block -> [TableCell]
splittab (Table _ alignments widths headers rows) = headers

hs :: [TableCell]
hs = splittab table

testBlock :: Block -> IO String
testBlock block = fmap unpack . runIOorExplode .  writeWhatever . fmap T.pack $ blockListToXWiki [block] -- >>= Prelude.putStrLn . unpack

testTable :: IO Text
testTable = runIOorExplode .  writeWhatever . fmap T.pack $ (blockListToXWiki [list])

escapeString :: String -> String
escapeString =  escapeStringForXML

surround :: String -> String -> String
surround str = (str ++) . (++ str)

-- | Convert list of Pandoc block elements to XWiki.
blockListToXWiki :: PandocMonad m
                     => [Block]       -- ^ List of block elements
                     -> XWikiWriter m String
blockListToXWiki blocks =
  fmap (intercalate "\n") $ mapM blockToXWiki blocks

-- | Convert Pandoc block element to XWiki.
blockToXWiki :: PandocMonad m
             => Block         -- ^ Block element
             -> XWikiWriter m String
blockToXWiki (Plain inlines) = inlineListToXWiki inlines

blockToXWiki (Para inlines) =
  surround "\n" <$> inlineListToXWiki inlines

blockToXWiki (LineBlock lines) = blockToXWiki $ linesToPara lines

blockToXWiki (CodeBlock (_, languages, _) code) = do
  return $ "{{code language=\"" ++ unwords languages ++ "\"}}" ++ "\n"
         ++ code ++ "\n"
         ++ "{{/code}}\n"

blockToXWiki block@(RawBlock format str)
  | format == Format "mediawiki" = return str
  | format == Format "xwiki" = return str
  | format == Format "html"  = return str
  | otherwise                = "" <$ report (BlockNotRendered block)

-- matches: [BlockQuote
--            [BlockQuote
--              [...]]]
--
-- There should only be a space after the last '>'
blockToXWiki (BlockQuote blocks@((BlockQuote _):_)) =
  ('>':)    <$> blockListToXWiki blocks

blockToXWiki (BlockQuote blocks) =
  ("> " ++) <$> blockListToXWiki blocks

blockToXWiki (OrderedList listAttributes blocks) = undefined

blockToXWiki (BulletList blocks) =
  (intercalate "\n" . map ("* " ++)) <$> mapM blockListToXWiki blocks

  -- lev <- asks listLevel
  -- contents <- local (\s -> s { listLevel = listLevel s ++ "*" }) $ mapM blockListToXWiki blocks
  -- return $ intercalate "\n" contents ++ if null lev then "\n" else ""

-- http://www.xwiki.org/xwiki/bin/view/Documentation/UserGuide/Features/XWikiSyntax/?syntax=2.1&section=DefinitionLists
blockToXWiki (DefinitionList definitions) = undefined

blockToXWiki (Header level _ inlines) = do
  contents <- inlineListToXWiki inlines
  let eqs = replicate level '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

blockToXWiki HorizontalRule = return "\n----\n"

-- Text.Pandoc.Table [Inline]
--                   [Alignment]
--                   [Double]
--                   [TableCell]
--                   [[TableCell]]

-- XWiki does not support Table captions
blockToXWiki (Table _ alignments widths headers rows) = do
  header <- concat <$> mapM (\h -> ("|=" ++) <$> blockListToXWiki h) headers
  body   <- forM rows (\row -> concat <$> mapM (\c -> ("|"  ++) <$> blockListToXWiki c) row)
  return . intercalate "\n" $ header : body

blockToXWiki (Div attr blocks) = undefined
blockToXWiki Null = return ""

-- | Convert list of Pandoc inline elements to XWiki.
inlineListToXWiki :: PandocMonad m => [Inline] -> XWikiWriter m String
inlineListToXWiki lst =
  fmap concat $ mapM inlineToXWiki lst

-- | Convert Pandoc inline element to XWiki.
inlineToXWiki :: PandocMonad m => Inline -> XWikiWriter m String

-- http://www.xwiki.org/xwiki/bin/view/Documentation/UserGuide/Features/XWikiSyntax/?syntax=2.1&section=TextFormatting
inlineToXWiki (Strong lst) =
  surround "**" <$> inlineListToXWiki lst

inlineToXWiki (Emph lst) =
  surround "//" <$> inlineListToXWiki lst

inlineToXWiki (Strikeout lst) =
  surround "--" <$> inlineListToXWiki lst

inlineToXWiki (Code _ str) =
  return . surround "##" $ escapeString str

inlineToXWiki (Superscript lst) =
  surround "^^" <$> inlineListToXWiki lst

inlineToXWiki (Subscript lst) =
  surround ",," <$> inlineListToXWiki lst

inlineToXWiki (SmallCaps lst) = inlineListToXWiki lst

inlineToXWiki (Quoted SingleQuote lst) = do
  contents <- inlineListToXWiki lst
  return $ "\8216" ++ contents ++ "\8217"

inlineToXWiki (Quoted DoubleQuote lst) = do
  contents <- inlineListToXWiki lst
  return $ "\8220" ++ contents ++ "\8221"

inlineToXWiki (Cite _  lst) = inlineListToXWiki lst

inlineToXWiki (Str str) = return $ str

inlineToXWiki (Math mt str) = return $
  "<math display=\"" ++
  (if mt == DisplayMath then "block" else "inline") ++
  "\">" ++ str ++ "</math>"
  -- note:  str should NOT be escaped

inlineToXWiki il@(RawInline f str)
  | f == Format "mediawiki" = return str
  | f == Format "html"      = return str
  | otherwise               = "" <$ report (InlineNotRendered il)

inlineToXWiki SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  listlevel <- asks listLevel
  case wrapText of
       WrapAuto     -> return " "
       WrapNone     -> return " "
       WrapPreserve -> if null listlevel
                          then return "\n"
                          else return " "

inlineToXWiki Space = return " "

inlineToXWiki (Link (_,_,attributes) txt (url, title)) = do
  label <- inlineListToXWiki txt
  let attrs = case attributes of
        [] -> ""
        as -> ("||" ++) . unwords $ map (\(k,v) -> concat [k, "=\"", v, "\""]) as

  return $ "[[" ++ label ++ ">>" ++ url ++ attrs ++ "]]"

-- The full format of an image is either image: (reference)
-- or [[image: (reference) {||parameters}]]
-- http://www.xwiki.org/xwiki/bin/view/Documentation/UserGuide/Features/XWikiSyntax/?syntax=2.1&section=Images
inlineToXWiki (Image _ _ _) = undefined

inlineToXWiki LineBreak     = return "\n"
inlineToXWiki (Note _)      = undefined

inlineToXWiki (Span (_,_,stylePairs) inlines) = do
  contents <- inlineListToXWiki inlines
  let attributes = map (\(k,v) -> k ++ "=\"" ++ v ++ "\"") stylePairs
  return $ "(% " ++ unwords attributes ++ " %)" ++ contents
