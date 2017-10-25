{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.XWiki where

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Default
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)

import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging( LogMessage(..) )
import Text.Pandoc.Options
import Text.Pandoc.Pretty (render)
import Text.Pandoc.Shared (linesToPara)
import Text.Pandoc.XML (escapeStringForXML)

-- Used for testing
import Text.Pandoc.Class (PandocIO, runIOorExplode)
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.Native

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
      env = WriterReader { options = opts, listLevel = [], useTags = False }
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

blockToXWiki (Para inlines) = do
  tags     <- asks useTags
  level    <- asks listLevel
  contents <- inlineListToXWiki inlines
  return $ if tags
              then "<p>\n" ++ contents ++ "\n</p>"
              else contents

blockToXWiki (LineBlock lines) = blockToXWiki $ linesToPara lines

blockToXWiki (CodeBlock (_, languages, _) code) = do
  return $ "{{code language=\"" ++ unwords languages ++ "\"}}" ++ "\n"
         ++ code ++ "\n"
         ++ "{{/code}"

blockToXWiki block@(RawBlock format str)
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

blockToXWiki (BulletList blocks) = do
  lev <- asks listLevel
  contents <- local (\s -> s { listLevel = listLevel s ++ "*" }) $ mapM blockListToXWiki blocks
  return $ intercalate "\n" contents ++ if null lev then "\n" else ""

blockToXWiki (DefinitionList definitions) =        undefined

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

-- [Table [] [AlignDefault,AlignCenter,AlignRight] [0.0,0.0,0.0]
--  [[Plain [Str "Tables"]]
--  ,[Plain [Str "Are"]]
--  ,[Plain [Str "Cool"]]]
--  [[[Plain [Str "col",Space,Str "3",Space,Str "is"]]
--   ,[Plain [Str "right-aligned"]]
--   ,[Plain [Str "$1600"]]]
--  ,[[Plain [Str "col",Space,Str "2",Space,Str "is"]]
--   ,[Plain [Str "centered"]]
--   ,[Plain [Str "$12"]]]
--  ,[[Plain [Str "zebra",Space,Str "stripes"]]
--   ,[Plain [Str "are",Space,Str "neat"]]
--   ,[Plain [Str "$1"]]]]]

-- XWiki does not support Table captions
-- blockToXWiki (Table _ alignments widths headers rows) = do
--   let header  = ('|':) . intercalate "|" . map ('=':) <$> blockListToXWiki headers
--       makerow = map (\b -> ("| " ++) <$> blockToXWiki b)
--       newrows = map makerow rows
--   return header

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

inlineToXWiki (Str str) = return $ escapeString str

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
