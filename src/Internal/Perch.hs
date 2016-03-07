{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Internal.Perch where

import           Internal.API
import           Internal.Type

#ifdef ghcjs_HOST_OS
import           Data.JSString          (JSString, pack)
import           GHCJS.Foreign.Callback (Callback)
import           GHCJS.Types            (JSVal)
#endif

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.String            (IsString (..))
import           Data.Typeable          (Typeable)
import           Unsafe.Coerce          (unsafeCoerce)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
#ifndef ghcjs_HOST_OS
pack = undefined
#endif

newtype PerchM a =
  Perch { build :: Elem -> IO Elem }
  deriving Typeable

type Perch = PerchM ()


class ToElem a where
  toElem :: a -> Perch


class Attributable h where
  (!) :: h -> Attribute -> h
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
instance Monoid (PerchM a) where
  mappend mx my = Perch $ \e ->
    do build mx e
       build my e
       return e
  mempty = Perch return

instance Functor PerchM

instance Applicative PerchM

instance Monad PerchM where
  (>>) x y = mappend (unsafeCoerce x) y
  (>>=) = error "bind (>>=) invocation in the Perch monad creating DOM elements"
  return = mempty

instance MonadIO PerchM where
  liftIO io = Perch $ \e -> io >> return e

instance ToElem (PerchM a) where
  toElem = unsafeCoerce

instance IsString Perch where
  fromString = toElem

instance Attributable Perch where
 (!) pe (aname, aval) = pe `attr` (aname,  aval)

instance ToElem a => Attributable (a -> Perch) where
 (!) pe (aname, aval) = \e -> pe e `attr` (aname,  aval)


instance ToElem JSString where
  toElem s = Perch $ \x ->
    do e <- newTextElem s
       addChild e x
       return e

#ifdef ghcjs_HOST_OS
instance ToElem String where
  toElem = toElem . pack
#endif

instance Show a => ToElem a where
  toElem = toElem . show
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- * DOM Tree Building

attr :: forall a. PerchM a -> (PropId, JSString) -> PerchM a
attr t (n, v) = Perch $ \x ->
  do tag <- build t x
     setAttr tag n v
     return tag

nelem :: JSString -> Perch
nelem s = Perch $ \x ->
  do e <- newElem s
     addChild e x
     return e

-- | Build an element as child of another one.  Child element becomes new
-- continuation for monadic expression.
child :: ToElem a
      => Perch -- ^ parent
      -> a     -- ^ child
      -> Perch
child me ch = Perch $ \x ->
  do e <- build me x
     build (toElem ch) e
     return e

setHtml :: Perch -> JSString -> Perch
setHtml me text = Perch $ \x ->
  do e <- build me x
     setInnerHTML e text
     return x

-- | Build perch and attach an event handler to its element.
--
-- Event handler should be an IO action wrapped by GHCJS' 'Callback' taking one
-- argument, that is an actual JavaScript event object baked in @JSVal@.
addEvent :: (NamedEvent e) => Perch -> e -> Callback (JSVal -> IO ()) -> Perch
addEvent be event action = Perch $ \x ->
  do e <- build be x
     onEvent e event action
     return e

-- | Build perch and attach an event handler to its element.  Use this function
-- only when you are sure that you won't detach handler during application run.
addEvent' :: (NamedEvent e) => Perch -> e -> (JSVal -> IO ()) -> Perch
addEvent' be event action = Perch $ \x ->
  do e <- build be x
     onEvent' e event action
     return e

-- | Build perch and remove an event handler from it.
--
-- Note, you still have to release callback manually.
remEvent :: (NamedEvent e) => Perch -> e -> Callback (JSVal -> IO ()) -> Perch
remEvent be event action = Perch $ \x ->
  do e <- build be x
     removeEvent e event action
     return e

-- ** Leaf DOM Nodes
area, base, br, col, embed, hr, img, input, keygen, link, menuitem :: Perch
meta, param, source, track, wbr :: Perch

area     = nelem "area"
base     = nelem "base"
br       = nelem "br"
col      = nelem "col"
embed    = nelem "embed"
hr       = nelem "hr"
img      = nelem "img"
input    = nelem "input"
keygen   = nelem "keygen"
link     = nelem "link"
menuitem = nelem "menuitem"
meta     = nelem "meta"
param    = nelem "param"
source   = nelem "source"
track    = nelem "track"
wbr      = nelem "wbr"

-- ** Parent DOM Nodes

a, abbr, address, article, aside, audio, b, bdo, blockquote, body, button,
  canvas, caption, center, cite, code, colgroup, command, datalist, dd, del,
  details, dfn, div, dl, dt, em, fieldset, figcaption, figure, footer, form, h1,
  h2, h3, h4, h5, h6, head, header, hgroup, html, i, iframe, ins, kbd, label,
  legend, li, map, mark, menu, meter, nav, noscript, object, ol, optgroup,
  option, output, p, pre, progress, q, rp, rt, ruby, samp, script, section,
  select, small, span, strong, sub, summary, sup, table, tbody, td, textarea,
  tfoot, th, thead, time, title, tr, ul, var, video :: ToElem a => a -> Perch

a cont          = nelem "a" `child` cont
abbr cont       = nelem "abbr" `child` cont
address cont    = nelem "address" `child` cont
article cont    = nelem "article" `child` cont
aside cont      = nelem "aside" `child` cont
audio cont      = nelem "audio" `child` cont
b cont          = nelem "b" `child` cont
bdo cont        = nelem "bdo" `child` cont
blockquote cont = nelem "blockquote" `child` cont
body cont       = nelem "body" `child` cont
button cont     = nelem "button" `child` cont
canvas cont     = nelem "canvas" `child` cont
caption cont    = nelem "caption" `child` cont
cite cont       = nelem "cite" `child` cont
code cont       = nelem "code" `child` cont
colgroup cont   = nelem "colgroup" `child` cont
command cont    = nelem "command" `child` cont
datalist cont   = nelem "datalist" `child` cont
dd cont         = nelem "dd" `child` cont
del cont        = nelem "del" `child` cont
details cont    = nelem "details" `child` cont
dfn cont        = nelem "dfn" `child` cont
div cont        = nelem "div" `child` cont
dl cont         = nelem "dl" `child` cont
dt cont         = nelem "dt" `child` cont
em cont         = nelem "em" `child` cont
fieldset cont   = nelem "fieldset" `child` cont
figcaption cont = nelem "figcaption" `child` cont
figure cont     = nelem "figure" `child` cont
footer cont     = nelem "footer" `child` cont
form cont       = nelem "form" `child` cont
h1 cont         = nelem "h1" `child` cont
h2 cont         = nelem "h2" `child` cont
h3 cont         = nelem "h3" `child` cont
h4 cont         = nelem "h4" `child` cont
h5 cont         = nelem "h5" `child` cont
h6 cont         = nelem "h6" `child` cont
head cont       = nelem "head" `child` cont
header cont     = nelem "header" `child` cont
hgroup cont     = nelem "hgroup" `child` cont
html cont       = nelem "html" `child` cont
i cont          = nelem "i" `child` cont
iframe cont     = nelem "iframe" `child` cont
ins cont        = nelem "ins" `child` cont
kbd cont        = nelem "kbd" `child` cont
label cont      = nelem "label" `child` cont
legend cont     = nelem "legend" `child` cont
li cont         = nelem "li" `child` cont
map cont        = nelem "map" `child` cont
mark cont       = nelem "mark" `child` cont
menu cont       = nelem "menu" `child` cont
meter cont      = nelem "meter" `child` cont
nav cont        = nelem "nav" `child` cont
noscript cont   = nelem "noscript" `child` cont
object cont     = nelem "object" `child` cont
ol cont         = nelem "ol" `child` cont
optgroup cont   = nelem "optgroup" `child` cont
option cont     = nelem "option" `child` cont
output cont     = nelem "output" `child` cont
p cont          = nelem "p" `child` cont
pre cont        = nelem "pre" `child` cont
progress cont   = nelem "progress" `child` cont
q cont          = nelem "q" `child` cont
rp cont         = nelem "rp" `child` cont
rt cont         = nelem "rt" `child` cont
ruby cont       = nelem "ruby" `child` cont
samp cont       = nelem "samp" `child` cont
script cont     = nelem "script" `child` cont
section cont    = nelem "section" `child` cont
select cont     = nelem "select" `child` cont
small cont      = nelem "small" `child` cont
span cont       = nelem "span" `child` cont
strong cont     = nelem "strong" `child` cont
{-style cont = nelem  "style" `child` cont-}
sub cont        = nelem "sub" `child` cont
summary cont    = nelem "summary" `child` cont
sup cont        = nelem "sup" `child` cont
table cont      = nelem "table" `child` cont
tbody cont      = nelem "tbody" `child` cont
td cont         = nelem "td" `child` cont
textarea cont   = nelem "textarea" `child` cont
tfoot cont      = nelem "tfoot" `child` cont
th cont         = nelem "th" `child` cont
thead cont      = nelem "thead" `child` cont
time cont       = nelem "time" `child` cont
title cont      = nelem "title" `child` cont
tr cont         = nelem "tr" `child` cont
ul cont         = nelem "ul" `child` cont
var cont        = nelem "var" `child` cont
video cont      = nelem "video" `child` cont

ctag :: (ToElem b) => JSString -> b -> Perch
ctag tag cont = nelem tag `child` cont

-- ** HTML4 Support
center cont = nelem "center" `child` cont

noHtml :: Perch
noHtml = mempty


-- * DOM Tree Navigation & Manipulation

-- ** Attributes

atr :: String -> JSString -> Attribute
atr n v = (pack n,  v)

id, height, href, src, style, width :: JSString -> Attribute

id     = atr "id"
height = atr "height"
href   = atr "href"
src    = atr "src"
style  = atr "style"
width  = atr "width"

-- ** Traversal

-- | Return the current node.
this :: Perch
this = Perch $ \e -> return e

-- | Goes to the parent node of the first and execute the second.
goParent :: Perch -> Perch -> Perch
goParent ch pe = Perch $ \e ->
  do fs <- build ch e
     pr <- parent fs
     sn <- build pe pr
     return sn

-- ** Manipulation

-- | Delete the current node and return the parent.
delete :: Perch
delete = Perch $ \e ->
  do par <- parent e
     removeChild e par
     return par

-- | Delete all children of the current node.
clear :: Perch
clear = Perch $ \e ->
  do clearChildren e
     return e

-- | Replace the current node with a new one
outer ::  Perch -> Perch -> Perch
outer olde newe = Perch $ \e ->
  do o <- build olde e
     n <- build newe e
     replace o n


-- | JQuery-like DOM manipulation.  It applies the Perch DOM manipulation for
-- each found element using @querySelectorAll@ function.
forElems :: JSString -> Perch -> Perch
forElems query action = Perch $ \e ->
  do els <- queryAll query
     mapM_ (build action) els
     return e

-- | Like 'forElems', but works in IO monad.
-- Example:
--
-- @
-- import GHCJS.Foreign.Callback (asyncCallback1)
--
-- main = do
--   body <- getBody
--   makeRed \<- asyncCallback1 (\\ _ -\> do
--     forElems_ ".changeable" $
--       this ! style "color:red")
--   (flip build) body . div $ do
--      div ! atr "class" "changeable" $ \"Changeable\"
--      div \"Static\"
--      div ! atr "class" "changeable" $ \"Changeable\"
--      addEvent this Click makeRed
-- @
forElems_ :: JSString -> Perch -> IO ()
forElems_ els action =
  do build (forElems els action) undefined
     return ()

-- | Decalarative synonym for @flip forElems@.
--
-- Examples:
--
-- @
-- doAction \``withElems`\` ".item"
-- `forElems` ".item" doAction
-- @
withElems ::  Perch -> JSString -> Perch
withElems = flip forElems

-- | A declarative synonym of @flip forElements@.
withElems_ :: Perch -> JSString -> IO ()
withElems_ = flip forElems_

-- | Apply action to perch with given identifier.
forElemId :: JSString -> Perch -> Perch
forElemId eid act = Perch $ \ e ->
  do el <- getElemById eid
     build act el
     return el

-- | IO version of 'forElemId_'.
forElemId_ :: JSString -> Perch -> IO ()
forElemId_ act eid =
  do flip build undefined (forElemId act eid)
     return ()

-- | A synonym to @flip forElemId@.
withElemId :: Perch -> JSString -> Perch
withElemId = flip forElemId

-- | A synonym to @flip forElemId_@.
withElemId_ :: Perch -> JSString -> IO ()
withElemId_ = flip forElemId_
