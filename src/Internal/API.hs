module Internal.API where

import           Internal.FFI
import           Internal.Type

import           GHCJS.Foreign (ForeignRetention (..), ToJSString (..),
                                asyncCallback1, fromArray, release)
import           GHCJS.Types   (JSRef)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
getDocument :: IO Elem
getDocument = js_document

getBody :: IO Elem
getBody = js_documentBody

newElem :: ToJSString a => a -> IO Elem
newElem = js_documentCreateNode . toJSString

newTextElem :: ToJSString a => a -> IO Elem
newTextElem = js_createTextNode . toJSString


parent :: Elem -> IO Elem
parent = js_parentNode

-- | Appends one element to another.
addChild :: Elem -- ^ child element to append
         -> Elem -- ^ parent element
         -> IO ()
addChild = flip js_appendChild

-- | Remove child from parent.
removeChild :: Elem -- ^ child to remove
            -> Elem -- ^ parent node
            -> IO ()
removeChild = flip js_removeChild

clearChildren :: Elem -> IO ()
clearChildren = js_clearChildren

replace :: Elem -> Elem -> IO Elem
replace o n =
  do par <- parent o
     js_replaceChild par o n
     return n

setAttr :: ToJSString a => Elem -> PropId -> a -> IO ()
setAttr e p = js_setAttribute e p . toJSString

inner :: ToJSString a => Elem -> a -> IO ()
inner e = js_innerHtml e . toJSString


queryAll :: ToJSString a => a -> IO [Elem]
queryAll query =
  do res <- js_querySelectorAll (toJSString query)
     fromArray res

onEvent :: NamedEvent a => Elem -> a -> (JSRef b -> IO()) -> IO (IO ())
onEvent el et hnd = do
  callback <- asyncCallback1 AlwaysRetain hnd
  js_addEventListener el (toJSString (eventName et)) callback
  return (release callback)
--------------------------------------------------------------------------------
