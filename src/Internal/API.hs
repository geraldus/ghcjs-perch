{-# LANGUAGE CPP #-}
module Internal.API where

import           Internal.FFI
import           Internal.Type

#ifdef ghcjs_HOST_OS
import           Data.JSString
import           GHCJS.Foreign.Callback (Callback, asyncCallback1)
import           GHCJS.Marshal          (FromJSVal (..))
import           GHCJS.Types            (JSVal)
#else
type JSVal = ()
type JSString = String
data Callback a = Callback a
#endif
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
#ifndef ghcjs_HOST_OS
notImplemented :: a
notImplemented = error "Client side call not implemented on server side."
#endif

getDocument :: IO Elem
#ifdef ghcjs_HOST_OS
getDocument = js_document
#else
getDocument = notImplemented
#endif

getBody :: IO Elem
#ifdef ghcjs_HOST_OS
getBody = js_documentBody
#else
getBody = notImplemented
#endif

newElem :: JSString -> IO Elem
#ifdef ghcjs_HOST_OS
newElem = js_documentCreateNode
#else
newElem = notImplemented
#endif

newTextElem :: JSString -> IO Elem
#ifdef ghcjs_HOST_OS
newTextElem = js_createTextNode
#else
newTextElem = notImplemented
#endif

parent :: Elem -> IO Elem
#ifdef ghcjs_HOST_OS
parent = js_parentNode
#else
parent = notImplemented
#endif

-- | Appends one element to another.
addChild :: Elem -- ^ child element to append
         -> Elem -- ^ parent element
         -> IO ()
#ifdef ghcjs_HOST_OS
addChild = flip js_appendChild
#else
addChild = notImplemented
#endif

-- | Remove child from parent.
removeChild :: Elem -- ^ child to remove
            -> Elem -- ^ parent node
            -> IO ()
#ifdef ghcjs_HOST_OS
removeChild = flip js_removeChild
#else
removeChild = notImplemented
#endif

clearChildren :: Elem -> IO ()
#ifdef ghcjs_HOST_OS
clearChildren = js_clearChildren
#else
clearChildren = notImplemented
#endif

replace :: Elem -> Elem -> IO Elem
#ifdef ghcjs_HOST_OS
replace o n =
  do par <- parent o
     js_replaceChild par o n
     return n
#else
replace = notImplemented
#endif

setAttr :: Elem -> PropId -> JSString -> IO ()
#ifdef ghcjs_HOST_OS
setAttr e p = js_setAttribute e p
#else
setAttr = notImplemented
#endif

setInnerHTML :: Elem -> JSString -> IO ()
#ifdef ghcjs_HOST_OS
setInnerHTML e = js_setInnerHtml e
#else
setInnerHTML = notImplemented
#endif


queryAll :: JSString -> IO [Elem]
#ifdef ghcjs_HOST_OS
queryAll query =
  do res <- js_querySelectorAll  query
     fromJSValUncheckedListOf res
#else
queryAll = notImplemented
#endif

-- | Attach an event listener to element.
--
-- Returns an action removing listener, though you still have to release
-- callback manually.
--
-- If you are sure that you do not want to remove handler consider using
-- 'onEvent\''.
onEvent :: NamedEvent e => Elem -> e -> Callback (JSVal -> IO()) -> IO (IO ())
onEvent' :: NamedEvent e => Elem -> e -> (JSVal -> IO()) -> IO ()
#ifdef ghcjs_HOST_OS
onEvent el et cb =
  do js_addEventListener el e cb
     return $ removeEvent el e cb
  where
    e = pack (eventName et)

-- | Attach endless event listener to element.
--
-- Use this function to attach event handlers which supposed not to be removed
-- during application run.
onEvent' el et hnd =
  do cb <- asyncCallback1 hnd
     js_addEventListener el e cb
  where
    e = pack (eventName et)
#else
onEvent = notImplemented
onEvent' = notImplemented
#endif
--------------------------------------------------------------------------------
