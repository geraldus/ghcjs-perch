{-# LANGUAGE CPP #-}
module Internal.API where


import           Internal.FFI
import           Internal.Type
#ifdef ghcjs_HOST_OS
import           Data.JSString
import           GHCJS.Foreign.Callback (asyncCallback1, releaseCallback)
import           GHCJS.Marshal          (FromJSVal (..))
import           GHCJS.Types            (JSVal)
#else

type JSVal= ()
type JSString= String

#endif
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
getDocument :: IO Elem
#ifdef ghcjs_HOST_OS
getDocument = js_document
#else
getDocument = notimplemented
#endif

notimplemented= error "client side call not implemented in the server"

getBody :: IO Elem
#ifdef ghcjs_HOST_OS
getBody = js_documentBody
#else
getBody = notimplemented
#endif

newElem :: JSString -> IO Elem
#ifdef ghcjs_HOST_OS
newElem = js_documentCreateNode
#else
newElem = notimplemented
#endif

newTextElem :: JSString -> IO Elem
#ifdef ghcjs_HOST_OS
newTextElem = js_createTextNode
#else
newTextElem = notimplemented
#endif

parent :: Elem -> IO Elem
#ifdef ghcjs_HOST_OS
parent = js_parentNode
#else
parent = notimplemented
#endif

-- | Appends one element to another.
addChild :: Elem -- ^ child element to append
         -> Elem -- ^ parent element
         -> IO ()
#ifdef ghcjs_HOST_OS
addChild = flip js_appendChild
#else
addChild = notimplemented
#endif

-- | Remove child from parent.
removeChild :: Elem -- ^ child to remove
            -> Elem -- ^ parent node
            -> IO ()
#ifdef ghcjs_HOST_OS
removeChild = flip js_removeChild
#else
removeChild = notimplemented
#endif

clearChildren :: Elem -> IO ()
#ifdef ghcjs_HOST_OS
clearChildren = js_clearChildren
#else
clearChildren = notimplemented
#endif

replace :: Elem -> Elem -> IO Elem
#ifdef ghcjs_HOST_OS
replace o n =
  do par <- parent o
     js_replaceChild par o n
     return n
#else
replace = notimplemented
#endif

setAttr :: Elem -> PropId -> JSString -> IO ()
#ifdef ghcjs_HOST_OS
setAttr e p = js_setAttribute e p
#else
setAttr = notimplemented
#endif

inner :: Elem -> JSString -> IO ()
#ifdef ghcjs_HOST_OS
inner e = js_innerHtml e
#else
inner = notimplemented
#endif


queryAll :: JSString -> IO [Elem]
#ifdef ghcjs_HOST_OS
queryAll query =
  do res <- js_querySelectorAll  query
     fromJSValUncheckedListOf res
#else
queryAll = notimplemented
#endif

onEvent :: NamedEvent a => Elem -> a -> (JSVal -> IO()) -> IO (IO ())
#ifdef ghcjs_HOST_OS
onEvent el et hnd = do
  callback <- asyncCallback1 hnd
  js_addEventListener el (pack (eventName et)) callback
  return (releaseCallback callback)
#else
onEvent = notimplemented
#endif
--------------------------------------------------------------------------------
