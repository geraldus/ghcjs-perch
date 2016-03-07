{-# LANGUAGE CPP #-}
module Internal.FFI where

#ifdef ghcjs_HOST_OS
import           Internal.Type

import           Data.JSString          (JSString)
import           GHCJS.Foreign.Callback (Callback)
import           GHCJS.Types            (JSVal)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | GHCJS' callback taking one argument.
-- Handy synonym to simplify type signatures.
type Callback1 = Callback (JSVal -> IO ())

foreign import javascript unsafe "$r = document;"
  js_document :: IO Elem

foreign import javascript unsafe "document.body"
  js_documentBody :: IO Elem

foreign import javascript unsafe "document.createElement($1)"
  js_documentCreateNode :: JSString -> IO Elem

foreign import javascript unsafe "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO Elem
-- | Check if object is an HTML Element of current DOM.  Works with HTML
-- elements and text nodes.
-- http://stackoverflow.com/a/20476546/1749901
foreign import javascript unsafe
  "$1 !== null && !(!$1.ownerDocument) && (window === ($1.ownerDocument.defaultView || $1.ownerDocument.parentWindow))"
  js_isInCurrentDOM :: JSVal -> IO Bool


foreign import javascript unsafe "$1.parentNode()"
  js_parentNode :: Elem -> IO Elem

foreign import javascript unsafe "$1.appendChild($2)"
  js_appendChild :: Elem -> Elem -> IO ()

foreign import javascript unsafe "$1.replaceChild($2,$3)"
  js_replaceChild :: Elem -> Elem -> Elem -> IO ()

foreign import javascript unsafe "$1.removeChild($2)"
  js_removeChild :: Elem -> Elem -> IO ()

foreign import javascript unsafe
  "while ($1.hasChildNodes()) $1.removeChild($1.lastChild)"
  js_clearChildren :: Elem -> IO ()


foreign import javascript unsafe "$1.setAttribute($2, $3)"
  js_setAttribute :: Elem -> PropId -> JSString -> IO ()

foreign import javascript unsafe "$1.innerHTML = $2"
  js_setInnerHtml :: Elem -> JSString -> IO ()


foreign import javascript unsafe "document.querySelectorAll($1)"
  js_querySelectorAll :: JSString -> IO JSVal


foreign import javascript unsafe
  "$1.addEventListener($2, $3);"
  js_addEventListener :: Elem -> JSString -> Callback1 -> IO ()

-- | Remove event listener from element.
foreign import javascript unsafe
  "$1.removeEventListener($2, $3);"
  js_removeEventListener :: Elem -> JSString -> Callback1 -> IO ()
--------------------------------------------------------------------------------
#endif
