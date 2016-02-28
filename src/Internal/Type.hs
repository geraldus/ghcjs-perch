{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Internal.Type
  ( Elem
  , PropId
  , PropID
  , Attribute
  , NamedEvent (..)
  , JsEvent (..)
  ) where

#ifdef ghcjs_HOST_OS
import           Data.JSString
import           GHCJS.Foreign (isNull, isUndefined)
import           GHCJS.Marshal (FromJSVal (..), ToJSVal(..))
import           GHCJS.Types   (JSVal)
#endif
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
#ifndef ghcjs_HOST_OS
type JSVal= ()
type JSString= String
unpack= undefined
#endif

newtype Elem = Elem JSVal

type PropId = JSString

type PropID = PropId

type Attribute = (JSString, JSString)


class NamedEvent a where
  eventName :: a -> String


data JsEvent = Blur
             | Change
             | Click
             | DblClick
             | Focus
             | KeyPress
             | KeyUp
             | KeyDown
             | Load
             | MouseDown
             | MouseMove
             | MouseOut
             | MouseOver
             | MouseUp
             | Submit
             | Unload
             | Wheel

#ifdef ghcjs_HOST_OS
instance FromJSVal Elem where
  fromJSVal v = return (if isUndefined v || isNull v
                           then Nothing
                           else Just (Elem v))

instance ToJSVal Elem where
  toJSVal (Elem val) = return val

#endif

#ifdef ghcjs_HOST_OS
instance NamedEvent String where
  eventName = Prelude.id
#endif

instance Show a => NamedEvent a where
  eventName = eventName . show

instance NamedEvent JSString where
  eventName x = eventName ((unpack x) :: String)

instance Show JsEvent where
  show Blur      = "blur"
  show Change    = "change"
  show Click     = "click"
  show DblClick  = "dblclick"
  show Focus     = "focus"
  show KeyDown   = "keydown"
  show KeyPress  = "keypress"
  show KeyUp     = "keyup"
  show Load      = "load"
  show MouseDown = "mousedown"
  show MouseMove = "mousemove"
  show MouseOut  = "mouseout"
  show MouseOver = "mouseover"
  show MouseUp   = "mouseup"
  show Submit    = "submit"
  show Unload    = "unload"
  show Wheel     = "wheel"
--------------------------------------------------------------------------------
