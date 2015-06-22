{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Internal.Type
  ( Elem
  , ElemArray
  , PropId
  , PropID
  , Attribute
  , NamedEvent (..)
  , JsEvent (..)
  ) where

import           GHCJS.Foreign (fromJSString)
import           GHCJS.Types   (JSArray, JSRef, JSString)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
type Elem = JSRef Element_

type ElemArray = JSArray Element_

data Element_ = Element_

type PropId = JSString

type PropID = PropId

type Attribute = (JSString, JSString)


class NamedEvent a where
  eventName :: a -> String


data JsEvent = BlurEvent
             | ChangeEvent
             | ClickEvent
             | DblClickEvent
             | FocusEvent
             | KeyPressEvent
             | KeyUpEvent
             | KeyDownEvent
             | LoadEvent
             | MouseDownEvent
             | MouseMoveEvent
             | MouseOutEvent
             | MouseOverEvent
             | MouseUpEvent
             | SubmitEvent
             | UnloadEvent
             | WheelEvent


instance NamedEvent String where
  eventName = Prelude.id

instance Show a => NamedEvent a where
  eventName = eventName . show

instance NamedEvent JSString where
  eventName x = eventName ((fromJSString x) :: String)

instance Show JsEvent where
  show BlurEvent      = "blur"
  show ChangeEvent    = "change"
  show ClickEvent     = "click"
  show DblClickEvent  = "dblclick"
  show FocusEvent     = "focus"
  show KeyDownEvent   = "keydown"
  show KeyPressEvent  = "keypress"
  show KeyUpEvent     = "keyup"
  show LoadEvent      = "load"
  show MouseDownEvent = "mousedown"
  show MouseMoveEvent = "mousemove"
  show MouseOutEvent  = "mouseout"
  show MouseOverEvent = "mouseover"
  show MouseUpEvent   = "mouseup"
  show SubmitEvent    = "submit"
  show UnloadEvent    = "unload"
  show WheelEvent     = "wheel"
--------------------------------------------------------------------------------
