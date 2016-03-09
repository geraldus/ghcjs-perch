# Perch #

[![Build Status](https://travis-ci.org/geraldus/ghcjs-perch.svg?branch=master)](https://travis-ci.org/geraldus/ghcjs-perch)

This is a GHCJS port of Perch library<sup>[1][haste-perch]</sup> originally
written by Alberto Gómez Corona.

![perch](http://i65.tinypic.com/zlu7g1.jpg)

Perch defines DOM element builders (perches) that are append-able, so that
dynamic HTML can be created in the client application in natural way, like
textual HTML but programmatically and with the advantage of static type
checking.

Main use case of Perch is client side applications, however it can be used in
monolithic server-and-client apps too, if this is what you need take a look at
`HPlay`<sup>[2][hplay]</sup> library.

This package makes the creation of DOM elements easy with a syntax similar to
other Haskell HTML generators such as `blaze-html`<sup>[3][blaze]</sup>, using
monoids and monads.

## Examples ##

Build DOM tree

```hs
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Main where

import           Data.JSString (JSString)
import           GHCJS.Perch
import           Prelude       hiding (div, id)
default (JSString)

dom :: Perch
dom =
  div ! id "wrap" $
    do div ! id "content" $
         do p "Hello World!"
            a ! href "https://github.com/geraldus/ghcjs-perch" $
              "Perch on GitHub"
       div ! id "footer" $ "Happy Haskelling!"

main :: IO ()
main =
  do documentBody <- getBody
     build dom documentBody
     return ()
```

This will create following DOM inside document's body at run time:
```html
<div id="wrap">
  <div id="content">
    <p>Hello World!</p>
    <a href="https://github.com/geraldus/ghcjs-perch">Perch on GitHub</a>
  </div>
  <div id="footer">Happy Haskelling!</div>
</div>
```


This example program takes already existing DOM element, attaches click event
handler to it and adds some content:

```haskell
import Prelude hiding (div, span)

main :: IO ()
main =
  do forElemId_ "my-element" $
       do addEvent' this Click (\ _ -> print "Hello, world!")
          div $
            do span "GHC"
               span ! atr "style" "color:red" $ "JS"
  return ()
```

yields following DOM as result:

```html
<div id="my-element">   <!-- was already in the DOM -->
  "Hello World!"
  <div>
    <span>GHC</span>
    <span style="color:red">JS</span>
  </div>
</div>
```


Next example modifies the previously created elements when the event is raised,
the event handler is attched using CSS selector and modifies all elements with
`modify` class:

```haskell
main :: IO ()
main =
  do body <- getBody
     flip build body $
       do div ! atr "class" "modify" $ "click"
          div "not changed"
          div ! atr "class" "modify" $ "here"
          addEvent' this Click $ \_ ->
              forElems_ ".modify" $
                this ! style "color:red" `child` " modified"
     return ()
```


The monoid expression can also be used.  Concatenate elements with the `<>`
operator.  `term1 <> term2 <> ...` is equivalent to

```
do term1
   term2
   ...
```

Perch can also be used to navigate the tree, search, etc.

The monad instance provided in order to use do-notation.  This adds a new level
of syntax in the fashion of `blaze-html` package.  This monad invokes the same
appending mechanism.  But be aware that `Perch` is a fake-monad, it lacks of
bind function implementation and does not satisfies monad laws.

Perch is a generalization of a list and it is handled in the same way.

While a list is an unary tree, perch create N-ary trees.  Monoid instance of
list adds child nodes down, and this is the only direction list can grow.  While
perch monoid adds child horizontally at the same level.  To create down
branching there is `child` primitive.


## Few Words About Library Name ##

The basic element is a builder that has a "hole" parameter and an IO action
which creates DOM element.  That "hole" will be filled with parent element
created by the build action.  So builder can be considered like a perch that has
other perches that hang from it.  Either no one or an entire tree.

The call `nelem` (new element) is a perch that creates a single DOM element.
Upon created, it is added to the given parent and return itself as parent for
the next build actions that can be hooked from it using `child`.  When appending
two elements, both are added to the parent.

[haste-perch]:https://github.com/agocorona/haste-perch
[hplay]:https://github.com/agocorona/ghcjs-hplay
[blaze]:http://hackage.haskell.org/package/blaze-html
