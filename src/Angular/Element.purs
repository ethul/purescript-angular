module Angular.Element
  ( Element()
  , El()
  , Handler()
  , DeregisterHandler()
  , element
  , addClass
  , after
  , getAttr
  , setAttr
  , setAllAttr
  , bind
  , children
  , clone
  , contents
  , getCss
  , setCss
  , setAllCss
  , getData
  , setData
  , setAllData
  , empty
  , eq
  , find
  , hasClass
  , html
  , next
  , on
  , off
  , offHandler
  , one
  , parent
  , prepend
  , getProp
  , setProp
  , setAllProp
  , ready
  , remove
  , removeAttr
  , removeClass
  , removeData
  , replaceWith
  , toggleClass
  , triggerHandler
  , unbind
  , unbindHandler
  , getVal
  , setVal
  , wrap
  , controller
  , injector
  , scope
  , isolateScope
  , inheritedData
  , (!!)
  ) where

import Prelude (Unit())

import Control.Monad.Eff
import Data.Function (Fn3(), Fn4(), Fn5(), runFn3, runFn4, runFn5)
import Data.Maybe

import DOM.Event (Event())
import DOM.Node (Node())

import Angular.Injector (Injector())
import Angular.Scope (Scope())

type Handler e = Event -> Eff e Unit

foreign import data Element :: *

foreign import data El :: !

foreign import data DeregisterHandler :: # ! -> *

foreign import element
  " function element(el) { \
  \   return function(){ \
  \     return angular.element(el); \
  \   }; \
  \ }"
  :: forall e. String -> Eff e Element

foreign import addClass
  " function addClass(cssClasses){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.addClass(cssClasses); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Element -> Eff (ngel :: El | e) Element

foreign import after
  " function after(newEl){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.after(newEl); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. Element -> Element -> Eff (ngel :: El | e) Element

foreign import getAttr
  " function getAttr(name){ \
  \   return function(el){ \
  \     return function(){ \
  \       var a = el.attr(name); \
  \       return angular.isString(a) ? Data_Maybe.Just(a) : Data_Maybe.Nothing; \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Element -> Eff (ngel :: El | e) (Maybe String)

foreign import setAttr
  " function setAttr(name){ \
  \   return function(value){ \
  \     return function(el){ \
  \       return function(){ \
  \         return el.attr(name, value); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> String -> Element -> Eff (ngel :: El | e) Element

foreign import setAllAttr
  " function setAllAttr(obj){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.attr(obj); \
  \     }; \
  \   }; \
  \ }"
  :: forall e a. { | a } -> Element -> Eff (ngel :: El | e) Element

bind :: forall e f. String -> Handler f -> Element -> Eff (ngel :: El | e) (DeregisterHandler f)
bind = on

foreign import children
  " function children(el){ \
  \   return function(){ \
  \     return el.children(); \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) Element

foreign import clone
  " function clone(el){ \
  \   return function(){ \
  \     return el.clone(); \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) Element

foreign import contents
  " function contents(el){ \
  \   return function(){ \
  \     return el.contents(); \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) Element

foreign import getCss
  " function getCss(name){ \
  \   return function(el){ \
  \     return function(){ \
  \       var a = el.css(name); \
  \       return angular.isString(a) ? Data_Maybe.Just(a) : Data_Maybe.Nothing; \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Element -> Eff (ngel :: El | e) (Maybe String)

foreign import setCss
  " function setCss(name){ \
  \   return function(value){ \
  \     return function(el){ \
  \       return function(){ \
  \         return el.css(name, value); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> String -> Element -> Eff (ngel :: El | e) Element

foreign import setAllCss
  " function setAllCss(obj){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.css(obj); \
  \     }; \
  \   }; \
  \ }"
  :: forall e a. { | a } -> Element -> Eff (ngel :: El | e) Element

foreign import getData
  " function getData(name){ \
  \   return function(el){ \
  \     return function(){ \
  \       var a = el.data(name); \
  \       return angular.isDefined(a) ? Data_Maybe.Just(a) : Data_Maybe.Nothing; \
  \     }; \
  \   }; \
  \ }"
  :: forall e a. String -> Element -> Eff (ngel :: El | e) (Maybe a)

foreign import setData
  " function setData(name){ \
  \   return function(value){ \
  \     return function(el){ \
  \       return function(){ \
  \         return el.data(name, value); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e a. String -> a -> Element -> Eff (ngel :: El | e) Element

foreign import getAllData
  " function getAllData(el){ \
  \   return function(){ \
  \     return el.data(); \
  \   }; \
  \ }"
  :: forall e a. Element -> Eff (ngel :: El | e) { | a }

foreign import setAllData
  " function setAllData(obj){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.data(obj); \
  \     }; \
  \   }; \
  \ }"
  :: forall e a. { | a } -> Element -> Eff (ngel :: El | e) Element

foreign import empty
  " function empty(el){ \
  \   return function(){ \
  \     return el.empty(); \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) Element

foreign import eq
  " function eq(i){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.eq(i); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. Number -> Element -> Eff (ngel :: El | e) Element

foreign import find
  " function find(selector){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.find(selector); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. Number -> Element -> Eff (ngel :: El | e) Element

foreign import hasClass
  " function hasClass(selector){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.hasClass(selector); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Element -> Eff (ngel :: El | e) Boolean

foreign import html
  " function html(el){ \
  \   return function(){ \
  \     return el.html(); \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) String

foreign import next
  " function next(el){ \
  \   return function(){ \
  \     return el.next(); \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) Element

foreign import on
  " function on(events){ \
  \   return function(k){ \
  \     return function(el){ \
  \       return function(){ \
  \         var handler = function(event){return k(event)();}; \
  \         el.on(events, handler); \
  \         return handler; \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e f. String -> Handler f -> Element -> Eff (ngel :: El | e) (DeregisterHandler f)

foreign import off
  " function off(events){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.off(events); \
  \     }; \
  \   }; \
  \ }"
  :: forall e f. String -> Element -> Eff (ngel :: El | e) Element

foreign import offHandler
  " function offHandler(events){ \
  \   return function(k){ \
  \     return function(el){ \
  \       return function(){ \
  \         return el.off(events, k); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e f. String -> DeregisterHandler f -> Element -> Eff (ngel :: El | e) Element

foreign import one
  " function one(events){ \
  \   return function(k){ \
  \     return function(el){ \
  \       return function(){ \
  \         var handler = function(event){return k(event)();}; \
  \         el.one(events, handler); \
  \         return handler; \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e f. String -> Handler f -> Element -> Eff (ngel :: El | e) (DeregisterHandler f)

foreign import parent
  " function parent(el){ \
  \   return function(){ \
  \     return el.parent(); \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) Element

foreign import prepend
  " function prepend(newEl){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.prepend(newEl); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. Element -> Element -> Eff (ngel :: El | e) Element

foreign import getProp
  " function getProp(name){ \
  \   return function(el){ \
  \     return function(){ \
  \       var a = el.prop(name); \
  \       return angular.isString(a) ? Data_Maybe.Just(a) : Data_Maybe.Nothing; \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Element -> Eff (ngel :: El | e) (Maybe String)

foreign import setProp
  " function setProp(name){ \
  \   return function(value){ \
  \     return function(el){ \
  \       return function(){ \
  \         return el.prop(name, value); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> String -> Element -> Eff (ngel :: El | e) Element

foreign import setAllProp
  " function setAllProp(obj){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.prop(obj); \
  \     }; \
  \   }; \
  \ }"
  :: forall e a. { | a } -> Element -> Eff (ngel :: El | e) Element

foreign import ready
  " function ready(k){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.ready(k); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. Eff e Unit -> Element -> Eff (ngel :: El | e) Element

foreign import remove
  " function remove(el){ \
  \   return function(){ \
  \     return el.remove(); \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) Element

foreign import removeAttr
  " function removeAttr(name){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.removeAttr(name); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Element -> Eff (ngel :: El | e) Element

foreign import removeClass
  " function removeClass(name){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.removeClass(name); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Element -> Eff (ngel :: El | e) Element

foreign import removeData
  " function removeData(name){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.removeData(name); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Element -> Eff (ngel :: El | e) Element

foreign import replaceWith
  " function replaceWith(newEl){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.replaceWith(newEl); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. Element -> Element -> Eff (ngel :: El | e) Element

foreign import text
  " function text(el){ \
  \   return function(){ \
  \     return el.text(); \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) String

foreign import toggleClass
  " function toggleClass(selector){ \
  \   return function(condition){ \
  \     return function(el){ \
  \       return function(){ \
  \         return el.toggleClass(selector, condition); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Boolean -> Element -> Eff (ngel :: El | e) Element

foreign import triggerHandler
  " function triggerHandler(eventName){ \
  \   return function(eventData){ \
  \     return function(el){ \
  \       return function(){ \
  \         return el.triggerHandler(eventName, eventData); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  :: forall e a. String -> [a] -> Element -> Eff (ngel :: El | e) Element

unbind :: forall e f. String -> Element -> Eff (ngel :: El | e) Element
unbind = off

unbindHandler :: forall e f. String -> DeregisterHandler f -> Element -> Eff (ngel :: El | e) Element
unbindHandler = offHandler

foreign import getVal
  " function getVal(el){ \
  \   return function(){ \
  \     var a = el.val(); \
  \     return angular.isString(a) ? Data_Maybe.Just(a) : Data_Maybe.Nothing; \
  \   }; \
  \ }"
  :: forall e. Element -> Eff (ngel :: El | e) (Maybe String)

foreign import setVal
  " function setVal(value){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.val(value); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. String -> Element -> Eff (ngel :: El | e) Element

foreign import wrap
  " function wrap(newEl){ \
  \   return function(el){ \
  \     return function(){ \
  \       return el.wrap(newEl); \
  \     }; \
  \   }; \
  \ }"
  :: forall e. Element -> Element -> Eff (ngel :: El | e) Element

foreign import controllerFn
  " function controllerFn(fromMaybe, nothing, just, name, el){ \
  \   return function(){ \
  \     var a = el.controller(fromMaybe(undefined)(name)); \
  \     return angular.isDefined(a) ? just(a) : nothing; \
  \   }; \
  \ }"
  :: forall e a. Fn5 (String -> Maybe String -> String)
                     (Maybe a)
                     (a -> Maybe a)
                     (Maybe String)
                     Element
                     (Eff (ngel :: El | e) (Maybe a))

controller :: forall e a. Maybe String -> Element -> Eff (ngel :: El | e) (Maybe a)
controller = runFn5 controllerFn fromMaybe Nothing Just

foreign import injectorFn
  " function injectorFn(nothing, just, el){ \
  \   return function(){ \
  \     var a = el.injector(); \
  \     return angular.isDefined(a) ? just(a) : nothing; \
  \   }; \
  \ }"
  :: forall e a. Fn3 (Maybe Injector) (Injector -> Maybe Injector) Element (Eff (ngel :: El | e) (Maybe Injector))

injector :: forall e a. Element -> Eff (ngel :: El | e) (Maybe Injector)
injector = runFn3 injectorFn Nothing Just

foreign import scopeFn
  " function scopeFn(nothing, just, el){ \
  \   return function(){ \
  \     var a = el.scope(); \
  \     return angular.isDefined(a) ? just(a) : nothing; \
  \   }; \
  \ }"
  :: forall e a. Fn3 (Maybe (Scope a)) (Scope a -> Maybe (Scope a)) Element (Eff (ngel :: El | e) (Maybe (Scope a)))

scope :: forall e a. Element -> Eff (ngel :: El | e) (Maybe (Scope a))
scope = runFn3 scopeFn Nothing Just

foreign import isolateScopeFn
  " function isolateScopeFn(nothing, just, el){ \
  \   return function(){ \
  \     var a = el.isolateScope(); \
  \     return angular.isDefined(a) ? just(a) : nothing; \
  \   }; \
  \ }"
  :: forall e a. Fn3 (Maybe (Scope a)) (Scope a -> Maybe (Scope a)) Element (Eff (ngel :: El | e) (Maybe (Scope a)))

isolateScope :: forall e a. Element -> Eff (ngel :: El | e) (Maybe (Scope a))
isolateScope = runFn3 isolateScopeFn Nothing Just

foreign import inheritedData
  " function inheritedData(el){ \
  \   return function(){ \
  \     return el.inheritedData(); \
  \   }; \
  \ }"
  :: forall e a. Element -> Eff (ngel :: El | e) { | a }

foreign import bangbangFn
  " function bangbangFn(nothing, just, el, i) { \
  \   var r = el[i]; \
  \   return angular.isDefined(r) ? just(r) : nothing; \
  \ }"
  :: Fn4 (Maybe Node) (Node -> Maybe Node) Element Number (Maybe Node)

infixl 8 !!

(!!) :: Element -> Number -> Maybe Node
(!!) = runFn4 bangbangFn Nothing Just
