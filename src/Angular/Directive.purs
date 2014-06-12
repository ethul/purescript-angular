module Angular.Directive where

import Control.Monad.Eff
import Data.Maybe

import Angular.Attributes (Attributes(..))
import Angular.Controller (ConstructorFnEff(..), This(..))
import Angular.Element (Element(..))
import Angular.Injector (InjectDependency(..))
import Angular.Module (Module(..), WriteModule(..))
import Angular.Scope (Scope(..))

type TranscludeFn e a b = Scope a -> (Element -> Scope b -> Eff e Unit) -> Eff e Unit

type Controllers a = { | a }

type LinkFn e a b c d = Scope a -> Element -> Attributes -> Controllers b -> Maybe (TranscludeFn e c d) -> Eff e Unit

type CompileFn e a b c d = Element -> Attributes -> Eff e (Link e a b c d)

type Url = String

type TemplateFn a = Element -> Attributes -> a

type ConstructorFn e r a b c d s t
  =  Scope a
  -> This b
  -> Element
  -> Attributes
  -> Maybe (TranscludeFn e s t)
  -> ConstructorFnEff e r a b c d

data Link e a b c d
  = PostLink (LinkFn e a b c d)
  | PrePostLink (LinkFn e a b c d) (LinkFn e a b c d) -- | { pre , post }
  | DefaultLink                                       -- | undefined

data Compile e a b c d
  = FnCompile (CompileFn e a b c d)
  | DefaultCompile -- | undefined

data Template
  = StringTemplate String
  | FnTemplate (TemplateFn String)
  | DefaultTemplate

data TemplateUrl
  = UrlTemplateUrl Url
  | FnTemplateUrl (TemplateFn Url)
  | DefaultTemplateUrl

data Restrict
  = RestrictElementName
  | RestrictAttribute
  | RestrictClass
  | RestrictComment

data Require
  = ElementRequire String               -- | directive
  | OptionalElementRequire String       -- | ?directive
  | ParentElementRequire String         -- | ^directive
  | OptionalParentElementRequire String -- | ?^directive

data ScopeBinding
  = UnidirectionalBinding String (Maybe String)         -- | @attr
  | BidirectionalBinding String (Maybe String)          -- | =attr
  | OptionalBidirectionalBinding String (Maybe String)  -- | =?attr
  | ExpressionBinding String (Maybe String)             -- | &attr

data DirectiveScope
  = NewScope                    -- | true
  | SameScope                   -- | false
  | IsolateScope [ScopeBinding] -- | { ... }

data DirectiveController e r a b c d s t
  = NamedController String                                   -- | controller name
  | DirectiveNameController                                  -- | @
  | ConstructorFnController (ConstructorFn e r a b c d s t)  -- | ['$scope', ... , function($scope, this, ...,
  | DefaultDirectiveController                               -- | undefined

data Transclude
  = ContentTransclude   -- | true
  | NoContentTransclude -- | false
  | ElementTransclude   -- | 'element'

type DirectiveDefinition e r a b c d s t u
  = { priority :: Number
    , terminal :: Boolean
    , scope :: DirectiveScope
    , controller :: DirectiveController e r a b c d t u
    , require :: [Require]
    , controllerAs :: Maybe String
    , restrict :: [Restrict]
    , template :: Template
    , templateUrl :: TemplateUrl
    , replace :: Boolean
    , transclude :: Transclude
    , compile :: Compile e a s t u
    , link :: Link e a s t u
    }

elementRequire :: String -> Require
elementRequire = ElementRequire

optionalElementRequire :: String -> Require
optionalElementRequire = OptionalElementRequire

parentElementRequire :: String -> Require
parentElementRequire = ParentElementRequire

optionalParentElementRequire :: String -> Require
optionalParentElementRequire = OptionalParentElementRequire

newScope :: DirectiveScope
newScope = NewScope

sameScope :: DirectiveScope
sameScope = SameScope

isolateScope :: [ScopeBinding] -> DirectiveScope
isolateScope = IsolateScope

unidirectionalBinding :: String -> ScopeBinding
unidirectionalBinding s = UnidirectionalBinding s Nothing

unidirectionalBinding' :: String -> String -> ScopeBinding
unidirectionalBinding' s t = UnidirectionalBinding s $ Just t

bidirectionalBinding :: String -> ScopeBinding
bidirectionalBinding s = BidirectionalBinding s Nothing

bidirectionalBinding' :: String -> String -> ScopeBinding
bidirectionalBinding' s t = BidirectionalBinding s $ Just t

optionalBidirectionalBinding :: String -> ScopeBinding
optionalBidirectionalBinding s = OptionalBidirectionalBinding s Nothing

optionalBidirectionalBinding' :: String -> String -> ScopeBinding
optionalBidirectionalBinding' s t = OptionalBidirectionalBinding s $ Just t

expressionBinding :: String -> ScopeBinding
expressionBinding s = ExpressionBinding s Nothing

expressionBinding' :: String -> String -> ScopeBinding
expressionBinding' s t = ExpressionBinding s $ Just t

compile :: forall e a b c d. CompileFn e a b c d -> Compile e a b c d
compile = FnCompile

postLink :: forall e a b c d. LinkFn e a b c d -> Link e a b c d
postLink = PostLink

prePostLink :: forall e a b c d. LinkFn e a b c d -> LinkFn e a b c d -> Link e a b c d
prePostLink = PrePostLink

template :: String -> Template
template = StringTemplate

templateFn :: TemplateFn String -> Template
templateFn = FnTemplate

templateUrl :: Url -> TemplateUrl
templateUrl = UrlTemplateUrl

templateUrlFn :: TemplateFn Url -> TemplateUrl
templateUrlFn = FnTemplateUrl

namedController :: forall e r a b c d s t. String -> DirectiveController e r a b c d s t
namedController = NamedController

directiveNameController :: forall e r a b c d s t. DirectiveController e r a b c d s t
directiveNameController = DirectiveNameController

controller :: forall e r a b c d s t. ConstructorFn e r a b c d s t -> DirectiveController e r a b c d s t
controller = ConstructorFnController

defn :: forall e r a b c d s t u. DirectiveDefinition e r a b c d s t u
defn = { priority: 0
       , terminal: false
       , scope: SameScope
       , controller: DefaultDirectiveController
       , require: []
       , controllerAs: Nothing
       , restrict: [RestrictAttribute]
       , template: DefaultTemplate
       , templateUrl: DefaultTemplateUrl
       , replace: false
       , transclude: NoContentTransclude
       , compile: DefaultCompile
       , link: DefaultLink
       }

foreign import directive
  " function __scope__(a){ \
  \   if (a.ctor === NewScope.ctor) return true; \
  \   else if (a.ctor === SameScope.ctor) return false; \
  \   else if (a.ctor === IsolateScope().ctor) { \
  \     var res = {}; \
  \     angular.forEach(a.values[0], function(b){ \
  \       if (b.ctor === UnidirectionalBinding()().ctor) res[b.values[0]] = '@' + (b.values[1].values[0] || ''); \
  \       else if (b.ctor === BidirectionalBinding()().ctor) res[b.values[0]] = '=' + (b.values[1].values[0] || ''); \
  \       else if (b.ctor === OptionalBidirectionalBinding()().ctor) res[b.values[0]] = '=?' + (b.values[1].values[0] || ''); \
  \       else if (b.ctor === ExpressionBinding()().ctor) res[b.values[0]] = '&' + (b.values[1].values[0] || ''); \
  \       else throw 'Failed pattern match'; \
  \     }); \
  \     return res; \
  \   } \
  \   else throw 'Failed pattern match'; \
  \ } \
  \ \
  \ function __restrict__(a){ \
  \   var res = []; \
  \   angular.forEach(a, function(b){ \
  \     if (b.ctor === RestrictElementName.ctor) res.push('E'); \
  \     else if (b.ctor === RestrictAttribute.ctor) res.push('A'); \
  \     else if (b.ctor === RestrictClass.ctor) res.push('C'); \
  \     else if (b.ctor === RestrictComment.ctor) res.push('M'); \
  \     else throw 'Failed pattern match'; \
  \   }); \
  \   return res; \
  \ } \
  \ \
  \ function __require__(a){ \
  \   var res = []; \
  \   angular.forEach(a, function(b){ \
  \     if (b.ctor === ElementRequire().ctor) res.push(b.values[0]); \
  \     else if (b.ctor === OptionalElementRequire().ctor) res.push('?' + b.values[0]); \
  \     else if (b.ctor === ParentElementRequire().ctor) res.push('^' + b.values[0]); \
  \     else if (b.ctor === OptionalParentElementRequire().ctor) res.push('?^' + b.values[0]); \
  \     else throw 'Failed pattern match'; \
  \   }); \
  \   return res; \
  \ } \
  \ \
  \ function __transclude__(a){ \
  \   if (a.ctor === ContentTransclude.ctor) return true; \
  \   else if (a.ctor === NoContentTransclude.ctor) return false; \
  \   else if (a.ctor === ElementTransclude.ctor) return 'element'; \
  \   else throw 'Failed pattern match'; \
  \ } \
  \ \
  \ function __trans__(t){ \
  \   return function(scope){ \
  \     return function(k){ \
  \       return t(scope, function(clone, scope$prime){ \
  \         return k(clone)(scope$prime)(); \
  \       }); \
  \     }; \
  \   }; \
  \ } \
  \ \
  \ function __link__(requires, k){ \
  \   var ctrls = function(cs){\
  \     var res = {}; \
  \     angular.forEach(angular.isArray(cs) ? cs : [], function(v, i){ \
  \       res[requires[i].values[0]] = v; \
  \     }); \
  \     return res; \
  \   };\
  \   if (k.ctor === PostLink().ctor) { \
  \     return function(a,b,c,d,e){return k.values[0](a)(b)(c)(ctrls(d))(e ? Data_Maybe.Just(__trans__(e)) : Data_Maybe.Nothing)();}; \
  \   } \
  \   else if (k.ctor === PrePostLink()().ctor) { \
  \     return { \
  \       pre: function(a,b,c,d,e){return k.values[0](a)(b)(c)(ctrls(d))(e ? Data_Maybe.Just(__trans__(e)) : Data_Maybe.Nothing)();}, \
  \       post: function(a,b,c,d,e){return k.values[0](a)(b)(c)(ctrls(d))(e ? Data_Maybe.Just(__trans__(e)) : Data_Maybe.Nothing)();} \
  \     }; \
  \   } \
  \   else if (k.ctor === DefaultLink.ctor) return undefined; \
  \   else throw 'Failed pattern match'; \
  \ } \
  \ \
  \ function __compile__(requires, k){ \
  \   if (k.ctor === FnCompile().ctor) { \
  \     return function(a,b,_){return __link__(requires, k.values[0](a)(b)());}; \
  \   } \
  \   else if (k.ctor === DefaultCompile.ctor) return undefined; \
  \   else throw 'Failed pattern match'; \
  \ } \
  \ \
  \ function __template__(k){ \
  \   if (k.ctor === FnTemplate().ctor) return function(a,b){return k.values[0](a)(b);}; \
  \   else if (k.ctor === StringTemplate().ctor) return k.values[0]; \
  \   else if (k.ctor === DefaultTemplate.ctor) return undefined; \
  \   else throw 'Failed pattern match'; \
  \ } \
  \ \
  \ function __templateUrl__(k){ \
  \   if (k.ctor === FnTemplateUrl().ctor) return function(a,b){return k.values[0](a)(b);}; \
  \   else if (k.ctor === UrlTemplateUrl().ctor) return k.values[0]; \
  \   else if (k.ctor === DefaultTemplateUrl.ctor) return undefined; \
  \   else throw 'Failed pattern match'; \
  \ } \
  \ function __controller__(k){ \
  \   if (k.ctor === NamedController().ctor) return k.values[0]; \
  \   else if (k.ctor === DirectiveNameController.ctor) return '@'; \
  \   else if (k.ctor === ConstructorFnController().ctor) return [ \
  \     '$scope', '$element', '$attrs', '$transclude', \
  \     function($scope, $element, $attrs, $transclude){ \
  \       k.values[0]($scope)(this)($element)($attrs)($transclude ? Data_Maybe.Just(__trans__($transclude)) : Data_Maybe.Nothing)(); \
  \     } \
  \   ]; \
  \   else if (k.ctor === DefaultDirectiveController.ctor) return undefined; \
  \   else throw 'Failed pattern match'; \
  \ } \
  \ function __controllerAs__(k){ \
  \   if (k.ctor === Data_Maybe.Just().ctor) return k.values[0]; \
  \   else if (k.ctor === Data_Maybe.Nothing.ctor) return undefined; \
  \   else throw 'Failed pattern match'; \
  \ } \
  \ \
  \ function directive(name){ \
  \   return function(module){ \
  \     return function(defnk){ \
  \       return function(){ \
  \         return module.directive(name, function(){ \
  \           var defn = defnk() \
  \             , res = angular.extend({}, defn, { \
  \                 scope: __scope__(defn.scope), \
  \                 controller: __controller__(defn.controller), \
  \                 controllerAs: __controllerAs__(defn.controllerAs), \
  \                 restrict: __restrict__(defn.restrict), \
  \                 require: __require__(defn.require), \
  \                 template: __template__(defn.template), \
  \                 templateUrl: __templateUrl__(defn.templateUrl), \
  \                 transclude: __transclude__(defn.transclude), \
  \                 compile: __compile__(defn.require, defn.compile), \
  \                 link: __link__(defn.require, defn.link) \
  \               }) \
  \           ; \
  \           return res; \
  \         }); \
  \       }; \
  \     }; \
  \   }; \
  \ }"
  ::
  forall e r a b c d s t u
  .  String
  -> Module
  -> Eff (nginj :: InjectDependency | e) (DirectiveDefinition e r a b c d s t u)
  -> Eff (ngwmod :: WriteModule | e) Module
