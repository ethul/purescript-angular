# Module Documentation

## Module Angular

### Values

    bootstrap :: forall e. Element -> [Prim.String] -> Eff e Injector

    copy :: forall e a. a -> Eff e a

    extend :: forall e a b c. {  | a } -> {  | b } -> Eff e {  | c }


## Module Angular.Attributes

### Types

    data Attr :: !

    data Attributes :: *


### Values

    addClass :: forall e. Prim.String -> Attributes -> Eff (ngattr :: Attr | e) Unit

    attr :: forall e a. Attributes -> Eff (ngattr :: Attr | e) {  | a }

    get :: forall e a. Attributes -> Eff (ngattr :: Attr | e) {  | a }

    observe :: forall e f. Prim.String -> (Prim.String -> Eff f Unit) -> Attributes -> Eff (ngattr :: Attr | e) Unit

    removeClass :: forall e. Prim.String -> Attributes -> Eff (ngattr :: Attr | e) Unit

    set :: forall e. Prim.String -> Prim.String -> Attributes -> Eff (ngattr :: Attr | e) Unit

    updateClass :: forall e. Prim.String -> Prim.String -> Attributes -> Eff (ngattr :: Attr | e) Unit


## Module Angular.Controller

### Types

    data ReadScopeState :: # * -> !

    data ReadThisState :: # * -> !

    type ScopeState a = {  | a }

    data This :: # * -> *

    type ThisState a = {  | a }

    data WriteScopeState :: # * -> !

    data WriteThisState :: # * -> !


### Values

    controller :: forall e r a b c d. Prim.String -> Module -> (Scope a -> This b -> Eff (nginj :: InjectDependency, ngwthis :: WriteThisState d, ngwscope :: WriteScopeState c, ngrthis :: ReadThisState b, ngrscope :: ReadScopeState a | e) r) -> Eff (ngwmod :: WriteModule | e) Module

    readScopeState :: forall e a. Scope a -> Eff (ngrscope :: ReadScopeState a | e) (ScopeState a)

    readThisState :: forall e a. This a -> Eff (ngrthis :: ReadThisState a | e) (ThisState a)

    writeScopeState :: forall e a b. ScopeState b -> Scope a -> Eff (ngwscope :: WriteScopeState a | e) (Scope b)

    writeThisState :: forall e a b. ThisState b -> This a -> Eff (ngwthis :: WriteThisState a | e) (This b)


## Module Angular.Directive

### Types

    data Compile e a where
      FnCompile :: CompileFn e a -> Compile e a
      DefaultCompile :: Compile e a

    type CompileFn e a = Element -> Attributes -> Eff e (Link e a)

    type Controller  = Unit

    data DirectiveController e where
      NamedController :: Prim.String -> DirectiveController e
      DirectiveNameController :: DirectiveController e
      ConstructorFnController :: Eff (i :: InjectDependency | e) Unit -> DirectiveController e
      DefaultDirectiveController :: DirectiveController e

    type DirectiveDefinition e f g a b c = { link :: Link g c, compile :: Compile f b, transclude :: Transclude, replace :: Prim.Boolean, templateUrl :: TemplateUrl, template :: Template, restrict :: [Restrict], controllerAs :: Maybe Prim.String, require :: [Require], controller :: DirectiveController e, scope :: DirectiveScope, terminal :: Prim.Boolean, priority :: Prim.Number }

    data DirectiveScope  where
      NewScope :: DirectiveScope 
      SameScope :: DirectiveScope 
      IsolateScope :: [ScopeBinding] -> DirectiveScope 

    data Link e a where
      PostLink :: LinkFn e a -> Link e a
      PrePostLink :: LinkFn e a -> LinkFn e a -> Link e a
      DefaultLink :: Link e a

    type LinkFn e a = Scope a -> Element -> Attributes -> [Controller] -> Maybe TranscludeFn -> Eff e Unit

    data Require  where
      ElementRequire :: Prim.String -> Require 
      OptionalElementRequire :: Prim.String -> Require 
      ParentElementRequire :: Prim.String -> Require 
      OptionalParentElementRequire :: Prim.String -> Require 

    data Restrict  where
      RestrictElementName :: Restrict 
      RestrictAttribute :: Restrict 
      RestrictClass :: Restrict 
      RestrictComment :: Restrict 

    data ScopeBinding  where
      UnidirectionalBinding :: Prim.String -> Maybe Prim.String -> ScopeBinding 
      BidirectionalBinding :: Prim.String -> Maybe Prim.String -> ScopeBinding 
      OptionalBidirectionalBinding :: Prim.String -> Maybe Prim.String -> ScopeBinding 
      ExpressionBinding :: Prim.String -> Maybe Prim.String -> ScopeBinding 

    data Template  where
      StringTemplate :: Prim.String -> Template 
      FnTemplate :: TemplateFn Prim.String -> Template 
      DefaultTemplate :: Template 

    type TemplateFn a = Element -> Attributes -> a

    data TemplateUrl  where
      UrlTemplateUrl :: Url -> TemplateUrl 
      FnTemplateUrl :: TemplateFn Url -> TemplateUrl 
      DefaultTemplateUrl :: TemplateUrl 

    data Transclude  where
      ContentTransclude :: Transclude 
      NoContentTransclude :: Transclude 
      ElementTransclude :: Transclude 

    type TranscludeFn  = Unit

    type Url  = Prim.String


### Values

    bidirectionalBinding :: Prim.String -> ScopeBinding

    bidirectionalBinding' :: Prim.String -> Prim.String -> ScopeBinding

    compile :: forall e a. CompileFn e a -> Compile e a

    controller :: forall e. Eff (i :: InjectDependency | e) Unit -> DirectiveController e

    defn :: forall e f g a b c. DirectiveDefinition e f g a b c

    directive :: forall e f g h i a b c. Prim.String -> Module -> Eff (nginj :: InjectDependency | e) (DirectiveDefinition f g h a b c) -> Eff (ngwmod :: WriteModule | i) Module

    directiveNameController :: forall e. DirectiveController e

    expressionBinding :: Prim.String -> ScopeBinding

    expressionBinding' :: Prim.String -> Prim.String -> ScopeBinding

    isolateScope :: [ScopeBinding] -> DirectiveScope

    namedController :: forall e. Prim.String -> DirectiveController e

    newScope :: DirectiveScope

    optionalBidirectionalBinding :: Prim.String -> ScopeBinding

    optionalBidirectionalBinding' :: Prim.String -> Prim.String -> ScopeBinding

    postLink :: forall e a. LinkFn e a -> Link e a

    prePostLink :: forall e a. LinkFn e a -> LinkFn e a -> Link e a

    sameScope :: DirectiveScope

    template :: Prim.String -> Template

    templateFn :: TemplateFn Prim.String -> Template

    templateUrl :: Url -> TemplateUrl

    templateUrlFn :: TemplateFn Url -> TemplateUrl

    unidirectionalBinding :: Prim.String -> ScopeBinding

    unidirectionalBinding' :: Prim.String -> Prim.String -> ScopeBinding


## Module Angular.Element

### Types

    type Controller a = Unit

    data DeregisterHandler :: # ! -> *

    data El :: !

    data Element :: *

    type Handler e = Event -> Eff e Unit


### Values

    (!!) :: Element -> Prim.Number -> Maybe Node

    addClass :: forall e. Prim.String -> Element -> Eff (ngel :: El | e) Element

    after :: forall e. Element -> Element -> Eff (ngel :: El | e) Element

    bind :: forall e f. Prim.String -> Handler f -> Element -> Eff (ngel :: El | e) (DeregisterHandler f)

    children :: forall e. Element -> Eff (ngel :: El | e) Element

    clone :: forall e. Element -> Eff (ngel :: El | e) Element

    contents :: forall e. Element -> Eff (ngel :: El | e) Element

    controller :: forall e a. Maybe Prim.String -> Element -> Eff (ngel :: El | e) (Maybe (Controller a))

    element :: forall e. Prim.String -> Eff e Element

    empty :: forall e. Element -> Eff (ngel :: El | e) Element

    eq :: forall e. Prim.Number -> Element -> Eff (ngel :: El | e) Element

    find :: forall e. Prim.Number -> Element -> Eff (ngel :: El | e) Element

    getAllData :: forall e a. Element -> Eff (ngel :: El | e) {  | a }

    getAttr :: forall e. Prim.String -> Element -> Eff (ngel :: El | e) (Maybe Prim.String)

    getCss :: forall e. Prim.String -> Element -> Eff (ngel :: El | e) (Maybe Prim.String)

    getData :: forall e a. Prim.String -> Element -> Eff (ngel :: El | e) (Maybe a)

    getProp :: forall e. Prim.String -> Element -> Eff (ngel :: El | e) (Maybe Prim.String)

    getVal :: forall e. Element -> Eff (ngel :: El | e) (Maybe Prim.String)

    hasClass :: forall e. Prim.String -> Element -> Eff (ngel :: El | e) Prim.Boolean

    html :: forall e. Element -> Eff (ngel :: El | e) Prim.String

    inheritedData :: forall e a. Element -> Eff (ngel :: El | e) {  | a }

    injector :: forall e a. Element -> Eff (ngel :: El | e) (Maybe Injector)

    isolateScope :: forall e a. Element -> Eff (ngel :: El | e) (Maybe (Scope a))

    next :: forall e. Element -> Eff (ngel :: El | e) Element

    off :: forall e f. Prim.String -> Element -> Eff (ngel :: El | e) Element

    offHandler :: forall e f. Prim.String -> DeregisterHandler f -> Element -> Eff (ngel :: El | e) Element

    on :: forall e f. Prim.String -> Handler f -> Element -> Eff (ngel :: El | e) (DeregisterHandler f)

    one :: forall e f. Prim.String -> Handler f -> Element -> Eff (ngel :: El | e) (DeregisterHandler f)

    parent :: forall e. Element -> Eff (ngel :: El | e) Element

    prepend :: forall e. Element -> Element -> Eff (ngel :: El | e) Element

    ready :: forall e. Eff e Unit -> Element -> Eff (ngel :: El | e) Element

    remove :: forall e. Element -> Eff (ngel :: El | e) Element

    removeAttr :: forall e. Prim.String -> Element -> Eff (ngel :: El | e) Element

    removeClass :: forall e. Prim.String -> Element -> Eff (ngel :: El | e) Element

    removeData :: forall e. Prim.String -> Element -> Eff (ngel :: El | e) Element

    replaceWith :: forall e. Element -> Element -> Eff (ngel :: El | e) Element

    scope :: forall e a. Element -> Eff (ngel :: El | e) (Maybe (Scope a))

    setAllAttr :: forall e a. {  | a } -> Element -> Eff (ngel :: El | e) Element

    setAllCss :: forall e a. {  | a } -> Element -> Eff (ngel :: El | e) Element

    setAllData :: forall e a. {  | a } -> Element -> Eff (ngel :: El | e) Element

    setAllProp :: forall e a. {  | a } -> Element -> Eff (ngel :: El | e) Element

    setAttr :: forall e. Prim.String -> Prim.String -> Element -> Eff (ngel :: El | e) Element

    setCss :: forall e. Prim.String -> Prim.String -> Element -> Eff (ngel :: El | e) Element

    setData :: forall e a. Prim.String -> a -> Element -> Eff (ngel :: El | e) Element

    setProp :: forall e. Prim.String -> Prim.String -> Element -> Eff (ngel :: El | e) Element

    setVal :: forall e. Prim.String -> Element -> Eff (ngel :: El | e) Element

    text :: forall e. Element -> Eff (ngel :: El | e) Prim.String

    toggleClass :: forall e. Prim.String -> Prim.Boolean -> Element -> Eff (ngel :: El | e) Element

    triggerHandler :: forall e a. Prim.String -> [a] -> Element -> Eff (ngel :: El | e) Element

    unbind :: forall e f. Prim.String -> Element -> Eff (ngel :: El | e) Element

    unbindHandler :: forall e f. Prim.String -> DeregisterHandler f -> Element -> Eff (ngel :: El | e) Element

    wrap :: forall e. Element -> Element -> Eff (ngel :: El | e) Element


## Module Angular.Http

### Types

    type Config a b = { "data" :: Maybe b, params :: Maybe a, url :: Url, method :: Method }

    data HTTP :: !

    data Http :: *

    data Method  where

    type Response a b c = { statusText :: Prim.String, config :: Config a b, headers :: [Prim.String] -> Prim.String, status :: Status, "data" :: c }

    data ResponseType  where

    data Status  where

    type Url  = Prim.String


### Type Class Instances

    instance showMethod :: Show Method

    instance showResponseType :: Show ResponseType


### Values

    defaultConfig :: forall a b. Config a b

    delete' :: forall e a b c. Url -> Maybe (Config a b) -> Http -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

    get :: forall e a b c. Url -> Maybe (Config a b) -> Http -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

    head :: forall e a b c. Url -> Maybe (Config a b) -> Http -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

    http :: forall e a b c. (Show Method) => Config a b -> Http -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

    injHttp :: forall e. Eff (nginj :: InjectDependency | e) Http

    jsonp :: forall e a b c. Url -> Maybe (Config a b) -> Http -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

    post :: forall e a b c. Url -> b -> Maybe (Config a b) -> Http -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

    put :: forall e a b c. Url -> b -> Maybe (Config a b) -> Http -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))


## Module Angular.Injector

### Types

    data InjectDependency :: !

    data Injector :: *


### Values

    injector :: forall e. Eff e Injector


## Module Angular.Location

### Types

    data Loc :: !

    data Location :: *


### Values

    getPath :: forall e. Location -> Eff (ngloc :: Loc | e) Prim.String

    location :: forall e. Eff (nginj :: InjectDependency | e) Location

    setPath :: forall e. Prim.String -> Location -> Eff (ngloc :: Loc | e) Prim.String


## Module Angular.Module

### Types

    data Module :: *

    data ReadModule :: !

    data RegisterModule :: !

    data WriteModule :: !


### Values

    constant :: forall e a. Prim.String -> a -> Module -> Eff (nggmod :: RegisterModule | e) Module

    factory :: forall e a. Prim.String -> Eff (nginj :: InjectDependency | e) {  | a } -> Module -> Eff (nggmod :: RegisterModule | e) Module

    getModule :: forall e. Prim.String -> Eff (ngrmod :: ReadModule | e) Module

    newModule :: forall e. Prim.String -> [Prim.String] -> Eff (ngwmod :: WriteModule | e) Module

    provider :: forall e a b. Prim.String -> Eff (nginj :: InjectDependency | e) { "$get" :: Eff (nginj :: InjectDependency | e) a | b } -> Module -> Eff (nggmod :: RegisterModule | e) Module

    service :: forall e a. Prim.String -> Eff (nginj :: InjectDependency | e) {  | a } -> Module -> Eff (nggmod :: RegisterModule | e) Module

    value :: forall e a. Prim.String -> a -> Module -> Eff (nggmod :: RegisterModule | e) Module


## Module Angular.Q

### Types

    data Promise :: * -> *

    data Q :: *


### Type Class Instances

    instance applicativePromise :: Applicative Promise

    instance applyPromise :: Apply Promise

    instance bindPromise :: Bind Promise

    instance functorPromise :: Functor Promise

    instance monadPromise :: Monad Promise


### Values

    all :: forall a. [Promise a] -> Q -> Promise [a]

    catch' :: forall a b c. Promise a -> (b -> Promise c) -> Promise c

    finally' :: forall a b. Promise a -> (Unit -> b) -> Promise a

    reject :: forall a. a -> Q -> Promise a

    resolve :: forall a. a -> Q -> Promise a

    then' :: forall a b. Promise a -> (a -> Promise b) -> Promise b

    then'' :: forall a b c. Promise a -> (a -> Promise b) -> (c -> Promise b) -> Promise b

    then''' :: forall a b c d. Promise a -> (a -> Promise b) -> (b -> Promise b) -> (c -> d) -> Promise b

    when :: forall a. a -> Q -> Promise a


## Module Angular.ST

### Values

    pushAllSTArray :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) Prim.Number

    pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Prim.Number

    readSTArray :: forall a h r. STArray h a -> Eff (st :: ST h | r) [a]

    spliceSTArray :: forall a h r. STArray h a -> Prim.Number -> Prim.Number -> [a] -> Eff (st :: ST h | r) [a]

    writeSTArray :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) [a]


## Module Angular.Scope

### Types

    data ApplyExpr e r a where
      FnApplyExpr :: Scope a -> Eff e r -> ApplyExpr e r a
      StringApplyExpr :: Prim.String -> ApplyExpr e r a
      DefaultApplyExpr :: ApplyExpr e r a

    type Event e a b = { defaultPrevented :: Prim.Boolean, preventDefault :: Eff e Unit, stopPropagation :: Eff e Unit, name :: Prim.String, currentScope :: Scope b, targetScope :: Scope a }

    data OnDeregistration :: *

    data Scope :: # * -> *

    data WatchDeregistration :: *

    type WatchListener e a b = a -> a -> Scope b -> Eff e Unit


### Values

    apply :: forall e r a. ApplyExpr e r a -> Scope a -> Eff e r

    applyExpr :: forall e r a. (Scope a -> Eff e r) -> ApplyExpr e r a

    broadcast :: forall e a b c. Prim.String -> a -> Scope b -> Eff e (Event e b c)

    defaultApplyExpr :: forall e r a. ApplyExpr e r a

    deregisterOn :: OnDeregistration -> Unit

    deregisterWatch :: WatchDeregistration -> Unit

    destroy :: forall e a. Scope a -> Eff e Unit

    digest :: forall e a. Scope a -> Eff e Unit

    emit :: forall e a b c. Prim.String -> a -> Scope b -> Eff e (Event e b c)

    evalAsync :: forall e r a. Maybe (Scope a -> Eff e r) -> Scope a -> Eff e Unit

    evalSync :: forall e r a b c. Maybe (Scope a -> Eff e r) -> Maybe {  | b } -> Scope a -> Eff e r

    id :: forall a. Scope a -> Prim.String

    newScope :: forall a b. Prim.Boolean -> Scope a -> Scope b

    on :: forall e a b c. Prim.String -> (Event e a b -> c -> Eff e Unit) -> Scope b -> Eff e OnDeregistration

    parent :: forall a b. Scope a -> Maybe (Scope b)

    root :: forall a b. Scope a -> Scope b

    stringApplyExpr :: forall e r a. Prim.String -> ApplyExpr e r a

    watch :: forall e a b. Prim.String -> Maybe (WatchListener e a b) -> Prim.Boolean -> Scope b -> Eff e WatchDeregistration

    watchCollection :: forall e a b. Prim.String -> WatchListener e a b -> Scope b -> Eff e WatchDeregistration


## Module DOM.Event

### Types

    type Event  = { keyCode :: Prim.Number }


## Module DOM.Node

### Types

    data DOM :: !

    data Node :: *


### Values

    focus :: forall e. Node -> Eff (dom :: DOM | e) Unit



