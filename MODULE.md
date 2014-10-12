# Module Documentation

## Module Angular

### Values

    bootstrap :: forall e. Element -> [String] -> Eff e Injector

    copy :: forall e a. a -> Eff e a

    extend :: forall e a b c. {  | a } -> {  | b } -> Eff e {  | c }


## Module Angular.Animate

### Types

    data Animate :: *

    type AnimateEff e = Eff (nganimate :: NgAnimate | e) Unit

    data NgAnimate :: !


### Values

    addClass :: forall e f r. Element -> String -> Maybe (Eff f r) -> Animate -> AnimateEff e

    enter :: forall e f r. Element -> Element -> Element -> Maybe (Eff f r) -> Animate -> AnimateEff e

    leave :: forall e f r. Element -> Maybe (Eff f r) -> Animate -> AnimateEff e

    move :: forall e f r. Element -> Element -> Element -> Maybe (Eff f r) -> Animate -> AnimateEff e

    removeClass :: forall e f r. Element -> String -> Maybe (Eff f r) -> Animate -> AnimateEff e

    setClass :: forall e f r. Element -> String -> String -> Maybe (Eff f r) -> Animate -> AnimateEff e


## Module Angular.Attributes

### Types

    type AttrEff e r = Eff (ngattr :: NgAttr | e) r

    data Attributes :: *

    data NgAttr :: !


### Values

    addClass :: forall e. String -> Attributes -> AttrEff e Unit

    attr :: forall e a. Attributes -> AttrEff e {  | a }

    get :: forall e a. Attributes -> AttrEff e {  | a }

    observe :: forall e f. String -> (String -> Eff f Unit) -> Attributes -> AttrEff e Unit

    removeClass :: forall e. String -> Attributes -> AttrEff e Unit

    set :: forall e. String -> String -> Attributes -> AttrEff e Unit

    updateClass :: forall e. String -> String -> Attributes -> AttrEff e Unit


## Module Angular.Cache

### Types

    data Cache :: *

    type CacheEff e r = Eff (ngcache :: NgCache | e) r

    data CacheFactory :: *

    type Key  = String

    type Name  = String

    data NgCache :: !

    type Options a = { capacity :: Number | a }


### Values

    cache :: forall e a. Name -> Maybe (Options a) -> CacheFactory -> CacheEff e Cache

    destroy :: forall e. Cache -> CacheEff e Unit

    get :: forall e a. Key -> Cache -> CacheEff e a

    info :: forall e a. Cache -> CacheEff e { size :: Number, id :: String | a }

    put :: forall e a. Key -> a -> Cache -> CacheEff e a

    remove :: forall e. Key -> Cache -> CacheEff e Unit

    removeAll :: forall e. Cache -> CacheEff e Unit


## Module Angular.DI

### Types

    data Annotated :: * -> *

    newtype RootElement where
      RootElement :: Element -> RootElement

    newtype RootScope a where
      RootScope :: Scope a -> RootScope a


### Type Classes

    class Dependency a where
      name :: String

    class Injectable a where
      dependencies :: a -> [String]

    class (Dependency a) <= Service a where


### Type Class Instances

    instance dependencyAnimate :: Dependency Animate

    instance dependencyAttributes :: Dependency Attributes

    instance dependencyCacheFactory :: Dependency CacheFactory

    instance dependencyElement :: Dependency Element

    instance dependencyHttp :: Dependency Http

    instance dependencyInjector :: Dependency Injector

    instance dependencyInterpolate :: Dependency Interpolate

    instance dependencyInterval :: Dependency Interval

    instance dependencyLocation :: Dependency Location

    instance dependencyLog :: Dependency Log

    instance dependencyParse :: Dependency Parse

    instance dependencyQ :: Dependency Q

    instance dependencyRootElement :: Dependency RootElement

    instance dependencyRootScope :: Dependency (RootScope a)

    instance dependencyScope :: Dependency (Scope a)

    instance dependencyThis :: Dependency (This a)

    instance dependencyTimeout :: Dependency Timeout

    instance injectableEff :: Injectable (Eff e r)

    instance injectableFn :: (Dependency a, Injectable b) => Injectable (a -> b)

    instance serviceAnimate :: Service Animate

    instance serviceCacheFactory :: Service CacheFactory

    instance serviceHttp :: Service Http

    instance serviceInjector :: Service Injector

    instance serviceInterpolate :: Service Interpolate

    instance serviceInterval :: Service Interval

    instance serviceLocation :: Service Location

    instance serviceLog :: Service Log

    instance serviceParse :: Service Parse

    instance serviceQ :: Service Q

    instance serviceRootElement :: Service RootElement

    instance serviceRootScope :: Service (RootScope a)

    instance serviceTimeout :: Service Timeout


### Values

    annotate :: forall a. (Injectable a) => a -> Annotated a

    get :: forall e a. (Service a) => Injector -> InjEff e a


## Module Angular.Deferred

### Types

    data Deferred :: * -> * -> *

    type DeferredEff e r = Eff (ngdeferred :: NgDeferred | e) r

    data NgDeferred :: !


### Values

    notify :: forall e s a b. s -> Deferred a b -> DeferredEff e Unit

    promise :: forall e a b. Deferred a b -> DeferredEff e (Promise a b)

    reject :: forall e a b. a -> Deferred a b -> DeferredEff e Unit

    resolve :: forall e a b. b -> Deferred a b -> DeferredEff e Unit


## Module Angular.Element

### Types

    data DeregisterHandler :: # ! -> *

    type ElEff e r = Eff (ngel :: NgEl | e) r

    data Element :: *

    type Handler e = Event -> Eff e Unit

    data NgEl :: !


### Values

    (!!) :: Element -> Number -> Maybe Node

    addClass :: forall e. String -> Element -> ElEff e Element

    after :: forall e. Element -> Element -> ElEff e Element

    bind :: forall e f. String -> Handler f -> Element -> ElEff e (DeregisterHandler f)

    children :: forall e. Element -> ElEff e Element

    clone :: forall e. Element -> ElEff e Element

    contents :: forall e. Element -> ElEff e Element

    controller :: forall e a. Maybe String -> Element -> ElEff e (Maybe a)

    element :: forall e. String -> ElEff e Element

    empty :: forall e. Element -> ElEff e Element

    eq :: forall e. Number -> Element -> ElEff e Element

    find :: forall e. Number -> Element -> ElEff e Element

    getAttr :: forall e. String -> Element -> ElEff e (Maybe String)

    getCss :: forall e. String -> Element -> ElEff e (Maybe String)

    getData :: forall e a. String -> Element -> ElEff e (Maybe a)

    getProp :: forall e. String -> Element -> ElEff e (Maybe String)

    getVal :: forall e. Element -> ElEff e (Maybe String)

    hasClass :: forall e. String -> Element -> ElEff e Boolean

    html :: forall e. Element -> ElEff e String

    inheritedData :: forall e a. Element -> ElEff e {  | a }

    injector :: forall e a. Element -> ElEff e (Maybe Injector)

    isolateScope :: forall e a. Element -> ElEff e (Maybe (Scope a))

    next :: forall e. Element -> ElEff e Element

    off :: forall e f. String -> Element -> ElEff e Element

    offHandler :: forall e f. String -> DeregisterHandler f -> Element -> ElEff e Element

    on :: forall e f. String -> Handler f -> Element -> ElEff e (DeregisterHandler f)

    one :: forall e f. String -> Handler f -> Element -> ElEff e (DeregisterHandler f)

    parent :: forall e. Element -> ElEff e Element

    prepend :: forall e. Element -> Element -> ElEff e Element

    ready :: forall e. Eff e Unit -> Element -> ElEff e Element

    remove :: forall e. Element -> ElEff e Element

    removeAttr :: forall e. String -> Element -> ElEff e Element

    removeClass :: forall e. String -> Element -> ElEff e Element

    removeData :: forall e. String -> Element -> ElEff e Element

    replaceWith :: forall e. Element -> Element -> ElEff e Element

    scope :: forall e a. Element -> ElEff e (Maybe (Scope a))

    setAllAttr :: forall e a. {  | a } -> Element -> ElEff e Element

    setAllCss :: forall e a. {  | a } -> Element -> ElEff e Element

    setAllData :: forall e a. {  | a } -> Element -> ElEff e Element

    setAllProp :: forall e a. {  | a } -> Element -> ElEff e Element

    setAttr :: forall e. String -> String -> Element -> ElEff e Element

    setCss :: forall e. String -> String -> Element -> ElEff e Element

    setData :: forall e a. String -> a -> Element -> ElEff e Element

    setProp :: forall e. String -> String -> Element -> ElEff e Element

    setVal :: forall e. String -> Element -> ElEff e Element

    toggleClass :: forall e. String -> Boolean -> Element -> ElEff e Element

    triggerHandler :: forall e a. String -> [a] -> Element -> ElEff e Element

    unbind :: forall e f. String -> Element -> ElEff e Element

    unbindHandler :: forall e f. String -> DeregisterHandler f -> Element -> ElEff e Element

    wrap :: forall e. Element -> Element -> ElEff e Element


## Module Angular.FormController

### Types

    data FormController :: *

    type FormEff e r = Eff (ngform :: NgForm | e) r

    data NgForm :: !


### Values

    addControl :: forall e a. NgModelController a -> FormController -> FormEff e Unit

    dirty :: forall e. FormController -> FormEff e Boolean

    error :: forall e a. FormController -> FormEff e {  | a }

    invalid :: forall e. FormController -> FormEff e Boolean

    pristine :: forall e. FormController -> FormEff e Boolean

    removeControl :: forall e a. NgModelController a -> FormController -> FormEff e Unit

    setDirty :: forall e. FormController -> FormEff e Unit

    setPristine :: forall e. FormController -> FormEff e Unit

    setValidity :: forall e a. ValidationErrorKey -> Boolean -> NgModelController a -> FormController -> FormEff e Unit

    valid :: forall e. FormController -> FormEff e Boolean


## Module Angular.Http

### Types

    type Config a b c d = { responseType :: ResponseType, withCredentials :: Boolean, timeout :: Either Number (Promise c d), cache :: Either Boolean Cache, xsrfCookieName :: String, xsrfHeaderName :: String, headers :: Headers, "data" :: RequestData b, params :: {  | a }, url :: Url, method :: Method }

    data Http :: *

    type HttpResponse e r a b c d = HttpEff e (Promise (Response r a b c d) (Response r a b c d))

    type Response r a b c d = { statusText :: String, config :: Config a b c d, headers :: [String] -> String, status :: Status, "data" :: ResponseData r }


### Values

    config :: forall a b c d. Config a b c d

    del :: forall e r a b c d. Url -> Http -> HttpResponse e r a b c d

    del' :: forall e r a b c d. Url -> Config a b c d -> Http -> HttpResponse e r a b c d

    get :: forall e r a b c d. Url -> Http -> HttpResponse e r a b c d

    get' :: forall e r a b c d. Url -> Config a b c d -> Http -> HttpResponse e r a b c d

    head :: forall e r a b c d. Url -> Http -> HttpResponse e r a b c d

    head' :: forall e r a b c d. Url -> Config a b c d -> Http -> HttpResponse e r a b c d

    http :: forall e r a b c d. Config a b c d -> Http -> HttpResponse e r a b c d

    jsonp :: forall e r a b c d. Url -> Http -> HttpResponse e r a b c d

    jsonp' :: forall e r a b c d. Url -> Config a b c d -> Http -> HttpResponse e r a b c d

    post :: forall e r a b c d. Url -> RequestData b -> Http -> HttpResponse e r a b c d

    post' :: forall e r a b c d. Url -> RequestData b -> Config a b c d -> Http -> HttpResponse e r a b c d

    put :: forall e r a b c d. Url -> RequestData b -> Http -> HttpResponse e r a b c d

    put' :: forall e r a b c d. Url -> RequestData b -> Config a b c d -> Http -> HttpResponse e r a b c d


## Module Angular.Injector

### Types

    type InjEff e a = Eff (nginj :: NgInj | e) a

    data Injector :: *

    data NgInj :: !


### Values

    annotate :: forall e a. a -> Injector -> InjEff e [String]

    get :: forall e a. String -> Injector -> InjEff e a

    has :: forall e. String -> Injector -> InjEff e Boolean

    injector :: forall e. [String] -> InjEff e Injector

    instantiate :: forall e r a b. a -> Maybe {  | b } -> Injector -> InjEff e r

    invoke :: forall e r a b c. a -> Maybe {  | b } -> Maybe {  | c } -> Injector -> InjEff e r


## Module Angular.Interpolate

### Types

    type Expression a = {  | a } -> String

    data Interpolate :: *

    type InterpolateEff e a = Eff (nginterpolate :: NgInterpolate | e) a

    data NgInterpolate :: !


### Values

    endSymbol :: Interpolate -> String

    interpolate :: forall e a. String -> Maybe Boolean -> Maybe String -> Interpolate -> InterpolateEff e (Expression a)

    startSymbol :: Interpolate -> String


## Module Angular.Interval

### Types

    data Interval :: *

    type IntervalEff e r = Eff (nginterval :: NgInterval | e) r

    type IntervalPromise  = Promise Unit Number

    data NgInterval :: !


### Values

    cancel :: forall e. IntervalPromise -> Interval -> IntervalEff e Boolean

    interval :: forall e f r. Eff f r -> Number -> Number -> Boolean -> Interval -> IntervalEff e IntervalPromise

    interval' :: forall e f r. Eff f r -> Number -> Number -> Interval -> IntervalEff e IntervalPromise

    interval'' :: forall e f r. Eff f r -> Number -> Interval -> IntervalEff e IntervalPromise

    intervalk :: forall e f r. Number -> Number -> Boolean -> Interval -> Eff f r -> IntervalEff e IntervalPromise


## Module Angular.Location

### Types

    type LocEff e a = Eff (ngloc :: NgLoc | e) a

    data Location :: *

    data NgLoc :: !


### Values

    getPath :: forall e. Location -> LocEff e String

    setPath :: forall e. String -> Location -> LocEff e String


## Module Angular.Log

### Types

    data Log :: *

    type LogEff e = Eff (nglog :: NgLog | e) Unit

    data NgLog :: !


### Values

    debug :: forall e a. a -> Log -> LogEff e

    error :: forall e a. a -> Log -> LogEff e

    info :: forall e a. a -> Log -> LogEff e

    log :: forall e a. a -> Log -> LogEff e

    warn :: forall e a. a -> Log -> LogEff e


## Module Angular.Module

### Types

    data Module :: *

    data NgReadModule :: !

    data NgRegisterToModule :: !

    data NgWriteModule :: !

    type ReadEff e = Eff (ngrmod :: NgReadModule | e) Module

    type RegisterEff e = Eff (nggmod :: NgRegisterToModule | e) Module

    type WriteEff e = Eff (ngwmod :: NgWriteModule | e) Module


### Values

    animation :: forall e a. String -> a -> Module -> RegisterEff e

    config :: forall e a. a -> Module -> RegisterEff e

    constant :: forall e a. String -> a -> Module -> RegisterEff e

    controller :: forall e a. String -> a -> Module -> RegisterEff e

    directive :: forall e a. String -> a -> Module -> RegisterEff e

    factory :: forall e a. String -> a -> Module -> RegisterEff e

    filter :: forall e a. String -> a -> Module -> RegisterEff e

    ngmodule :: forall e. String -> ReadEff e

    ngmodule' :: forall e. String -> [String] -> WriteEff e

    provider :: forall e a. String -> a -> Module -> RegisterEff e

    run :: forall e a. a -> Module -> RegisterEff e

    service :: forall e a. String -> a -> Module -> RegisterEff e

    value :: forall e a. String -> a -> Module -> RegisterEff e


## Module Angular.NgModelController

### Types

    type Formatter a = a -> String

    data NgModel :: !

    data NgModelController :: * -> *

    type NgModelEff e r = Eff (ngmodel :: NgModel | e) r

    type Parser a = String -> Maybe a

    type ValidationErrorKey  = String


### Values

    appendFormatters :: forall e a. [Formatter a] -> NgModelController a -> NgModelEff e Unit

    appendParsers :: forall e a. [Parser a] -> NgModelController a -> NgModelEff e Unit

    appendViewChangeListeners :: forall e a. [Eff e Unit] -> NgModelController a -> NgModelEff e Unit

    dirty :: forall e a. NgModelController a -> NgModelEff e Boolean

    error :: forall e a b. NgModelController a -> NgModelEff e {  | b }

    invalid :: forall e a. NgModelController a -> NgModelEff e Boolean

    isEmpty :: forall e a. a -> NgModelController a -> NgModelEff e Boolean

    modelValue :: forall e a. NgModelController a -> NgModelEff e a

    prependFormatters :: forall e a. [Formatter a] -> NgModelController a -> NgModelEff e Unit

    prependParsers :: forall e a. [Parser a] -> NgModelController a -> NgModelEff e Unit

    prependViewChangeListeners :: forall e a. [Eff e Unit] -> NgModelController a -> NgModelEff e Unit

    pristine :: forall e a. NgModelController a -> NgModelEff e Boolean

    render :: forall e a. NgModelController a -> NgModelEff e Unit

    setIsEmpty :: forall e a b. (b -> NgModelEff e Boolean) -> NgModelController a -> NgModelEff e (NgModelController b)

    setModelValue :: forall e a b. b -> NgModelController a -> NgModelEff e (NgModelController b)

    setPristine :: forall e a. NgModelController a -> NgModelEff e Unit

    setRender :: forall e a. NgModelEff e Unit -> NgModelController a -> NgModelEff e Unit

    setValidity :: forall e a. ValidationErrorKey -> Boolean -> NgModelController a -> NgModelEff e Unit

    setViewValue :: forall e a. String -> NgModelController a -> NgModelEff e Unit

    valid :: forall e a. NgModelController a -> NgModelEff e Boolean

    viewValue :: forall e a. NgModelController a -> NgModelEff e String


## Module Angular.Parse

### Types

    type Expression  = String

    data Getter :: * -> *

    data NgParse :: !

    data Parse :: *

    type ParseEff e r = Eff (ngparse :: NgParse | e) r

    data Setter :: * -> *


### Values

    assign :: forall a. Getter a -> Maybe (Setter a)

    constant :: forall a. Getter a -> Boolean

    get :: forall e a b c. {  | a } -> {  | b } -> Getter c -> ParseEff e c

    literal :: forall a. Getter a -> Boolean

    parse :: forall e a. Expression -> Parse -> ParseEff e (Getter a)

    set :: forall e a b. {  | a } -> b -> Setter b -> ParseEff e b


## Module Angular.Promise

### Types

    data Promise :: * -> * -> *


### Type Class Instances

    instance applicativePromise :: Applicative (Promise a)

    instance applyPromise :: Apply (Promise a)

    instance bifunctorPromise :: Bifunctor Promise

    instance bindPromise :: Bind (Promise a)

    instance functorPromise :: Functor (Promise a)

    instance monadPromise :: Monad (Promise a)


### Values

    catch' :: forall a b c d. (a -> Promise c d) -> Promise a b -> Promise c d

    finally' :: forall e r a b. Eff e r -> Promise a b -> Promise a b

    pureReject :: forall a b. a -> Promise a b

    pureResolve :: forall a b. b -> Promise a b

    then' :: forall a b c d. (b -> Promise c d) -> Promise a b -> Promise c d

    then'' :: forall a b c d. (b -> Promise c d) -> (a -> Promise c d) -> Promise a b -> Promise c d

    then''' :: forall e s t a b c d. (b -> Promise c d) -> (a -> Promise c d) -> (s -> Eff e t) -> Promise a b -> Promise c d


## Module Angular.Q

### Types

    data NgQ :: !

    data Q :: *

    type QEff e r = Eff (ngq :: NgQ | e) r


### Values

    all :: forall e a b. [Promise a b] -> Q -> QEff e (Promise b [a])

    defer :: forall e a b. Q -> QEff e (Deferred a b)

    reject :: forall e a b. a -> Q -> QEff e (Promise a b)

    when :: forall e a b. b -> Q -> QEff e (Promise a b)


## Module Angular.ST

### Values

    pushAllSTArray :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) Number

    pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Number

    readSTArray :: forall a h r. STArray h a -> Eff (st :: ST h | r) [a]

    spliceSTArray :: forall a h r. STArray h a -> Number -> Number -> [a] -> Eff (st :: ST h | r) [a]

    writeSTArray :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) [a]


## Module Angular.Scope

### Types

    data ApplyExpr e r a where
      DefaultApplyExpr :: ApplyExpr e r a
      StringApplyExpr :: String -> ApplyExpr e r a
      FnApplyExpr :: (Scope a -> Eff e r) -> ApplyExpr e r a

    type Event e a b = { defaultPrevented :: Boolean, preventDefault :: Eff e Unit, stopPropagation :: Eff e Unit, name :: String, currentScope :: Scope b, targetScope :: Scope a }

    data NgReadScope :: !

    data NgScope :: !

    data NgWriteScope :: !

    data OnDeregistration :: *

    type ReadEff e a = Eff (ngrscope :: NgReadScope | e) {  | a }

    type ReadWriteEff e r = Eff (ngwscope :: NgWriteScope, ngrscope :: NgReadScope | e) r

    data Scope :: # * -> *

    type ScopeEff e r = Eff (ngscope :: NgScope | e) r

    data WatchDeregistration :: *

    type WatchListener e a b = a -> a -> Scope b -> Eff e Unit

    type WriteEff e = Eff (ngwscope :: NgWriteScope | e) Unit


### Values

    apply :: forall e r a. ApplyExpr e r a -> Scope a -> ScopeEff e r

    applyExpr :: forall e r a. (Scope a -> Eff e r) -> ApplyExpr e r a

    broadcast :: forall e a b c. String -> a -> Scope b -> ScopeEff e (Event e b c)

    defaultApplyExpr :: forall e r a. ApplyExpr e r a

    deregisterOn :: OnDeregistration -> Unit

    deregisterWatch :: WatchDeregistration -> Unit

    destroy :: forall e a. Scope a -> ScopeEff e Unit

    digest :: forall e a. Scope a -> ScopeEff e Unit

    emit :: forall e a b c. String -> a -> Scope b -> ScopeEff e (Event e b c)

    evalAsync :: forall e r a. Maybe (Scope a -> Eff e r) -> Scope a -> ScopeEff e r

    evalSync :: forall e r a b. Maybe (Scope a -> Eff e r) -> Maybe {  | b } -> Scope a -> ScopeEff e r

    extendScope :: forall e a b. {  | b } -> Scope a -> WriteEff e

    id :: forall a. Scope a -> String

    modifyScope :: forall e a b. ({  | a } -> Eff e {  | b }) -> Scope a -> ReadWriteEff e Unit

    newScope :: forall e a b. Boolean -> Scope a -> ScopeEff e (Scope b)

    on :: forall e a b c. String -> (Event e a b -> c -> Eff e Unit) -> Scope b -> ScopeEff e OnDeregistration

    parent :: forall a b. Scope a -> Maybe (Scope b)

    readScope :: forall e a. Scope a -> ReadEff e a

    root :: forall a b. Scope a -> Scope b

    stringApplyExpr :: forall e r a. String -> ApplyExpr e r a

    watch :: forall e a b. String -> Maybe (WatchListener e a b) -> Boolean -> Scope b -> ScopeEff e WatchDeregistration

    watchCollection :: forall e a b. String -> WatchListener e a b -> Scope b -> ScopeEff e WatchDeregistration

    writeScope :: forall e a b. String -> b -> Scope a -> WriteEff e


## Module Angular.This

### Types

    data NgReadThis :: !

    data NgWriteThis :: !

    type ReadEff e a = Eff (ngrthis :: NgReadThis | e) {  | a }

    type ReadWriteEff e r = Eff (ngwthis :: NgWriteThis, ngrthis :: NgReadThis | e) r

    data This :: # * -> *

    type WriteEff e = Eff (ngwthis :: NgWriteThis | e) Unit


### Values

    extendThis :: forall e a b. {  | b } -> This a -> WriteEff e

    modifyThis :: forall e a b. ({  | a } -> Eff e {  | b }) -> This a -> ReadWriteEff e Unit

    readThis :: forall e a. This a -> ReadEff e a

    writeThis :: forall e a b. String -> b -> This a -> WriteEff e


## Module Angular.Timeout

### Types

    data NgTimeout :: !

    data Timeout :: *

    type TimeoutEff e r = Eff (ngtimeout :: NgTimeout | e) r

    type TimeoutPromise a = Promise Error a


### Values

    cancel :: forall e r. TimeoutPromise r -> Timeout -> TimeoutEff e Boolean

    timeout :: forall e f r. Eff f r -> Number -> Boolean -> Timeout -> TimeoutEff e (TimeoutPromise r)

    timeout' :: forall e f r. Eff f r -> Number -> Timeout -> TimeoutEff e (TimeoutPromise r)

    timeout'' :: forall e f r. Eff f r -> Timeout -> TimeoutEff e (TimeoutPromise r)

    timeoutk :: forall e f r. Number -> Boolean -> Timeout -> Eff f r -> TimeoutEff e (TimeoutPromise r)


## Module DOM.Event

### Types

    type Event  = { keyCode :: Number }


## Module DOM.Node

### Types

    data DOM :: !

    data Node :: *


### Values

    focus :: forall e. Node -> Eff (dom :: DOM | e) Unit


## Module DOM.Types

### Types

    data ArrayBuffer :: *

    data Blob :: *

    data Document :: *

    data MozBlob :: *

    data MozChunkedArrayBuffer :: *

    data MozChunkedText :: *


## Module Angular.Http.Internal

### Types

    data ForeignConfig :: *

    data ForeignResponse :: *


### Values

    foreignConfig :: forall e. HttpEff e ForeignConfig

    getConfigCache :: ForeignConfig -> Either Boolean Cache

    getConfigHeaders :: ForeignConfig -> Headers

    getConfigMethod :: ForeignConfig -> Method

    getConfigParams :: forall a. ForeignConfig -> {  | a }

    getConfigRequestData :: forall e a. ForeignConfig -> RequestData a

    getConfigResponseType :: ForeignConfig -> ResponseType

    getConfigTimeout :: forall a b. ForeignConfig -> Either Number (Promise a b)

    getConfigUrl :: ForeignConfig -> Url

    getConfigWithCredentials :: ForeignConfig -> Boolean

    getConfigXsrfCookieName :: ForeignConfig -> String

    getConfigXsrfHeaderName :: ForeignConfig -> String

    getResponseConfig :: ForeignResponse -> ForeignConfig

    getResponseData :: forall a. ResponseType -> ForeignResponse -> ResponseData a

    getResponseHeaders :: ForeignResponse -> [String] -> String

    getResponseStatus :: ForeignResponse -> Status

    getResponseStatusText :: ForeignResponse -> String

    setConfigCache :: forall e. Either Boolean Cache -> ForeignConfig -> HttpEff e Unit

    setConfigHeaders :: forall e. Headers -> ForeignConfig -> HttpEff e Unit

    setConfigMethod :: forall e. Method -> ForeignConfig -> HttpEff e Unit

    setConfigParams :: forall e a. {  | a } -> ForeignConfig -> HttpEff e Unit

    setConfigRequestData :: forall e a. RequestData a -> ForeignConfig -> HttpEff e Unit

    setConfigResponseType :: forall e. ResponseType -> ForeignConfig -> HttpEff e Unit

    setConfigTimeout :: forall e a b. Either Number (Promise a b) -> ForeignConfig -> HttpEff e Unit

    setConfigUrl :: forall e. Url -> ForeignConfig -> HttpEff e Unit

    setConfigWithCredentials :: forall e. Boolean -> ForeignConfig -> HttpEff e Unit

    setConfigXsrfCookieName :: forall e. String -> ForeignConfig -> HttpEff e Unit

    setConfigXsrfHeaderName :: forall e. String -> ForeignConfig -> HttpEff e Unit


## Module Angular.Http.Types

### Types

    data ForeignCache :: *

    data ForeignHeaders :: *

    data ForeignRequestData :: *

    data ForeignResponseData :: *

    data ForeignTimeout :: *

    type Header  = Tuple String (Either String (Unit -> String))

    newtype Headers where
      Headers :: [Header] -> Headers

    type HttpEff e r = Eff (nghttp :: NgHttp | e) r

    data Method where
      GET :: Method
      POST :: Method
      PUT :: Method
      DELETE :: Method
      PATCH :: Method
      HEAD :: Method
      OPTIONS :: Method
      JSONP :: Method

    data NgHttp :: !

    data RequestData a where
      NoRequestData :: RequestData a
      StringRequestData :: String -> RequestData a
      ObjectRequestData :: a -> RequestData a

    type RequestDataFn a = { objectRequestData :: a -> RequestData a, stringRequestData :: String -> RequestData a, noRequestData :: RequestData a }

    data ResponseData a where
      NoResponseData :: ResponseData a
      DefaultResponseData :: String -> ResponseData a
      ArrayBufferResponseData :: D.ArrayBuffer -> ResponseData a
      BlobResponseData :: D.Blob -> ResponseData a
      DocumentResponseData :: D.Document -> ResponseData a
      JsonResponseData :: a -> ResponseData a
      TextResponseData :: String -> ResponseData a
      MozBlobResponseData :: D.MozBlob -> ResponseData a
      MozChunkedTextResponseData :: D.MozChunkedText -> ResponseData a
      MozChunkedArrayBufferResponseData :: D.MozChunkedArrayBuffer -> ResponseData a

    type ResponseDataFn a = { mozChunkedArrayBufferResponseData :: D.MozChunkedArrayBuffer -> ResponseData a, mozChunkedTextResponseData :: D.MozChunkedText -> ResponseData a, mozBlobResponseData :: D.MozBlob -> ResponseData a, textResponseData :: String -> ResponseData a, jsonResponseData :: a -> ResponseData a, documentResponseData :: D.Document -> ResponseData a, blobResponseData :: D.Blob -> ResponseData a, arrayBufferResponseData :: D.ArrayBuffer -> ResponseData a, defaultResponseData :: String -> ResponseData a, noResponseData :: ResponseData a }

    data ResponseType where
      Default :: ResponseType
      ArrayBuffer :: ResponseType
      Blob :: ResponseType
      Document :: ResponseType
      Json :: ResponseType
      Text :: ResponseType
      MozBlob :: ResponseType
      MozChunkedText :: ResponseType
      MozChunkedArrayBuffer :: ResponseType

    data Status where
      OK :: Status
      Created :: Status
      NoContent :: Status
      BadRequest :: Status
      Unauthorized :: Status
      Forbidden :: Status
      NotFound :: Status
      InternalServerError :: Status
      OtherStatus :: Number -> Status

    type Url  = String


### Type Class Instances

    instance showMethod :: Show Method

    instance showResponseType :: Show ResponseType


### Values

    cataRequestData :: forall a b. b -> (String -> b) -> (a -> b) -> RequestData a -> b

    fnHeader :: String -> (Unit -> String) -> Header

    readCacheFn :: Fn3 (Boolean -> Either Boolean Cache) (Cache -> Either Boolean Cache) ForeignCache (Either Boolean Cache)

    readHeadersFn :: forall a b. Fn4 (String -> Either String (Unit -> String)) ((Unit -> String) -> Either String (Unit -> String)) (String -> Either String (Unit -> String) -> Header) ForeignHeaders Headers

    readMethod :: String -> Method

    readRequestDataFn :: forall a. Fn2 (RequestDataFn a) ForeignRequestData (RequestData a)

    readResponseDataFn :: forall a. Fn3 (ResponseDataFn a) String ForeignResponseData (ResponseData a)

    readResponseType :: String -> ResponseType

    readStatus :: Number -> Status

    readTimeoutFn :: forall a b. Fn3 (Number -> Either Number (Promise a b)) (Promise a b -> Either Number (Promise a b)) ForeignTimeout (Either Number (Promise a b))

    stringHeader :: String -> String -> Header

    writeRequestData :: forall a. RequestData a -> ForeignRequestData


## Module Angular.Promise.Eff

### Types

    newtype PromiseEff e f a b where
      PromiseEff :: Promise (Eff e a) (Eff f b) -> PromiseEff e f a b


### Type Class Instances

    instance applicativePromiseEff :: Applicative (PromiseEff e f a)

    instance applyPromise :: Apply (PromiseEff e f a)

    instance bifunctorPromise :: Bifunctor (PromiseEff e f)

    instance bindPromiseEff :: Bind (PromiseEff e f a)

    instance functorPromiseEff :: Functor (PromiseEff e f a)


### Values

    liftPromiseEff :: forall e f a b. Eff e a -> Eff f b -> PromiseEff e f a b

    liftPromiseEff' :: forall e f a b. Eff f b -> PromiseEff e f a b

    promiseEff :: forall e f a b. Promise a b -> PromiseEff e f a b

    promiseEff' :: forall e f a b. Promise a (Eff f b) -> PromiseEff e f a b

    promiseEff'' :: forall e f a b. Promise (Eff e a) b -> PromiseEff e f a b

    runPromiseEff :: forall e f a b. PromiseEff e f a b -> Promise (Eff e a) (Eff f b)

    unsafeRunPromiseEff :: forall e f a b. PromiseEff e f a b -> Promise a b



