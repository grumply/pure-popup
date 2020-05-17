{-# LANGUAGE ExistentialQuantification, PatternSynonyms, ViewPatterns, RecordWildCards, DuplicateRecordFields, CPP, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, OverloadedStrings, FlexibleContexts, PostfixOperators #-}
module Pure.Popup where

import Pure hiding (Content,Content_,offset,not,(#))

import Pure.Data.Txt (isInfixOf)
import Pure.Data.Cond
import Pure.Data.Prop
import Pure.Portal as Portal

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Monad (void,join)
import Data.Foldable (for_)
import Data.IORef
import Data.List hiding (isInfixOf)
import Data.Maybe
import GHC.Generics as G

import Data.Function as Tools ((&))

import Prelude hiding (rem,max,min)

data Popup = Popup_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , trigger :: View
    , basic :: Bool
    , flowing :: Bool
    , hideOnScroll :: Bool
    , hoverable :: Bool
    , inverted :: Bool
    , offset :: Double
    , onClose :: IO ()
    , onMount :: IO ()
    , onOpen :: IO ()
    , onUnmount :: IO ()
    , position_ :: Txt
    , size_ :: Txt
    , triggerOn :: [Txt]
    , wide :: Maybe Txt
    , withPortal :: Portal.Portal -> Portal.Portal
    , themed :: SomePopupT
    } deriving (Generic)

data SomePopupT = forall t. Theme t => SomePopupT t

instance Default SomePopupT where
    def = defaultPopupTheme

instance Default Popup where
    def = (G.to gdef)
        { as = \fs cs -> Div & Features fs & Children cs
        , position_ = "top left"
        , triggerOn = [ "hover" ]
        , withPortal = id
        }

pattern Popup :: Popup -> Popup
pattern Popup p = p

data PopupState = WPS
    { closed :: Bool
    , currentStyles :: [(Txt,Txt)]
    , currentPosition :: Txt
    , coords :: IORef (Maybe BoundingRect)
    , popupCoords :: IORef (Maybe BoundingRect)
    , scrollHandler :: IORef (IO ())
    }

instance Pure Popup where
    view =
        LibraryComponentIO $ \self ->
            let
                bounds = do
                    let fi = fromIntegral :: Int -> Double
                    (fi -> pxo,fi -> pyo,fi -> cw,fi -> ch)
                        <- (,,,) <$> pageXOffset
                                 <*> pageYOffset
                                 <*> clientWidth
                                 <*> clientHeight
                    return (pxo,pyo,cw,ch)

                computePopupStyle offset pbr cbr (pxo,pyo,cw,ch) p =
                    let xOff = brWidth pbr + 8

                        isLeft   = left   `isInfixOf` p
                        isRight  = right  `isInfixOf` p
                        isTop    = top    `isInfixOf` p
                        isBottom = bottom `isInfixOf` p

                        centerV = not (isTop  || isBottom)

                        leftStyle
                            | isRight           = Nothing
                            | isLeft            = Just 0
                            | otherwise         = Just $ (brWidth cbr - brWidth pbr) / 2

                        leftStyle' = fmap (\l -> l + pxo + brLeft cbr - offset) leftStyle

                        leftStyle''
                            | centerV   = fmap (subtract xOff) leftStyle'
                            | otherwise = leftStyle'

                        rightStyle
                            | isRight   = Just 0
                            | otherwise = Nothing

                        rightStyle' = fmap (\r -> r + cw - (brRight cbr + pxo) - offset) rightStyle

                        rightStyle''
                            | centerV   = fmap (subtract xOff) rightStyle'
                            | otherwise = rightStyle'

                        topStyle
                            | isTop     = Nothing
                            | isBottom  = Just 0
                            | otherwise = Just $ negate $ (brHeight cbr + brHeight pbr) / 2

                        topStyle' = fmap (\t -> t + brBottom cbr + pyo) topStyle

                        bottomStyle
                            | isTop     = Just 0
                            | otherwise = Nothing

                        bottomStyle' = fmap (\b -> b + ch - (brTop cbr + pyo)) bottomStyle

                    in (leftStyle'',rightStyle'',topStyle',bottomStyle')

                isStyleInViewport BR {..} (pxo,pyo,cw,ch) (l,r,t,b) =
                    let
                        leftValue
                            | isJust r  = maybe 0 (\_ -> cw - fromJust r - brWidth) l
                            | otherwise = fromMaybe 0 l

                        topValue
                            | isJust b  = maybe 0 (\_ -> ch - fromJust b - brHeight) t
                            | otherwise = fromMaybe 0 t

                        visibleTop    = topValue > pyo
                        visibleBottom = topValue + brHeight < pyo + ch
                        visibleLeft   = leftValue > pyo
                        visibleRight  = leftValue + brWidth < pxo + cw

                    in visibleTop && visibleBottom && visibleLeft && visibleRight

                setPopupStyles = do
                    WPS {..} <- get self
                    Popup_ {..} <- ask self
                    mcbr <- readIORef coords
                    mpbr <- readIORef popupCoords
                    for_ ((,) <$> mcbr <*> mpbr) $ \(cbr,pbr) -> do
                        bs  <- bounds
                        let
                            view d x = (d,maybe auto (pxs . round) x)

                            compute = computePopupStyle offset pbr cbr bs

                            s = compute position_

                            positions =
                                [ "top left"
                                , "top right"
                                , "top center"
                                , "bottom left"
                                , "bottom right"
                                , "bottom center"
                                , "right center"
                                , "left center"
                                ]

                            ps = (position_,s) : fmap (id &&& compute) (filter (/= position_) positions)

                            findValid [] = (position_,s)
                            findValid ((p,c) : cs)
                                | isStyleInViewport pbr bs c = (p,c)
                                | otherwise                  = findValid cs

                            (p,(l,r,t,b)) = findValid ps

                        let ss = [("position","absolute"),view left l,view right r,view top t,view bottom b]

                        modify_ self $ \_ WPS {..} ->
                            WPS { currentStyles  = ss
                                , currentPosition = p
                                , ..
                                }

                handleOpen (evtTarget -> t) = do
                    Popup_ {..} <- ask self
                    WPS {..} <- get self
                    br <- boundingRect (Element t)
                    writeIORef coords (Just br)
                    onOpen

                handlePortalMount = do
                    Popup_ {..} <- ask self
                    WPS {..} <- get self
                    sh <- onRaw (Node $ toJSV window) "scroll" def $ \_ _ -> do
                        Popup_ {..} <- ask self
                        WPS {..} <- get self
                        modify self $ \_ WPS {..} -> WPS { closed = True, .. }
                        join $ readIORef scrollHandler
                        forkIO $ do
                            threadDelay 50000
                            modify_ self $ \_ WPS {..} -> WPS { closed = False, .. }
                        onClose
                    writeIORef scrollHandler sh
                    onMount

                handlePortalUnmount = do
                    Popup_ {..} <- ask self
                    WPS {..} <- get self
                    join $ readIORef scrollHandler
                    onUnmount

                handlePopupRef (Node n) = do
                    modifyM_ self $ \_ WPS {..} -> return
                        ( WPS {..} , do
                          br <- boundingRect (Element n)
                          writeIORef popupCoords (isNull n ? Nothing $ Just br)
                          setPopupStyles
                        )

            in def
                { construct = WPS def def "top left" <$> newIORef def <*> newIORef def <*> newIORef def
                , render = \Popup_ {..} WPS {..} ->
                    case themed of
                        SomePopupT t -> 
                            let
                                applyPortalProps =
                                    let
                                        hoverableProps = hoverable ? (CloseOnPortalMouseLeave True . MouseLeaveDelay 300) $ id
                                        clickProps = ("click" `elem` triggerOn) ? (OpenOnTriggerClick True . CloseOnTriggerClick True . CloseOnDocumentClick True) $ id
                                        focusProps = ("focus" `elem` triggerOn) ? (OpenOnTriggerFocus True . CloseOnTriggerBlur True) $ id
                                        hoverProps = ("hover" `elem` triggerOn) ? (OpenOnTriggerMouseEnter True . CloseOnTriggerMouseLeave True . MouseLeaveDelay 70 . MouseEnterDelay 50) $ id
                                    in
                                        hoverProps . focusProps . clickProps . hoverableProps

                                cs =
                                    [ currentPosition
                                    , size_
                                    , maybe "" (<<>> "wide") wide
                                    , basic # "basic"
                                    , flowing # "flowing"
                                    , inverted # "inverted"
                                    , "popup transition visible"
                                    ]
                            in
                                closed
                                    ? trigger
                                    $ View $ Portal.Portal $ withPortal $ applyPortalProps $ def
                                        & Portal.OnClose onClose
                                        & OnMount handlePortalMount
                                        & OnOpen handleOpen
                                        & OnUnmounted handlePortalUnmount
                                        & PortalNode (\f -> as (f $ features & Pure.themed t & Classes cs & Pure.Styles currentStyles & Lifecycle (HostRef handlePopupRef)) children)
                                        & Children [ trigger ]
                }

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = window.pageYOffset" pageYOffset_js :: IO Int
#endif

pageYOffset :: IO Int
pageYOffset =
#ifdef __GHCJS__
    pageYOffset_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = window.pageXOffset" pageXOffset_js :: IO Int
#endif

pageXOffset :: IO Int
pageXOffset =
#ifdef __GHCJS__
    pageXOffset_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = document.documentElement.clientWidth" clientWidth_js :: IO Int
#endif

clientWidth :: IO Int
clientWidth =
#ifdef __GHCJS__
    clientWidth_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = document.documentElement.clientHeight" clientHeight_js :: IO Int
#endif

clientHeight :: IO Int
clientHeight =
#ifdef __GHCJS__
    clientHeight_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$r = $1.getBoundingClientRect()" bounding_client_rect_js :: Element -> IO JSV
#endif

data BoundingRect = BR
    { brLeft :: Double
    , brTop :: Double
    , brRight :: Double
    , brBottom :: Double
    , brWidth :: Double
    , brHeight :: Double
    } deriving (Eq)

instance Default BoundingRect where def = BR 0 0 0 0 0 0

boundingRect :: Element -> IO BoundingRect
boundingRect node = do
#ifdef __GHCJS__
  o <- bounding_client_rect_js node
  return $ fromMaybe (error "Semantic.Utils.boundingRect: fromMaybe got Nothing") $ do
    brLeft   <- o .# "left"
    brTop    <- o .# "top"
    brRight  <- o .# "right"
    brBottom <- o .# "bottom"
    brWidth  <- o .# "width"
    brHeight <- o .# "height"
    return BR {..}
#else
    return $ BR 0 0 0 0 0 0
#endif

defaultPopupTheme = SomePopupT (def :: PopupT)

data PopupT = PopupT
  { baseStyles :: Styles ()
  , arrowStyles :: Styles ()
  , spacing :: Styles ()
  , topSpacing :: Styles ()
  , topLeftSpacing :: Styles ()
  , topCenterSpacing :: Styles ()
  , topRightSpacing :: Styles ()
  , leftCenterSpacing :: Styles ()
  , rightCenterSpacing :: Styles ()
  , bottomSpacing :: Styles ()
  , bottomLeftSpacing :: Styles ()
  , bottomCenterSpacing :: Styles ()
  , bottomRightSpacing :: Styles ()
  , bottomCenterArrow :: Styles ()
  , bottomLeftArrow :: Styles ()
  , bottomRightArrow :: Styles ()
  , topCenterArrow :: Styles ()
  , topLeftArrow :: Styles ()
  , topRightArrow :: Styles ()
  , leftCenterArrow :: Styles ()
  , rightCenterArrow :: Styles ()
  , bottomArrowColor :: Styles ()
  , sideCenterArrowColor :: Styles ()
  , topArrowColor :: Styles ()
  , invertedBottomArrowColor :: Styles ()
  , invertedSideCenterArrowColor :: Styles ()
  , invertedTopArrowColor :: Styles ()
  , loadingPopup :: Styles ()
  , animatingPopup :: Styles ()
  , visiblePopup :: Styles ()
  , basicPopup :: Styles ()
  , widePopup :: Styles ()
  , veryWidePopup :: Styles ()
  , wideSmallScreenPopup :: Styles ()
  , veryWideSmallScreenPopup :: Styles ()
  , fluidPopup :: Styles ()
  , invertedPopup :: Styles ()
  , invertedArrow :: Styles ()
  , flowingPopup :: Styles ()
  , miniPopup :: Styles ()
  , tinyPopup :: Styles ()
  , smallPopup :: Styles ()
  , regularPopup :: Styles ()
  , largePopup :: Styles ()
  , hugePopup :: Styles ()
  }

instance Default PopupT where
    def = PopupT {..}
      where
        baseStyles = void $ do
            display =: none
            position =: absolute
            top =: 0px
            right =: 0px
            min-width =: webkit-min-content
            min-width =: moz-min-content
            min-width =: min-content
            z-index =: 1900
            border =* [1px,solid,hex 0xD4D4D5]
            line-height =: 1.4285em
            max-width =: 250px
            background =: hex 0xFFF
            padding =* [0.833em,1em]
            font-weight =: normal
            font-style =: normal
            color =: rgba(0,0,0,0.87)
            border-radius =: rems 0.28571429
            box-shadow =* [0px,2px, 4px,0px,rgba(34,36,38,0.12),","
                          ,0px,2px,10px,0px,rgba(34,36,38,0.15)
                          ]
        arrowStyles = void $ do
            position =: absolute
            content =: emptyQuotes
            width =: 0.71428571em
            height =: 0.71428571em
            background =: hex 0xFFF
            transform =: rotate(45deg)
            z-index =: 2
            box-shadow =* [1px,1px,0px,0px,hex 0xbababc]
        spacing = void $ do
            margin =: 0
        topSpacing = void $ do
            margin =* [0em,0em,0em,0.71428571em]
        topLeftSpacing = void $ do
            margin-left =: 0
            transform-origin =* [left,bottom]
        topCenterSpacing = void $
            transform-origin =* [center,bottom]
        topRightSpacing = void $ do
            margin-right =: 0
            transform-origin =* [right,bottom]
        leftCenterSpacing = void $ do
            margin =* [0em,0.71428571em,0em,0em]
            transform-origin =* [right,(50%)]
        rightCenterSpacing = void $ do
            margin =* [0em,0em,0em,0.71428571em]
            transform-origin =* [right,(50%)]
        bottomSpacing = void $ do
            margin =* [0.71428571em,0em,0em]
        bottomLeftSpacing = void $ do
            margin-left =: 0em
            transform-origin =* [left,top]
        bottomCenterSpacing = void $
            transform-origin =* [center,top]
        bottomRightSpacing = void $ do
            margin-right =: 0em
            transform-origin =* [right,top]
        bottomCenterArrow = void $ do
            margin-left =: (-0.30714286)em
            top =: (-0.30714286)em
            left =: (50%)
            right =: auto
            bottom =: auto
            box-shadow =* [(-1)px,(-1)px,0px,0px,hex 0xbababc]
        bottomLeftArrow = void $ do
            top =: (-0.30714286)em
            left =: 1em
            right =: auto
            bottom =: auto
            margin-left =: 0em
            box-shadow =* [(-1)px,(-1)px,0px,0px,hex 0xbababc]
        bottomRightArrow = void $ do
            top =: (-0.30714286)em
            right =: 1em
            bottom =: auto
            left =: auto
            margin-left =: 0em
            box-shadow =* [(-1)px,(-1)px,0px,0px,hex 0xbababc]
        topCenterArrow = void $ do
            top =: auto
            right =: auto
            bottom =: (-0.30714286)em
            left =: (50%)
            margin-left =: (-0.30714286)em
        topLeftArrow = void $ do
            bottom =: (-0.30714286)em
            left =: 1em
            top =: auto
            right =: auto
            margin-right =: 0em
        topRightArrow = void $ do
            bottom =: (-0.30714286)em
            right =: 1em
            top =: auto
            left =: auto
            margin-left =: 0em
        leftCenterArrow = void $ do
            top =: (50%)
            right =: (-0.30714286)em
            bottom =: auto
            left =: auto
            margin-top =: (-0.30714286)em
            box-shadow =* [1px,(-1)px,0px,0px,hex 0xbababc]
        rightCenterArrow = void $ do
            top =: (50%)
            left =: (-0.30714286)em
            bottom =: auto
            right =: auto
            margin-top =: (-0.30714286)em
            box-shadow =* [(-1)px,1px,0px,0px,hex 0xbababc]
        bottomArrowColor = void $ 
            background =: hex 0xFFF
        sideCenterArrowColor = void $ 
            background =: hex 0xFFF
        topArrowColor = void $ 
            background =: hex 0xFFF
        invertedBottomArrowColor = void $ 
            background =: hex 0x1B1C1D
        invertedSideCenterArrowColor = void $ 
            background =: hex 0x1B1C1D
        invertedTopArrowColor = void $ 
            background =: hex 0x1B1C1D
        loadingPopup = void $ do
            display =: block
            visibility =: hidden
            z-index =: (-1)
        animatingPopup = void $ 
            display =: block
        visiblePopup = void $ do
            display =: block
            transform =: translateZ(0px)
            webkit-backface-visibility =: hidden
            backface-visibility =: hidden
        basicPopup = void $ 
            display =: none
        widePopup = void $ 
            max-width =: 350px
        veryWidePopup = void $ 
            max-width =: 550px
        wideSmallScreenPopup = void $ 
            max-width =: 250px
        veryWideSmallScreenPopup = void $ 
            max-width =: 250px
        fluidPopup = void $ do
            width =: (100%)
            max-width =: none
        invertedPopup = void $ do
            background =: hex 0x1B1C1D
            color =: hex 0xFFF
            border =: none
            box-shadow =: none
        invertedArrow = void $ do
            background-color =: hex 0x1B1C1D
            important $ box-shadow =: none
        flowingPopup = void $ 
            max-width =: none
        miniPopup = void $ 
            font-size =: 0.78571429rem
        tinyPopup = void $ 
            font-size =: 0.85714286rem
        smallPopup = void $ 
            font-size =: 0.92857143rem
        regularPopup = void $ 
            font-size =: 1rem
        largePopup = void $ 
            font-size =: 1.14285714rem
        hugePopup = void $ 
            font-size =: 1.42857143rem

instance Theme PopupT where
    theme c = do
        let PopupT {..} = def
        is c $ do
            -- base popup styles and spacing
            apply $ do
                baseStyles
                spacing
                regularPopup

            -- base arrow styles
            is before $ apply arrowStyles

            -- spacing
            is ".top" $ do
                apply topSpacing
                is ".inverted" . is before .> invertedTopArrowColor
                is ".left" $ do
                    apply topLeftSpacing
                    is before .> topLeftArrow
                is ".center" $ do
                    apply topCenterSpacing
                    is before .> topCenterArrow
                is ".right" $ do 
                    apply topRightSpacing
                    is before .> topRightArrow
                is before .> topArrowColor

            is ".left" $ do
                is ".center" $ do
                    apply $ do
                        leftCenterSpacing
                        leftCenterArrow
                    is before $ do
                        apply sideCenterArrowColor
                        is ".inverted" .> invertedSideCenterArrowColor

            is ".right" $ do
                is ".center" $ do
                    apply $ do 
                        rightCenterSpacing
                        rightCenterArrow
                    is before $ do
                        apply sideCenterArrowColor
                        is ".inverted" .> invertedSideCenterArrowColor

            is ".bottom" $ do
                apply bottomSpacing
                is ".inverted" . is before .> invertedBottomArrowColor
                is ".left" $ do
                    apply bottomLeftSpacing
                    is before .> bottomLeftArrow
                is ".center" $ do
                    apply bottomCenterSpacing
                    is before .> bottomCenterArrow
                is ".right" $ do
                    apply bottomRightSpacing
                    is before .> bottomRightArrow
                is before .> bottomArrowColor

            is ".loading" .> loadingPopup

            is ".animating" .> animatingPopup

            is ".visible" .> visiblePopup

            is ".basic" .> basicPopup

            is ".wide" $ do
                apply widePopup
                is ".very" .> veryWidePopup

            is ".fluid" .> fluidPopup

            is ".inverted" $ do 
                apply invertedPopup
                is before .> invertedArrow

            is ".flowing" .> flowingPopup

            is ".mini" .> miniPopup

            is ".tiny" .> tinyPopup

            is ".small" .> smallPopup

            is ".large" .> largePopup

            is ".huge" .> hugePopup

        atMedia "only screen and (max-width: 767px)" $ do
            is c . is ".wide" .> wideSmallScreenPopup
            is c . is ".very" . is ".wide" .> veryWideSmallScreenPopup

data As = As_
pattern As :: HasProp As a => Prop As a -> a -> a
pattern As p a <- (getProp As_ &&& id -> (p,a)) where
    As p a = setProp As_ p a

data Basic = Basic_
pattern Basic :: HasProp Basic a => Prop Basic a -> a -> a
pattern Basic p a <- (getProp Basic_ &&& id -> (p,a)) where
    Basic p a = setProp Basic_ p a

data Flowing = Flowing_
pattern Flowing :: HasProp Flowing a => Prop Flowing a -> a -> a
pattern Flowing p a <- (getProp Flowing_ &&& id -> (p,a)) where
    Flowing p a = setProp Flowing_ p a

data HideOnScroll = HideOnScroll_
pattern HideOnScroll :: HasProp HideOnScroll a => Prop HideOnScroll a -> a -> a
pattern HideOnScroll p a <- (getProp HideOnScroll_ &&& id -> (p,a)) where
    HideOnScroll p a = setProp HideOnScroll_ p a

data Hoverable = Hoverable_
pattern Hoverable :: HasProp Hoverable a => Prop Hoverable a -> a -> a
pattern Hoverable p a <- (getProp Hoverable_ &&& id -> (p,a)) where
    Hoverable p a = setProp Hoverable_ p a

data Inverted = Inverted_
pattern Inverted :: HasProp Inverted a => Prop Inverted a -> a -> a
pattern Inverted p a <- (getProp Inverted_ &&& id -> (p,a)) where
    Inverted p a = setProp Inverted_ p a

data Offset = Offset_
pattern Offset :: HasProp Offset a => Prop Offset a -> a -> a
pattern Offset p a <- (getProp Offset_ &&& id -> (p,a)) where
    Offset p a = setProp Offset_ p a

data Position = Position_
pattern Position :: HasProp Position a => Prop Position a -> a -> a
pattern Position p a <- (getProp Position_ &&& id -> (p,a)) where
    Position p a = setProp Position_ p a

data Size = Size_
pattern Size :: HasProp Size a => Prop Size a -> a -> a
pattern Size p a <- (getProp Size_ &&& id -> (p,a)) where
    Size p a = setProp Size_ p a

data TriggerOn = TriggerOn_
pattern TriggerOn :: HasProp TriggerOn a => Prop TriggerOn a -> a -> a
pattern TriggerOn p a <- (getProp TriggerOn_ &&& id -> (p,a)) where
    TriggerOn p a = setProp TriggerOn_ p a

data Wide = Wide_
pattern Wide :: HasProp Wide a => Prop Wide a -> a -> a
pattern Wide p a <- (getProp Wide_ &&& id -> (p,a)) where
    Wide p a = setProp Wide_ p a

data Trigger = Trigger_
pattern Trigger :: HasProp Trigger a => Prop Trigger a -> a -> a
pattern Trigger p a <- (getProp Trigger_ &&& id -> (p,a)) where
    Trigger p a = setProp Trigger_ p a

instance HasProp As Popup where
    type Prop As Popup = Features -> [View] -> View
    getProp _ = as
    setProp _ f p = p { as = f }

instance HasFeatures Popup where
    getFeatures = features
    setFeatures cs p = p { features = cs }

instance HasChildren Popup where
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasProp Basic Popup where
    type Prop Basic Popup = Bool
    getProp _ = basic
    setProp _ b p = p { basic = b }

instance HasProp Flowing Popup where
    type Prop Flowing Popup = Bool
    getProp _ = flowing
    setProp _ f p = p { flowing = f }

instance HasProp HideOnScroll Popup where
    type Prop HideOnScroll Popup = Bool
    getProp _ = hideOnScroll
    setProp _ hos p = p { hideOnScroll = hos }

instance HasProp Hoverable Popup where
    type Prop Hoverable Popup = Bool
    getProp _ = hoverable
    setProp _ h p = p { hoverable = h }

instance HasProp Inverted Popup where
    type Prop Inverted Popup = Bool
    getProp _ = inverted
    setProp _ i p = p { inverted = i }

instance HasProp Offset Popup where
    type Prop Offset Popup = Double
    getProp _ = offset
    setProp _ o p = p { offset = o }

instance HasProp OnClose Popup where
    type Prop OnClose Popup = IO ()
    getProp _ = onClose
    setProp _ oc p = p { onClose = oc }

instance HasProp OnMount Popup where
    type Prop OnMount Popup = IO ()
    getProp _ = onMount
    setProp _ om p = p { onMount = om }

instance HasProp OnOpen Popup where
    type Prop OnOpen Popup = IO ()
    getProp _ = onOpen
    setProp _ oo p = p { onOpen = oo }

instance HasProp OnUnmounted Popup where
    type Prop OnUnmounted Popup = IO ()
    getProp _ = onUnmount
    setProp _ ou p = p { onUnmount = ou }

instance HasProp Position Popup where
    type Prop Position Popup = Txt
    getProp _ = position_
    setProp _ pos p = p { position_ = pos }

instance HasProp Size Popup where
    type Prop Size Popup = Txt
    getProp _ = size_
    setProp _ sz p = p { size_ = sz }

instance HasProp Trigger Popup where
    type Prop Trigger Popup = View
    getProp _ = trigger
    setProp _ t p = p { trigger = t }

instance HasProp TriggerOn Popup where
    type Prop TriggerOn Popup = [Txt]
    getProp _ = triggerOn
    setProp _ to p = p { triggerOn = to }

instance HasProp Wide Popup where
    type Prop Wide Popup = Maybe Txt
    getProp _ = wide
    setProp _ w p = p { wide = w }

instance HasProp WithPortal Popup where
    type Prop WithPortal Popup = Portal -> Portal
    getProp _ = withPortal
    setProp _ wp p = p { withPortal = wp }

