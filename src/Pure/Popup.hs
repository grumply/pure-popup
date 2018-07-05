{-# LANGUAGE ExistentialQuantification, PatternSynonyms, ViewPatterns, RecordWildCards, DuplicateRecordFields, CPP, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, OverloadedStrings #-}
module Pure.Popup where

import Pure hiding (Content,Content_,position)

import Pure.Data.Txt (isInfixOf)
import Pure.Data.Cond
import Pure.Data.CSS
import Pure.Data.Prop
import Pure.Data.Styles
import Pure.Theme
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
    , position :: Txt
    , size :: Txt
    , triggerOn :: [Txt]
    , wide :: Maybe Txt
    , withPortal :: Portal.Portal -> Portal.Portal
    , themed :: SomePopupT
    } deriving (Generic)

data SomePopupT = forall t. Themeable t => SomePopupT t

instance Default SomePopupT where
    def = defaultPopupTheme

instance Default Popup where
    def = (G.to gdef)
        { as = \fs cs -> Div & Features fs & Children cs
        , Pure.Popup.position = "top left"
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

                            s = compute position

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

                            ps = (position,s) : fmap (id &&& compute) (filter (/= position) positions)

                            findValid [] = (position,s)
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
                                    , size
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
                                        & PortalNode (\f -> as (f $ features & Theme t & Classes cs & Pure.Styles currentStyles & Lifecycle (HostRef handlePopupRef)) children)
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
            "position" =: absolute
            top =: pxs 0
            right =: pxs 0
            minWidth =: "-webkit-min-content"
            minWidth =: "-moz-min-content"
            minWidth =: "min-content"
            zIndex =: "1900"
            border =: pxs 1 <<>> solid <<>> "#D4D4D5"
            lineHeight =: ems 1.4285
            maxWidth =: pxs 250
            background =: "#FFFFFF"
            padding =: ems 0.833 <<>> ems 1
            fontWeight =: normal
            "font-style" =: normal
            color =: rgba(0,0,0,0.87)
            borderRadius =: rems 0.28571429
            "-webkit-box-shadow" =: pxs 0 <<>> pxs 2 <<>> pxs 4 <<>> pxs 0 <<>> rgba(34,36,38,0.12) <&>> pxs 0 <<>> pxs 2 <<>> pxs 10 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
            "box-shadow" =: pxs 0 <<>> pxs 2 <<>> pxs 4 <<>> pxs 0 <<>> rgba(34,36,38,0.12) <&>> pxs 0 <<>> pxs 2 <<>> pxs 10 <<>> pxs 0 <<>> rgba(34,36,38,0.15)
        arrowStyles = void $ do
            "position" =: absolute
            content =: emptyQuotes
            width =: ems 0.71428571
            height =: ems 0.71428571
            background =: "#FFFFFF"
            "-webkit-transform" =: rotate(deg 45)
            "transform" =: rotate(deg 45)
            zIndex =: int 2
            "-webkit-box-shadow" =: pxs 1 <<>> pxs 1 <<>> pxs 0 <<>> pxs 0 <<>> "#bababc"
            "box-shadow" =: pxs 1 <<>> pxs 1 <<>> pxs 0 <<>> pxs 0 <<>> "#bababc"
        spacing = void $ do
            margin =: ems 0
        topSpacing = void $ do
            margin =: ems 0 <<>> ems 0 <<>> ems 0.71428571
        topLeftSpacing = do
            marginLeft =: ems 0
            transOrig (left <<>> bottom)
        topCenterSpacing = transOrig (center <<>> bottom)
        topRightSpacing = do
            marginRight =: ems 0
            transOrig (right <<>> bottom)
        leftCenterSpacing = do
            margin =: ems 0 <<>> ems 0.71428571 <<>> ems 0 <<>> ems 0
            transOrig (right <<>> per 50)
        rightCenterSpacing = do
            margin =: ems 0 <<>> ems 0 <<>> ems 0 <<>> ems 0.71428571
            transOrig (right <<>> per 50)
        bottomSpacing = void $ do
            margin =: ems 0.71428571 <<>> ems 0 <<>> ems 0
        bottomLeftSpacing = do
            marginLeft =: ems 0
            transOrig (left <<>> top)
        bottomCenterSpacing = transOrig (center <<>> top)
        bottomRightSpacing = do
            marginRight =: ems 0
            transOrig (right <<>> top)
        bottomCenterArrow = void $ do
            marginLeft =: neg (ems 0.30714286)
            top =: neg (ems 0.30714286)
            left =: per 50
            right =: auto
            bottom =: auto
            boxShad (neg (pxs 1) <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> "#bababc")
        bottomLeftArrow = void $ do
            top =: neg (ems 0.30714286)
            left =: ems 1
            right =: auto
            bottom =: auto
            marginLeft =: ems 0
            boxShad (neg (pxs 1) <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> "#bababc")
        bottomRightArrow = void $ do
            top =: neg (ems 0.30714286)
            right =: ems 1
            bottom =: auto
            left =: auto
            marginLeft =: ems 0
            boxShad (neg (pxs 1) <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> "#bababc")
        topCenterArrow = void $ do
            top =: auto
            right =: auto
            bottom =: neg (ems 0.30714286)
            left =: per 50
            marginLeft =: neg (ems 0.30714286)
        topLeftArrow = void $ do
            bottom =: neg (ems 0.30714286)
            left =: ems 1
            top =: auto
            right =: auto
            marginRight =: ems 0
        topRightArrow = void $ do
            bottom =: neg (ems 0.30714286)
            right =: ems 1
            top =: auto
            left =: auto
            marginLeft =: ems 0
        leftCenterArrow = void $ do
            top =: per 50
            right =: neg (ems 0.30714286)
            bottom =: auto
            left =: auto
            marginTop =: neg (ems 0.30714286)
            boxShad (pxs 1 <<>> neg (pxs 1) <<>> pxs 0 <<>> pxs 0 <<>> "#bababc")
        rightCenterArrow = void $ do
            top =: per 50
            left =: neg (ems 0.30714286)
            bottom =: auto
            right =: auto
            marginTop =: neg (ems 0.30714286)
            boxShad (neg (pxs 1) <<>> pxs 1 <<>> pxs 0 <<>> pxs 0 <<>> "#bababc") 
        bottomArrowColor = void $ background =: "#FFFFFF"
        sideCenterArrowColor = void $ background =: "#FFFFFF"
        topArrowColor = void $ background =: "#FFFFFF"
        invertedBottomArrowColor = void $ background =: "#1B1C1D"
        invertedSideCenterArrowColor = void $ background =: "#1B1C1D"
        invertedTopArrowColor = void $ background =: "#1B1C1D"
        loadingPopup = void $ do
            display =: block
            visibility =: hidden
            zIndex =: neg one
        animatingPopup = void $ display =: block
        visiblePopup = void $ do
            display =: block
            trans (translateZ(pxs 0))
            backface hidden
        basicPopup = void $ display =: none
        widePopup = void $ maxWidth =: pxs 350
        veryWidePopup = void $ maxWidth =: pxs 550
        wideSmallScreenPopup = void $ maxWidth =: pxs 250
        veryWideSmallScreenPopup = void $ maxWidth =: pxs 250
        fluidPopup = void $ do
            width =: per 100
            maxWidth =: none
        invertedPopup = void $ do
            background =: "#1B1C1D"
            color =: "#FFFFFF"
            border =: none
            boxShad none
        invertedArrow = void $ do
            backgroundColor =: "#1B1C1D"
            important $ boxShad none
        flowingPopup = void $ maxWidth =: none
        miniPopup = void $ fontSize =: rems 0.78571429
        tinyPopup = void $ fontSize =: rems 0.85714286
        smallPopup = void $ fontSize =: rems 0.92857143
        regularPopup = void $ fontSize =: rems 1
        largePopup = void $ fontSize =: rems 1.14285714
        hugePopup = void $ fontSize =: rems 1.42857143

        transOrig t = void $ do
            "-webkit-transform-origin" =: t
            "transform-origin" =: t

        boxShad bs = void $ do
            "-webkit-box-shadow" =: bs
            "box-shadow" =: bs

        trans t = void $ do
            "-webkit-transform" =: t
            "transform" =: t
            
        backface b = void $ do
            "-webkit-backface-visibility" =: b
            "backface-visibility" =: b

instance Themeable PopupT where
    theme c PopupT {..} = do
        is c $ do
            -- base popup styles and spacing
            apply $ do
                baseStyles
                spacing
                regularPopup

            -- base arrow styles
            is ":before" $ apply arrowStyles

            -- spacing
            is ".top" $ do
                apply topSpacing
                is ".inverted" . is ":before" .> invertedTopArrowColor
                is ".left" $ do
                    apply topLeftSpacing
                    is ":before" .> topLeftArrow
                is ".center" $ do
                    apply topCenterSpacing
                    is ":before" .> topCenterArrow
                is ".right" $ do 
                    apply topRightSpacing
                    is ":before" .> topRightArrow
                is ":before" .> topArrowColor

            is ".left" $ do
                is ".center" $ do
                    apply $ do
                        leftCenterSpacing
                        leftCenterArrow
                    is ":before" $ do
                        apply sideCenterArrowColor
                        is ".inverted" .> invertedSideCenterArrowColor

            is ".right" $ do
                is ".center" $ do
                    apply $ do 
                        rightCenterSpacing
                        rightCenterArrow
                    is ":before" $ do
                        apply sideCenterArrowColor
                        is ".inverted" .> invertedSideCenterArrowColor

            is ".bottom" $ do
                apply bottomSpacing
                is ".inverted" . is ":before" .> invertedBottomArrowColor
                is ".left" $ do
                    apply bottomLeftSpacing
                    is ":before" .> bottomLeftArrow
                is ".center" $ do
                    apply bottomCenterSpacing
                    is ":before" .> bottomCenterArrow
                is ".right" $ do
                    apply bottomRightSpacing
                    is ":before" .> bottomRightArrow
                is ":before" .> bottomArrowColor

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
                is ":before" .> invertedArrow

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
    getProp _ = Pure.Popup.position
    setProp _ pos p = p { Pure.Popup.position = pos }

instance HasProp Size Popup where
    type Prop Size Popup = Txt
    getProp _ = size
    setProp _ sz p = p { size = sz }

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

