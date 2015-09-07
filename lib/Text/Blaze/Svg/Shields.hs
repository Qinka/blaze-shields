{-# LANGUAGE OverloadedStrings #-}




module Text.Blaze.Svg.Shields
    (
    plasticStyle,
    flatStyle,
    socialStyle,
    flatSquareStyle
    ) where

      import Prelude hiding (id,show)
      import Control.Monad(when)
      import Data.Maybe(fromMaybe)

      import Text.Blaze(ToMarkup,AttributeValue)
      import Text.Blaze.Internal(string,stringValue)
      import Text.Blaze.Html((!),toHtml)
      import Text.Blaze.Html5(style)
      import Text.Blaze.Html5.Attributes(xmlns)
      import Text.Blaze.Svg(Svg)
      import Text.Blaze.Svg11(
        svg,lineargradient,stop,rect,g,path,text_,rect,
        image,a
        )
      import Text.Blaze.Svg11.Attributes(
        width,height,x2,y2,offset,stopColor,stopOpacity,id_,
        rx,fill,d,textAnchor,fontFamily,fontSize,x,y,fillOpacity,
        xlinkHref,shapeRendering,type_,stroke,fontWeight
        )
      import qualified Text.Blaze.Svg11 as S11
      import qualified Text.Blaze.Svg11.Attributes as S11A
      import qualified Text.Blaze.Html5.Attributes as H5A
      import qualified Prelude as P

      show :: Show a => a -> AttributeValue
      show x = stringValue  show_
        where
          show_ = P.show x


      plasticStyle ::(Show a,ToMarkup a,Show b,Num b,Floating b)=> (a,b)        --left
                                                      -> (a,b)        --right
                                                      -> Maybe String --colorA
                                                      -> Maybe String --colorB
                                                      -> Svg          -- rt
      plasticStyle (l,lp) (r,rp) cA cB = svg ! xmlns "http://www.w3.org/2000/svg" ! width (show $ lp+rp+20) ! height "18" $ do
        lineargradient ! id_ "smooth" ! x2 "0" ! y2 "100%" $ do
          stop ! offset "0" ! stopColor "#fff" ! stopOpacity ".7"
          stop ! offset ".1" ! stopColor "#aaa" ! stopOpacity ".1"
          stop ! offset ".9" ! stopColor "#000" ! stopOpacity ".3"
          stop ! offset "1" ! stopColor "#000" ! stopOpacity ".5"
        S11.mask ! id_ "round" $
          rect ! width (show $ lp+rp+20) ! height "18" ! rx "4" ! fill "#fff"
        g ! S11A.mask "url(#round)" $ do
          rect ! width (show $ lp+10) ! height "18" !  fill (colorA cA)
          rect ! x (show $ lp+10) ! width (show $ 10+rp) !  height "18" ! fill (colorB cB)
          rect ! width (show $ lp+rp+20) ! height "18" ! fill "url(#smooth)"
        g ! fill "#fff" ! textAnchor "middle" ! fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! fontSize "11" $ do
            text_ ! x (show $ lp/2+7) ! y "14" ! fill "#010101" ! fillOpacity "0.3" $ toHtml l
            text_ ! x (show $ lp/2+7) ! y "13" $ toHtml l
            text_ ! x (show $ lp+rp/2+13) ! y "14" ! fill "#010101" ! fillOpacity "0.3" $ toHtml r
            text_ ! x (show $ lp+rp/2+13) ! y "13" $ toHtml r
        where
          colorA (Just a) = show a
          colorA Nothing = "#555"
          colorB (Just a) = show a
          colorB Nothing = "#4c1"

      flatStyle ::(Show a,ToMarkup a,Show b,Num b,Floating b)=> (a,b)        --left
                                                   -> (a,b)        --right
                                                   -> Maybe (b,b,String) --logo logowidth logopadding logo-url
                                                   -> Maybe String --colorA
                                                   -> Maybe String --colorB
                                                   -> Svg          -- rt
      flatStyle (l,lp) (r,rp) logo cA cB = svg ! xmlns "http://www.w3.org/2000/svg" ! width (show $ lp+rp+20) ! height "20" $ do
          lineargradient ! id_ "smooth" ! x2 "0" ! y2 "100%" $ do
           stop ! offset "0" ! stopOpacity ".1" ! stopColor "#bbb"
           stop ! offset "1" ! stopOpacity ".1"
          S11.mask ! id_ "round" $
            rect ! width (show $ lp+rp+20) !  height "20" ! rx "3" ! fill "#fff"
          g ! S11A.mask "url(#round)" $ do
            rect ! width (show $lp+10) ! height "20" ! fill (colorA cA)
            rect ! width (show $rp+10) ! x (show $lp+10) ! height "20" ! fill (colorB cB)
            rect ! width (show $ lp+rp+20) ! height "20" ! fill "url(#smooth)"
          g ! fill "#fff" ! textAnchor "middle" ! fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! fontSize "11" $ do
            case logo of
              Just (w,p,u) -> image ! x "5" ! y "3" ! width (show w) ! height "14" ! xlinkHref (stringValue u)
              Nothing -> return ()
            text_ ! x (show $ (lp+ww+pp)/2+6) ! y "15"  ! fill "#010101" ! fillOpacity ".3" $ toHtml l
            text_ ! x (show $ (lp+ww+pp)/2+6) ! y "14"  $ toHtml l
            text_ ! x (show $ lp +rp/2+13) ! y "14"  ! fill "#010101" ! fillOpacity ".3" $ toHtml r
            text_ ! x (show $ lp +rp/2+13) ! y "14"  $ toHtml r
        where
          colorA (Just a) = stringValue a
          colorA Nothing = stringValue "#555"
          colorB (Just a) = stringValue a
          colorB Nothing = stringValue "#4c1"
          ww = case logo of
            Nothing -> 0
            Just (w,_,_) -> w
          pp = case logo of
            Nothing -> 0
            Just (_,p,_) -> p



      flatSquareStyle ::(Show a,ToMarkup a,Show b,Num b,Floating b)=> (a,b)        --left
                                                        -> (a,b)        --right
                                                        -> Maybe String --colorA
                                                        -> Maybe String --colorB
                                                        -> Svg          -- rt

      flatSquareStyle (l,lp) (r,rp) cA cB = svg ! xmlns "http://www.w3.org/2000/svg" ! width (show $ lp+rp+20) ! height "20" $ do
        g ! shapeRendering "crispEdges" $ do
          rect ! width (show $ lp+10) ! height "20" ! fill colorA
          rect ! x (show $ lp+10) ! width (show $ rp+10) ! height "20" ! fill colorB
        g ! fill "#fff" ! textAnchor "middle" ! fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! fontSize "11" $ do
          text_ ! x (show $ lp/2+7) !y "14" $ toHtml l
          text_ ! x (show $ lp+rp/2+13) !y "14" $ toHtml r
        where
          colorA = stringValue $ fromMaybe "#555" cA
          colorB = stringValue $ fromMaybe "#4c1" cB
      socialStyle ::(Show a,ToMarkup a,Show b,Num b,Floating b)=> (a,b)        --left
                                                     -> (a,b)        --right
                                                     -> Maybe (b,b,String) --logo logowidth logopadding logo-url
                                                     -> Maybe String --colorA
                                                     -> Maybe String --colorB
                                                     -> Maybe String --link1
                                                     -> Maybe String --link2
                                                     -> Svg          -- rt
      socialStyle (l,lp) (r,rp) logo cA cB la lb= svg ! xmlns "http://www.w3.org/2000/svg" ! width (show $ lp+rp+7) ! height "20" $ do
          style ! type_ "text/css" $ do
            "<![CDATA["
            "#llink:hover { fill:url(#b); stroke:#ccc; }"
            "#rlink:hover { fill:#4183C4; }"
            "]]>"
          lineargradient ! id_ "a" ! x2 "0" ! y2 "100%" $ do
            stop ! offset "0" ! stopColor "#fcfcfc" ! stopOpacity "0"
            stop ! offset "1" ! stopOpacity ".1"
          lineargradient ! id_ "b" ! x2 "0" ! y2 "100%" $ do
            stop ! offset "0" ! stopColor "#ccc" ! stopOpacity ".1"
            stop ! offset "1" ! stopOpacity ".1"
          g ! stroke "#d5d5d5" $ do
            rect ! stroke "none" ! fill "#fcfcfc" ! x "0.5" ! y "0.5" ! width (show lp) ! height "19" ! rx "2"
            rect ! y "0.5" ! x (show $ lp+6.5) ! width (show rp) ! height "19" ! rx "2" ! fill "#fafafa"
            rect ! x (show $ lp+6) ! y "7.5" ! width "0.5" ! height "5" ! stroke "#fafafa"
            path ! d (show pd) ! stroke "d5d5d5" ! fill "#fafafa"
          case logo of
            Just (w,p,u) -> image ! x "5" ! y "3" ! width (show w) ! height "14" ! xlinkHref (show u)
            Nothing -> return ()
          g ! fill "#333" ! textAnchor "middle" ! fontFamily "Helvetica Neue,Helvetica,Arial,sans-serif" ! fontWeight "700" ! fontSize "11px"  $ do
            text_ ! x (show $ (lp+ww+pp)/2) ! y "15" ! fill "#fff" $ toHtml l
            text_ ! x (show $ (lp+ww+pp)/2) ! y "14" $ toHtml l
            text_ ! x (show $ lp+rp/2+6) ! y "15" ! fill "#fff" $ toHtml r
            case la of
              Just la' -> a ! xlinkHref (stringValue la') $
                text_ ! id_  "rlink" ! x (show $ lp+rp/2+6) ! y "14" $ toHtml r
              Nothing -> return ()
          case la of
              Just la' -> a ! xlinkHref (stringValue la') $
                rect ! id_ "llink" ! stroke "#d5d5d5" ! fill "url(#a)" ! x "0.5" ! y "0.5" ! width (show lp) ! height "19" ! rx "2"
              Nothing -> return ()
        where
          pd = "M" ++P.show (lp+6.5)++ "6.5 l-3 3v1 l3 3"
          colorA = fromMaybe "#555" cA
          colorB = fromMaybe "#4c1" cB
          ww = case logo of
            Nothing -> 0
            Just (w,_,_) -> w
          pp = case logo of
            Nothing -> 0
            Just (_,p,_) -> p
