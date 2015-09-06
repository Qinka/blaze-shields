{-# LANGUAGE OverloadedStrings #-}




module Text.Blaze.Svg.Shields
    (
    ) where

      import Prelude hiding (id,show)
      import Control.Monad(when)
      import Data.Maybe(fromMaybe)

      import Text.Blaze(ToMarkup,AttributeValue)
      import Text.Blaze.Internal(string,stringValue)
      import Text.Blaze.Html((!),toHtml)
      import Text.Blaze.Html5.Attributes(xmlns,style)
      import Text.Blaze.Svg(Svg)
      import Text.Blaze.Svg11(
        svg,lineargradient,stop,rect,g,path,text_,rect,
        image
        )
      import Text.Blaze.Svg11.Attributes(
        width,height,x2,y2,offset,stopColor,stopOpacity,id_,
        rx,fill,d,textAnchor,fontFamily,fontSize,x,y,fillOpacity,
        xlinkHref,shapeRendering,type_
        )
      import qualified Text.Blaze.Svg11 as S11
      import qualified Text.Blaze.Svg11.Attributes as S11A
      import qualified Prelude as P

      show :: Show a => a -> AttributeValue
      show x = stringValue  show_
        where
          show_ = P.show x


      plasticStyle ::(Show a,ToMarkup a,Show b,Num b)=> (a,b)        --left
                                                      -> (a,b)        --right
                                                      -> Maybe String --colorA
                                                      -> Maybe String --colorB
                                                      -> Svg          -- rt
      plasticStyle (l,lp) (r,rp) cA cB = svg ! xmlns "http://www.w3.org/2000/svg" ! width (show $ lp+rp) ! height "18" $ do
        lineargradient ! id_ "smooth" ! x2 "0" ! y2 "100%" $ do
          stop ! offset "0" ! stopColor "#fff" ! stopOpacity ".7"
          stop ! offset ".1" ! stopColor "#aaa" ! stopOpacity ".1"
          stop ! offset ".9" ! stopColor "#000" ! stopOpacity ".3"
          stop ! offset "1" ! stopColor "#000" ! stopOpacity ".5"
        S11.mask ! id_ "round" $ do
          rect ! width (show $ lp+rp) ! height "18" ! rx "4" ! fill "#fff"
        g ! S11A.mask "url(#round)" $ do
          rect ! width (show lp) ! height "18" !  fill (colorA cA)
          rect ! x (show lp) ! width (show rp) !  height "18" ! fill (colorB cB)
          rect ! width (show $ lp + rp) ! height "18" ! fill "url#smooth"
        g ! fill "#fff" ! textAnchor "middle" ! fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! fontSize "11" $ do
            text_ ! x (show $ lp/2+1) ! y "14" ! fill "#010101" ! fillOpacity "0.3" $ toHtml l
            text_ ! x (show $ lp/2+1) ! y "13" $ toHtml l
            text_ ! x (show $ lp+rp/2-1) ! y "14" ! fill "#010101" ! fillOpacity "0.3" $ toHtml r
            text_ ! x (show $ lp+rp/2-1) ! y "13" $ toHtml r
        where
          colorA (Just a) = show a
          colorA Nothing = "#555"
          colorB (Just a) = show a
          colorB Nothing = "#4c1"

      flatStyle ::(Show a,ToMarkup a,Show b,Num b)=> (a,b)        --left
                                                   -> (a,b)        --right
                                                   -> Maybe (b,b,String) --logo logowidth logopadding logo-url
                                                   -> Maybe String --colorA
                                                   -> Maybe String --colorB
                                                   -> Svg          -- rt
      flatStyle (l,lp) (r,rp) logo cA cB = svg ! xmlns "http://www.w3.org/2000/svg" ! width (show $ lp+rp) ! height "20" $ do
          lineargradient ! id_ "smooth" ! x2 "0" ! y2 "100%" $ do
           stop ! offset "0" ! stopOpacity ".1" ! stopColor "#bbb"
           stop ! offset "1" ! stopOpacity ".1"
          S11.mask ! id_ "round" $
            rect ! width (show $ lp+rp) !  height "20" ! rx "3" ! fill "#fff"
          g ! S11A.mask "url(#round)" $ do
            rect ! width (show lp) ! height "20" ! fill (colorA cA)
            rect ! width (show lp) ! x (show rp) ! height "20" ! fill (colorB cB)
            rect ! width (show $ lp+rp) ! height "20" ! fill "#url(smooth)"
          g ! fill "#fff" ! textAnchor "middle" ! fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! fontSize "11" $ do
            case logo of
              Just (w,p,u) -> image ! x "5" ! y "3" ! width (show w) ! height "14" ! xlinkHref u
              Nothing -> return ()
            text_ ! x (show $ (lp+ww+pp)/2) ! y "15"  ! fill "#010101" ! fillOpacity ".3" $ l
            text_ ! x (show $ (lp+ww+pp)/2) ! y "14"  $ l
            text_ ! x (show $ lp +rp/2-1) ! y "14"  ! fill "#010101" ! fillOpacity ".3" $ r
            text_ ! x (show $ lp +rp/2-1) ! y "14"  $ r
        where
          colorA (Just a) = a
          colorA Nothing = "#555"
          colorB (Just a) = a
          colorB Nothing = "#4c1"
          ww = case logo of
            Nothing -> 0
            Just (w,_,_) -> w
          pp = case logo of
            Nothing -> 0
            Just (_,p,_) -> p



      flatSquareStyle ::(Show a,ToMarkup a,Show b,Num b)=> (a,b)        --left
                                                        -> (a,b)        --right
                                                        -> Maybe String --colorA
                                                        -> Maybe String --colorB
                                                        -> Svg          -- rt

      flatSquareStyle (l,lp) (r,rp) cA cB = svg ! xmlns "http://www.w3.org/2000/svg" ! width (show $ lp+rp) ! height "20" $ do
        g ! shapeRendering "crispEdges" $ do
          rect ! width (show lp) ! height "20" ! fill colorA
          rect ! x (show lp) ! width (show rp) ! height "20" ! fill colorB
        g ! fill "#fff" ! textAnchor "middle" ! fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! fontSize "11" $ do
          text_ ! x (show lp/2+1) !y "14" $ lp
          text_ ! x (show lp+rp/2-1) !y "14" $ rp
        where
          colorA = fromMaybe "#555" cA
          colorB = fromMaybe "#4c1" cB
      socialStyle ::(Show a,ToMarkup a,Show b,Num b)=> (a,b)        --left
                                                     -> (a,b)        --right
                                                     -> Maybe (b,b,String) --logo logowidth logopadding logo-url
                                                     -> Maybe String --colorA
                                                     -> Maybe String --colorB
                                                     -> Svg          -- rt
      socialStyle (l,lp) (r,rp) logo cA cB = svg ! xmlns "http://www.w3.org/2000/svg" ! width (show $ lp+rp+7) ! height "20" $ do
        style ! type_ "text/css" $ do
          "<![CDATA["
          "#llink:hover { fill:url(#b); stroke:#ccc; }"
          "#rlink:hover { fill:#4183C4; }"
          "]]>"
        where
          colorA = fromMaybe "#555" cA
          colorB = fromMaybe "#4c1" cB
          ww = case logo of
            Nothing -> 0
            Just (w,_,_) -> w
          pp = case logo of
            Nothing -> 0
            Just (_,p,_) -> p



      frameScotty :: Svg
      frameScotty = do
        svg ! xmlns "http://www.w3.org/2000/svg" ! width "92" ! height "20" $ do
          lineargradient ! id_ "b" ! x2 "0" ! y2 "100%" $ do
            stop ! offset "0" ! stopColor"#bbb" ! stopOpacity ".1"
            stop ! offset "1" ! stopOpacity ".1"
          S11.mask ! id_ "a" $ do
            rect ! width "92" ! height "20" ! rx "3" ! fill "#fff"
          g ! S11A.mask "url(#a)" $ do
            path ! fill "#555" ! d "M0 0h46v20H0z"
            path ! fill "#ff69b4" ! d "M46 0h46v20H46z"
            path ! fill "url(#b)" ! d "M0 0h92v20H0z"
          g ! fill "#fff" ! textAnchor "middle" ! fontFamily "DejaVu Sans,Verdana,Geneva,sans-serif" ! fontSize "11" $ do
            text_ ! x "23" ! y "15" ! fill "#010101" ! fillOpacity ".3" $ do
              "Frame"
            text_ ! x "23" ! y "14" $ do
              "Frame"
            text_ ! x "68" ! y "15" ! fill "#010101" ! fillOpacity ".3" $ do
              "Scotty"
            text_ ! x "68" ! y "14" $ do
              "Scotty"

              {-
              <svg xmlns="http://www.w3.org/2000/svg" width="{{=it.widths[0]+it.widths[1]}}" height="18">
                <linearGradient id="smooth" x2="0" y2="100%">
                  <stop offset="0"  stop-color="#fff" stop-opacity=".7"/>
                  <stop offset=".1" stop-color="#aaa" stop-opacity=".1"/>
                  <stop offset=".9" stop-color="#000" stop-opacity=".3"/>
                  <stop offset="1"  stop-color="#000" stop-opacity=".5"/>
                </linearGradient>

                <mask id="round">
                  <rect width="{{=it.widths[0]+it.widths[1]}}" height="18" rx="4" fill="#fff"/>
                </mask>

                <g mask="url(#round)">
                  <rect width="{{=it.widths[0]}}" height="18" fill="{{=it.escapeXml(it.colorA||"#555")}}"/>
                  <rect x="{{=it.widths[0]}}" width="{{=it.widths[1]}}" height="18" fill="{{=it.escapeXml(it.colorB||"#4c1")}}"/>
                  <rect width="{{=it.widths[0]+it.widths[1]}}" height="18" fill="url(#smooth)"/>
                </g>

                <g fill="#fff" text-anchor="middle" font-family="DejaVu Sans,Verdana,Geneva,sans-serif" font-size="11">
                  <text x="{{=it.widths[0]/2+1}}" y="14" fill="#010101" fill-opacity=".3">{{=it.escapeXml(it.text[0])}}</text>
                  <text x="{{=it.widths[0]/2+1}}" y="13">{{=it.escapeXml(it.text[0])}}</text>
                  <text x="{{=it.widths[0]+it.widths[1]/2-1}}" y="14" fill="#010101" fill-opacity=".3">{{=it.escapeXml(it.text[1])}}</text>
                  <text x="{{=it.widths[0]+it.widths[1]/2-1}}" y="13">{{=it.escapeXml(it.text[1])}}</text>
                </g>
              </svg>

              -}
