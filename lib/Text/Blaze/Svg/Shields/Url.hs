{-# LANGUAGE OverloadedStrings #-}




module Text.Blaze.Svg.Shields.Url
    (
    SvgShields(..),
    eval
    ) where

      import Text.Blaze(ToMarkup)
      import Text.Blaze.Svg(Svg)
      import Text.Blaze.Svg.Shields(
        plasticStyle,
        flatStyle,
        socialStyle,
        flatSquareStyle
        )
      import Data.Text.Lazy(unpack)
      import Data.Text.Internal.Lazy(Text)




      data (Show a,ToMarkup a,Read a,Show b,Floating b,Read b) => (SvgShields a b)
        = PlasticStyle (a,b) (a,b) (Maybe String) (Maybe String)
        | FlatStyle (a,b) (a,b) (Maybe String) (Maybe String)
        | FlatSquareStyle (a,b) (a,b) (Maybe String) (Maybe String)
        | SocialStyle (a,b) (a,b) (Maybe String) (Maybe String) (Maybe String)
        deriving (Show,Read)

      eval :: Text -> Svg
      eval url= case ssData of
          PlasticStyle (l,lp) (r,rp) cA cB ->
            plasticStyle (l,lp) (r,rp) cA cB
          FlatStyle (l,lp) (r,rp) cA cB ->
            flatStyle (l,lp) (r,rp) cA cB
          FlatSquareStyle (l,lp) (r,rp) cA cB ->
            flatSquareStyle (l,lp) (r,rp) cA cB
          SocialStyle (l,lp) (r,rp) cA cB u ->
            socialStyle (l,lp) (r,rp) cA cB u
          where
            ssData = read url' ::SvgShields String Double
            url' = unpack url
