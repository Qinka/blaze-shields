# blaze-shields
中文
## 介绍
blaze-shields 是利用Haskell中的DSL —— **blaze-markup** 与 **blaze-svg** 近似地实现了 ［shields.io](https://shields.io) 中几种图标的生成。

## 安装

```bash
cabal update
cabal inatsll blaze-shields
```

## 使用
再 GHCi 中加载 Text.Blaze.Svg.Shields Text.Blaze.Svg.Renderer.String 两个 module 之后，键入：
```
Prelude Text.Blaze.Svg.Shields Text.Blaze.Svg.Renderer.String>  writeFile "a.svg" $ renderSvg $ socialStyle  ("a",7) ("b",7) (Just "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0MCIgaGVpZ2h0PSI0MCIgdmlld0JveD0iMTIgMTIgNDAgNDAiPgo8cGF0aCBmaWxsPSIjMzMzMzMzIiBkPSJNMzIsMTMuNGMtMTAuNSwwLTE5LDguNS0xOSwxOWMwLDguNCw1LjUsMTUuNSwxMywxOGMxLDAuMiwxLjMtMC40LDEuMy0wLjljMC0wLjUsMC0xLjcsMC0zLjIgYy01LjMsMS4xLTYuNC0yLjYtNi40LTIuNkMyMCw0MS42LDE4LjgsNDEsMTguOCw0MWMtMS43LTEuMiwwLjEtMS4xLDAuMS0xLjFjMS45LDAuMSwyLjksMiwyLjksMmMxLjcsMi45LDQuNSwyLjEsNS41LDEuNiBjMC4yLTEuMiwwLjctMi4xLDEuMi0yLjZjLTQuMi0wLjUtOC43LTIuMS04LjctOS40YzAtMi4xLDAuNy0zLjcsMi01LjFjLTAuMi0wLjUtMC44LTIuNCwwLjItNWMwLDAsMS42LTAuNSw1LjIsMiBjMS41LTAuNCwzLjEtMC43LDQuOC0wLjdjMS42LDAsMy4zLDAuMiw0LjcsMC43YzMuNi0yLjQsNS4yLTIsNS4yLTJjMSwyLjYsMC40LDQuNiwwLjIsNWMxLjIsMS4zLDIsMywyLDUuMWMwLDcuMy00LjUsOC45LTguNyw5LjQgYzAuNywwLjYsMS4zLDEuNywxLjMsMy41YzAsMi42LDAsNC42LDAsNS4yYzAsMC41LDAuNCwxLjEsMS4zLDAuOWM3LjUtMi42LDEzLTkuNywxMy0xOC4xQzUxLDIxLjksNDIuNSwxMy40LDMyLDEzLjR6Ii8+Cjwvc3ZnPgo=") (Just "#fed")  (Just "#def")
```

## 效果
使用浏览器打开 a.svg

Try to do what shields do with Haskell
