module Html.Internal where

newtype Html = Html String 
newtype Structure = Structure String
type Title = String

getStuctureString :: Structure -> String
getStuctureString struct =
    case struct of 
        Structure str -> str

html_ :: Title -> Structure -> Html
html_ title (Structure content) =  Html (el "head" (el "title" (escape title)) <> el "body" content)

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat .  map (el "il" . getStuctureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat .  map (el "il" . getStuctureString)

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"


instance Semigroup Structure where
    (<>) (Structure first) (Structure second) = 
        Structure (first <> second)

render :: Html -> String
render (Html html) = html

escape :: String -> String
escape = 
    let 
    escapeChar c =
        case c of
            '<' -> "&lt;"
            '>' -> "&gt;"
            '"' -> "&quot;"
            '\'' -> "&#39;"
            _ -> [c]
    in 
        concat . map escapeChar

