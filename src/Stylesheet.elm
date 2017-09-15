module StyleSheet exposing (stylesheet, Styles(..), Variations(..))

import Style exposing (style, StyleSheet)
import Color
import Style.Color as Color
import Style.Font as Font
import Style.Border as Border


type Styles
    = None
    | Main
    | Bordered
    | Error
    | Button
    | NoOutline
    | MenuItem
    | HtmlAttribute
    | PropertyName
    | ItemIndex
    | PropertySeparator
    | PropertyValue
    | HtmlElement
    | SchemaHeader
    | JsonEditor
    | SourceCode
    | InlineError


type Variations
    = Active


fancyBlue : Color.Color
fancyBlue =
    Color.rgb 17 123 206


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheetWith [ Style.unguarded ]
        [ style None []
          -- It's handy to have a blank style
        , style Main
            [ Color.text Color.darkCharcoal
            , Color.background Color.white
            , Style.prop "font-family" "helvetica, arial, sans-serif"
            , Font.size 14
            , Style.paddingHint 10
            , Font.lineHeight 1.5
              -- line height, given as a ratio of current font size.
            ]
        , style Bordered
            [ Border.all 1
            , Color.border <| Color.rgb 230 230 230
            , Style.paddingHint 10
            ]
        , style Error
            [ Color.text Color.darkBrown
            , Color.background Color.white
            , Color.border Color.brown
            , Border.all 3
            , Border.rounded 3
            , Font.size 16
            , Style.paddingHint 5
            , Style.prop "margin" "10px"
            ]
        , style InlineError
            [ Style.prop "border" "1px solid red"
            , Style.prop "z-index" "1"
            , Style.prop "background" "white"
            , Style.paddingHint 10
            ]
        , style HtmlAttribute
            [ Color.text Color.darkBlue
              --, Style.paddingLeftHint 5
            ]
        , style PropertyName
            [ Style.prop "color" "#c80000"
            , Style.prop "outline" "none"
            ]
        , style ItemIndex
            [ Color.text fancyBlue
            , Style.prop "outline" "none"
            ]
        , style PropertySeparator
            [ Color.text Color.black
              --, Style.paddingRightHint 5
            , Style.prop "outline" "none"
            ]
        , style PropertyValue
            [ Color.text Color.darkCharcoal
            , Style.prop "outline" "none"
            ]
        , style HtmlElement
            [ Font.typeface [ "Menlo", "monospace" ]
            , Font.size 11
            , Color.text Color.blue
            ]
        , style Button
            [ Font.bold
            , Font.typeface [ "Menlo", "monospace" ]
            , Font.size 11
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Border.all 3
            , Border.solid
            , Border.rounded 3
            , Color.border Color.lightGrey
            , Style.paddingLeftHint 4
            , Style.paddingRightHint 4
            , Style.paddingTopHint 3
            , Style.paddingBottomHint 3
            ]
        , style NoOutline
            [ Style.prop "outline" "none"
            ]
        , style MenuItem
            [ Style.paddingHint 5
            , Style.prop "color" "royalblue"
            , Style.prop "background " "inherit"
            , Style.cursor "pointer"
              --, Style.prop "transition" "all .5s ease"
            , Style.variation Active
                [ Color.text Color.white
                , Style.prop "background " "#117bce"
                , Style.prop "text-decoration" "none"
                ]
            ]
        , style SchemaHeader
            [ Style.prop "outline" "none"
            , Style.prop "font-family" "Menlo, monospace"
            , Font.size 11
            , Color.text fancyBlue
            ]
        , style JsonEditor
            [ Style.prop "font-family" "Menlo, monospace"
            , Font.size 11
            , Border.all 1
            , Color.border Color.lightGrey
            , Style.prop "resize" "none"
            , Style.prop "box-shadow" "1px 1px 3px 0px rgba(0, 0, 0, 0.1)"
            , Style.prop "outline" "none"
            , Style.prop "display" "inline-block"
              --, Style.paddingHint 10
            ]
        , style SourceCode
            [ Style.prop "font-family" "Menlo, monospace"
            , Font.size 11
            , Style.paddingHint 10
            ]
        ]
