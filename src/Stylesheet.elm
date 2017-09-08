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
    | PropertySeparator
    | PropertyValue
    | HtmlElement
    | SchemaHeader
    | JsonEditor
    | SourceCode
    | InlineError


type Variations
    = Active


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.stylesheet
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
            [ Color.text Color.darkGreen
            ]
        , style PropertySeparator
            [ Color.text Color.black
            , Style.paddingRightHint 5
            ]
        , style PropertyValue
            [ Color.text Color.darkOrange
            ]
        , style HtmlElement
            [ Font.typeface [ "menlo", "monospace" ]
            , Font.size 10
            , Color.text Color.blue
            ]
        , style Button
            [ Font.bold
            , Font.typeface [ "menlo", "monospace" ]
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
            , Style.prop "font-family" "monospace"
            , Color.text <| Color.rgb 17 123 206
            ]
        , style JsonEditor
            [ Style.prop "font-family" "monospace"
            , Font.size 10
            , Border.all 1
            , Color.border Color.lightGrey
            , Style.paddingHint 10
            ]
        , style SourceCode
            [ Style.prop "font-family" "monospace"
            , Font.size 10
            , Style.paddingHint 10
            ]
        ]
