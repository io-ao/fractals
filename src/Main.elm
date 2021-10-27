module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)


type alias Model =
    { count : Float }
    
type alias SubSquares =
    { size : Float
    , squares : List (Float, Float) }


type Msg
    = Frame Float

main = view

width =
    600


height =
    600


centerX =
    width / 2


centerY =
    height / 2


view =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "8px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , shapes [ fill Color.lightPurple ] (List.concat (sieRemoved 5 600.0))
            ]
        ]


clearScreen =
    shapes [ fill Color.darkPurple  ] [ rect ( 0, 0 ) width height ]
    
renderSieRemoved removedSub =
    let size = removedSub.size
        squars = removedSub.squares
    in
        List.map (\squar -> rect squar size size ) squars
    
sub_squares subsquare =
    let
        size = subsquare.size
        squares = subsquare.squares
        subSize = size / 3
        
        sub_square position =
            let
                x = Tuple.first position
                y = Tuple.second position
                midx = x +  subSize
                endx = x + (subSize * 2)
                midy = y + subSize
                endy = y + (subSize * 2)
            in
                ( (midx, midy) , [(x, y), (midx, y), (endx, y) 
                                  ,(x, midy),        (endx, midy)
                                  ,(x, endy),(midx, endy), (endx, endy)] )
    in
        let
            sub = List.unzip ( List.map sub_square squares)
            removed = Tuple.first sub
            newSub = List.concat ( Tuple.second sub )
        in
            (SubSquares subSize removed, SubSquares subSize newSub)
            
sieRemoved iteration size =
    let
        helper iteration1 suSquare cutout =
            if iteration1 <= 0 then cutout
            else
                let
                    iter = sub_squares suSquare
                in
                    helper (iteration1 - 1) ( Tuple.second iter)  ((Tuple.first iter)::cutout)
    
    in
       List.map renderSieRemoved (helper iteration (SubSquares size [( 0, 0 )]) [])