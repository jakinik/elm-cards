module Main exposing (Model, Msg(..), init, main, update, view, viewCard, viewRank, viewSuit)

import Browser
import Cards exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import List
import Random


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    List Card



-- UPDATE


type Msg
    = Reset
    | ChangeDeck (List Card)
    | Shuffle


init : flags -> ( Model, Cmd Msg )
init flags =
    ( fullDeck, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( fullDeck, Cmd.none )

        ChangeDeck deck ->
             ( deck, Cmd.none )

        Shuffle ->
            ( model, Random.generate ChangeDeck (shuffleOf model) )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Shuffle ] [ text "Shuffle" ]
        , ul
            []
            (li [] [ text ("Number of cards: " ++ Debug.toString (List.length model)) ]
                :: (List.map
                        viewCard
                        model
                   )
            )
        ]


viewCard : Card -> Html Msg
viewCard card =
    li []
        [ text
            ((card |> toRank |> viewRank) ++ (card |> toSuit |> viewSuit))
        ]


viewSuit : Suit -> String
viewSuit suit =
    case suit of
        Hearts ->
            "H"

        Clubs ->
            "C"

        Diamonds ->
            "D"

        Spades ->
            "S"


viewRank : Rank -> String
viewRank rank =
    case rank of
        Ace ->
            "A"

        King ->
            "K"

        Queen ->
            "Q"

        Jack ->
            "J"

        LowRank number ->
            Debug.toString number




-------------- SUBS

subscriptions: Model -> Sub Msg
subscriptions model = Sub.none