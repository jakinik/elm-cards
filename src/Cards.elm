module Cards
    exposing
        ( Card
        , Rank(..)
        , Suit(..)
        , fullDeck
        , isFigure
        , ranks
        , shuffleOf
        , suits
        , toRank
        , toSuit
        )

import List exposing (..)
import Tuple exposing (..)
import Random


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Rank
    = Ace
    | King
    | Queen
    | Jack
    | LowRank Int


type alias Card =
    { suit : Suit, rank : Rank }


suits : List Suit
suits =
    [ Clubs, Diamonds, Hearts, Spades ]


ranks : List Rank
ranks =
    map LowRank (range 2 10)
        ++ [ Jack, Queen, King, Ace ]


fullDeck : List Card
fullDeck =
    suits |> concatMap fullSuit


fullSuit : Suit -> List Card
fullSuit suit =
    map (Card suit) ranks


isFigure : Card -> Bool
isFigure { suit, rank } =
    case rank of
        LowRank _ ->
            False

        _ ->
            True


toRank : Card -> Rank
toRank { suit, rank } =
    rank


toSuit : Card -> Suit
toSuit { suit, rank } =
    suit


shuffleOf : List Card -> Random.Generator (List Card)
shuffleOf cards =
    randomListGenerator cards |> Random.map (shuffleList cards)


shuffleList : List a -> List Int -> List a
shuffleList toShuffle randomList =
    randomList
        |> map2 Tuple.pair toShuffle
        |> sortBy Tuple.second
        |> map Tuple.first


randomListGenerator : List a -> Random.Generator (List Int)
randomListGenerator list =
    Random.list
        (List.length list)
        (Random.int Random.minInt Random.maxInt)
