module Cards
    exposing
        ( Card(..)
        , Rank(..)
        , Suit(..)
        , fullDeck
        , isFigure
        , ranks
        , shuffle
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


type Card
    = Card Suit Rank


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
isFigure card =
    isFigureRank (toRank card)


isFigureRank : Rank -> Bool
isFigureRank rank =
    case rank of
        LowRank _ ->
            False

        _ ->
            True


toRank : Card -> Rank
toRank card =
    case card of
        Card suit rank ->
            rank


toSuit : Card -> Suit
toSuit card =
    case card of
        Card suit rank ->
            suit


shuffle : Random.Seed -> List Card -> List Card
shuffle initialSeed cards =
    cards
        |> zipWithRandomList initialSeed
        |> sortCardsByGeneratedInt
        |> unzipCards


zipWithRandomList : Random.Seed -> List Card -> List ( Card, Int )
zipWithRandomList initialSeed cards =
    List.map2 Tuple.pair cards (
    generateRandomList initialSeed cards)


generateRandomList : Random.Seed -> List Card -> List Int
generateRandomList initialSeed cards =
    Tuple.first <|
        Random.step
            (randomListGenerator cards)
            initialSeed


randomListGenerator : List a -> List Int
randomListGenerator list =
    Random.list
        (List.length list)
        (Random.int Random.minInt Random.maxInt)


sortCardsByGeneratedInt : List ( Card, Int ) -> List ( Card, Int )
sortCardsByGeneratedInt cardsWithSeed =
    cardsWithSeed |> sortBy Tuple.second


unzipCards : List ( Card, Int ) -> List Card
unzipCards cardWithSeed =
    cardWithSeed |> List.map Tuple.first
