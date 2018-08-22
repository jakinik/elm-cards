module Cards
    exposing
        ( Suit(..)
        , Rank(..)
        , Card(..)
        , fullDeck
        , ranks
        , suits
        , toRank
        , toSuit
        , shufle
        , isFigure
        )

import List exposing (..)
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
    (map LowRank (range 2 10))
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


shufle : Int -> List Card -> List Card
shufle initialSeedValue cards =
    cards
        |> foldl (accumulateWithNextSeed initialSeedValue) []
        |> sortCardsByGeneratedInt
        |> unzipCards


unzipCards : List ( Card, ( Int, Random.Seed ) ) -> List Card
unzipCards cardWithSeed =
    cardWithSeed
        |> List.map Tuple.first


sortCardsByGeneratedInt : List ( Card, ( Int, Random.Seed ) ) -> List ( Card, ( Int, Random.Seed ) )
sortCardsByGeneratedInt cardsWithSeed =
    cardsWithSeed
        |> sortBy (Tuple.second >> Tuple.first)


accumulateWithNextSeed : Int -> Card -> List ( Card, ( Int, Random.Seed ) ) -> List ( Card, ( Int, Random.Seed ) )
accumulateWithNextSeed initialSeedValue card cardsWithSeed =
    ( card, nextSeed initialSeedValue cardsWithSeed ) :: cardsWithSeed


nextSeed : Int -> List ( Card, ( Int, Random.Seed ) ) -> ( Int, Random.Seed )
nextSeed initialSeedValue cardsWithSeed =
    case cardsWithSeed of
        ( _, ( _, seed ) ) :: _ ->
            Random.step (Random.int 0 Random.maxInt) seed

        [] ->
            ( Random.maxInt // 2, Random.initialSeed initialSeedValue )
