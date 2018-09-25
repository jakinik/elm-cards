module CardsTest exposing (..)

import Cards exposing (..)
import Expect exposing (..)
import Fuzz exposing (..)
import List
import Random exposing (Generator, Seed, initialSeed, maxInt, minInt)
import Set
import Test exposing (..)


retrievalTest : Test
retrievalTest =
    describe "Test retrieval"
        [ fuzz2 suitFuzzer rankFuzzer "rank" <| \suit rank -> toRank (Card suit rank) |> equal rank
        , fuzz2 suitFuzzer rankFuzzer "suit" <| \suit rank -> toSuit (Card suit rank) |> equal suit
        ]


fullDeckTest : Test
fullDeckTest =
    describe "Full Deck"
        [ test "contains 52 cards" <| \_ -> List.length fullDeck |> equal 52
        , test "contains 13 unique Spades" <| \_ -> countUniqueRankBySuit Spades fullDeck |> equal 13
        , test "contains 13 unique Clubs" <| \_ -> countUniqueRankBySuit Clubs fullDeck |> equal 13
        , test "contains 13 unique Diamonds" <| \_ -> countUniqueRankBySuit Diamonds fullDeck |> equal 13
        , test "contains 13 unique Hearts" <| \_ -> countUniqueRankBySuit Hearts fullDeck |> equal 13
        ]


shuffleTest : Test
shuffleTest =
    describe "Test that check properties of shuffle"
        [ fuzz seedFuzzer "shuffle of empty deck" <| \seed -> randomValue (shuffleOf []) seed |> equalLists []
        , fuzz2 seedFuzzer cardFuzzer "shuffle one card" <|
            \seed card -> randomValue (shuffleOf [ card ]) seed |> equalLists [ card ]
        , fuzz seedFuzzer "shuffle of full deck retains number of cards" <|
            \seed -> randomValue (shuffleOf fullDeck) seed |> List.length |> equal 52
        ]


countUniqueRankBySuit : Suit -> List Card -> Int
countUniqueRankBySuit suit cards =
    Set.size
        (Set.fromList
            (cards
                |> List.filter (toSuit >> (==) suit)
                |> List.map toRank
                |> List.map
                    rankOrdinal
            )
        )


rankOrdinal : Rank -> Int
rankOrdinal rank =
    case rank of
        Ace ->
            14

        King ->
            13

        Queen ->
            12

        Jack ->
            11

        LowRank number ->
            number


intRandomRange : Fuzzer Int
intRandomRange =
    Fuzz.intRange Random.minInt Random.maxInt


seedFuzzer : Fuzzer Random.Seed
seedFuzzer =
    Fuzz.map Random.initialSeed intRandomRange


suitFuzzer : Fuzzer Suit
suitFuzzer =
    suits
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


rankFuzzer : Fuzzer Rank
rankFuzzer =
    ranks
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


cardFuzzer : Fuzzer Card
cardFuzzer =
    Fuzz.map2 (\suit rank -> Card suit rank) suitFuzzer rankFuzzer


randomValue : Random.Generator a -> Seed -> a
randomValue generator seed =
    seed |> Random.step generator |> Tuple.first
