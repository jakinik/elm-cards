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
    describe "Card component retrieval"
        [ fuzz2 suitFuzzer rankFuzzer "rank" <| \suit rank -> toRank (Card suit rank) |> equal rank
        , fuzz2 suitFuzzer rankFuzzer "suit" <| \suit rank -> toSuit (Card suit rank) |> equal suit
        ]


fullDeckTest : Test
fullDeckTest =
    describe "Full Deck"
        [ test "contains 52 cards" <| \_ -> List.length fullDeck |> equal 52
        , fuzz cardFuzzer "contains only unique cards" <|
            \card -> fullDeck |> List.filter ((==) card) |> List.length |> equal 1
        ]


shuffleTest : Test
shuffleTest =
    describe "Check properties of shuffle"
        [ fuzz seedFuzzer "shuffle of empty deck" <| \seed -> randomValue (shuffleOf []) seed |> equalLists []
        , fuzz2 seedFuzzer cardFuzzer "shuffle one card" <|
            \seed card -> randomValue (shuffleOf [ card ]) seed |> equalLists [ card ]
        , fuzz seedFuzzer "shuffle of full deck retains number of cards" <|
            \seed -> randomValue (shuffleOf fullDeck) seed |> List.length |> equal 52
        , fuzz2 seedFuzzer cardFuzzer "after shuffling deck has only unique cards" <|
            \seed card -> randomValue (shuffleOf fullDeck) seed |> List.filter ((==) card) |> List.length |> equal 1
        ]


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
