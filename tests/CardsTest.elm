module CardsTest exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect exposing (..)
import Cards exposing (..)
import List
import Set
import Random exposing (minInt, maxInt)


toRankTest : Test
toRankTest =
    describe "Ranks"
        [ test "Ace of Spades" <|
            \_ -> toRank (Card Spades Ace) |> equal Ace
        , test "King of Diamonds" <|
            \_ -> toRank (Card Diamonds King) |> equal King
        , test "Queen of Clubs" <|
            \_ -> toRank (Card Clubs Queen) |> equal Queen
        , test "Jack of Hearts" <|
            \_ -> toRank (Card Hearts Jack) |> equal Jack
        , test "Ten of Clubs" <|
            \_ -> toRank (Card Clubs (LowRank 10)) |> equal (LowRank 10)
        , test "Six of Diamonds" <|
            \_ -> toRank (Card Diamonds (LowRank 6)) |> equal (LowRank 6)
        ]


toSuitTest : Test
toSuitTest =
    describe "Suits"
        [ test "Ace of Spades" <|
            \_ -> toSuit (Card Spades Ace) |> equal Spades
        , test "King of Diamonds" <|
            \_ -> toSuit (Card Diamonds King) |> equal Diamonds
        , test "Queen of Clubs" <|
            \_ -> toSuit (Card Clubs Queen) |> equal Clubs
        , test "Jack of Hearts" <|
            \_ -> toSuit (Card Hearts Jack) |> equal Hearts
        , test "Ten of Clubs" <|
            \_ -> toSuit (Card Clubs (LowRank 10)) |> equal Clubs
        , test "Six of Diamonds" <|
            \_ -> toSuit (Card Diamonds (LowRank 6)) |> equal Diamonds
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


shufleTest : Test
shufleTest =
    describe "Test that check properties of shufle"
        [ fuzz intRandomRange "shufle of empty deck" <| \seed -> shufle seed [] |> equalLists []
        , fuzz2 intRandomRange cardFuzzer "shufle one card" <|
            \seed card -> shufle seed [ card ] |> equalLists [ card ]
        , fuzz intRandomRange "shufle of full deck retains number of cards" <|
            \seed -> shufle seed fullDeck |> List.length |> equal 52
        ]


countUniqueRankBySuit : Suit -> List Card -> Int
countUniqueRankBySuit suit cards =
    Set.size
        (Set.fromList
            (cards
                |> List.filter (toSuit >> ((==) suit))
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
