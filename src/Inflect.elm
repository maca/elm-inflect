module Inflect exposing (toPlural, toSingular)

{-| String pluralization and singularization

@docs toPlural, toSingular

-}

import Regex exposing (Regex)
import String exposing (toUpper)
import String.Extra exposing (toTitleCase)
import Tuple exposing (pair)


{-| Convert word to plural form.

    toPlural "potato" == "potatoes"

    toPlural "cat" == "cats"

    toPlural "The Illustrated Man" == "The Illustrated Men"

    toPlural "SHOUT" == "SHOUTS"

-}
toPlural : String -> String
toPlural string =
    inflect expressionsPlural string


{-| Convert word to singlular form.

    toSingular "potatoes" == "potato"

    toSingular "cats" == "cat"

    toSingular "The Illustrated Men" == "The Illustrated Man"

    toSingular "SHOUTS" == "SHOUT"

-}
toSingular : String -> String
toSingular string =
    inflect expressionsSingular string


inflect : List ( String, String ) -> String -> String
inflect expressions string =
    List.foldl (replaceFoldFun string) Nothing expressions
        |> Maybe.withDefault string


replaceFoldFun : String -> ( String, String ) -> Maybe String -> Maybe String
replaceFoldFun string ( expression, replacement ) result =
    case result of
        Just _ ->
            result

        Nothing ->
            Regex.find (parse (expression ++ "$")) string
                |> List.head
                |> Maybe.andThen
                    (\{ submatches, match } ->
                        case submatches of
                            a :: (Just _) :: _ ->
                                Just
                                    (String.replace match
                                        (Maybe.withDefault "" a ++ replacement)
                                        string
                                    )

                            (Just s) :: _ ->
                                Just
                                    (String.replace match
                                        (s ++ replacement)
                                        string
                                    )

                            _ ->
                                Nothing
                    )


expressionsPlural : List ( String, String )
expressionsPlural =
    List.map (\( s, p ) -> ( "(^|-|_|\\s)(" ++ s ++ ")", p )) irregulars
        ++ [ expressionsUncountable identity uncountable
           , expressionsUncountable toTitleCase uncountable
           , expressionsUncountable toUpper uncountable
           ]
        ++ List.concatMap caseSensitiveExpressions
            [ pair "(proof|roof)" "s"
            , pair "(quiz)" "zes"
            , pair "(kni|li|sa|wi)fe" "ves"
            , pair "(radi)us" "i"
            , pair "(alumn|bacill|cact|foc|fung|hippopotam|loc|nucle|vir|octop|stimul|syllab)us" "i"
            , pair "(phenomen|criteri)on" "a"
            , pair "(appendi|matri)x" "ces"
            , pair "(tomato|potato|torpedo|hero|veto)" "es"
            , pair "(categor|quer|agenc|abilit|neuropath)y" "ies"
            , pair "(.*)man" "men"
            , pair "(.*)person" "people"
            , pair "(.*)um" "a"
            , pair "(.*)a" "ae"
            , pair "(.*)ex" "ices"
            , pair "(.*)f" "ves"
            , pair "(.*)is" "es"
            , pair "(.*[sxh])" "es"
            , pair "(.*au)" "x"
            , pair "([A-Z][A-Z].*)" "S"
            , pair "(.*)" "s"
            ]


expressionsSingular : List ( String, String )
expressionsSingular =
    List.map (\( s, p ) -> ( "(^|-|_|\\s)(" ++ p ++ ")", s )) irregulars
        ++ List.concatMap caseSensitiveExpressions
            [ pair "(movi|sho)es" "e"
            , pair "(bus|fax|fox|alias|estatus|sex|census|circus)es" ""
            , pair "(hiv)es" "e"
            , pair "(ax)es" "is"
            , pair "(alia|analy|antithe|oa|ba|diagno|ellip|hypothe|neme|neuro|parenthe|synop|the)ses" "sis"
            , pair "(cris|test)es" "is"
            , pair "(alumn|bacill|cact|foc|fung|hippopotam|loc|nucle|vir|octop|stimul|syllab)i" "us"
            , pair "(phenomen|criteri)a" "on"
            , pair "(millenni|agend|dat|memorand|medi|aquari|bacteri|strat|curricul|addend|errat|ov|phyl|referend|symposi|stadi)a" "um"
            , pair "(cal|lea|hoo|loa|scar|sel|thie|whar|wol|dwar|hal|el)ves" "f"
            , pair "(kni|li|sa|wi)ves" "fe"
            , pair "(appendi|matri)ces" "x"
            , pair "(ap|cod|ind|vert|vort)ices" "ex"
            , pair "(series|news|species|barracks|gallows|means|lux)" ""
            , pair "(categor|quer|agenc|abilit|neuropath)ies" "y"
            , pair "(cap|sens|zombi|servic|cas)es" "e"
            , pair "(flux|abyss|foss|search|switch|fix|box|process|address|wish|status|dash)es" ""
            , pair "(.*i)i" "us"
            , pair "(.*)men" "man"
            , pair "(.*)people" "person"
            , pair "(.*)ae" "a"
            , pair "(.*)ux" "u"
            , pair "(.*o)es" ""
            , pair "(.*z)zes" ""
            , pair "(.*)s" ""
            ]


caseSensitiveExpressions : ( String, String ) -> List ( String, String )
caseSensitiveExpressions tuple =
    [ tuple
    , Tuple.mapFirst titleizeExpression tuple
    , Tuple.mapBoth toUpper toUpper tuple
    ]


expressionsUncountable : (String -> String) -> List String -> ( String, String )
expressionsUncountable transform nouns =
    ( "(?:^|-|_|\\s)(" ++ String.join "|" (List.map transform nouns) ++ ")"
    , ""
    )


irregulars : List ( String, String )
irregulars =
    List.concatMap
        (\tuple ->
            [ tuple
            , Tuple.mapBoth toTitleCase toTitleCase tuple
            , Tuple.mapBoth toUpper toUpper tuple
            ]
        )
        [ pair "child" "children"
        , pair "corpus" "corpora"
        , pair "concerto" "concerti"
        , pair "database" "databases"
        , pair "die" "dice"
        , pair "faeces" "feces"
        , pair "faux pas" "faux pas"
        , pair "fez" "fezes"
        , pair "foot" "feet"
        , pair "genus" "genera"
        , pair "goose" "geese"
        , pair "jeans" "jeans"
        , pair "graffito" "graffiti"
        , pair "libretto" "libretti"
        , pair "louse" "lice"
        , pair "man" "men"
        , pair "person" "people"
        , pair "mouse" "mice"
        , pair "opus" "opera"
        , pair "pants" "pants"
        , pair "runner-up" "runners-up"
        , pair "scissors" "scissors"
        , pair "son-in-law" "sons-in-law"
        , pair "tooth" "teeth"
        , pair "ox" "oxen"
        ]


uncountable : List String
uncountable =
    [ "advice"
    , "aircraft"
    , "barracks"
    , "bison"
    , "buffalo"
    , "deer"
    , "duck"
    , "equipment"
    , "fish"
    , "gallows"
    , "grouse"
    , "hovercraft"
    , "information"
    , "jeans"
    , "lux"
    , "means"
    , "miniseries"
    , "moose"
    , "news"
    , "offspring"
    , "pants"
    , "pike"
    , "police"
    , "rice"
    , "salmon"
    , "scissors"
    , "series"
    , "sheep"
    , "shrimp"
    , "spacecraft"
    , "species"
    , "squid"
    , "swine"
    , "trout"
    , "tuna"
    ]


titleizeExpression : String -> String
titleizeExpression expression =
    Regex.replace (parse "([(|])(\\w)")
        (\{ submatches } ->
            case submatches of
                (Just a) :: (Just s) :: _ ->
                    a ++ toTitleCase s

                _ ->
                    ""
        )
        expression


parse : String -> Regex
parse =
    Regex.fromString >> Maybe.withDefault Regex.never
