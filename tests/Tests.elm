module Tests exposing (..)

import Expect exposing (Expectation)
import Inflect
import String exposing (toUpper)
import String.Extra exposing (toTitleCase)
import Test exposing (..)
import Tuple exposing (pair)


examples : List ( String, String )
examples =
    [ -- (-f, -fe)
      pair "calf" "calves"
    , pair "dwarf" "dwarves"
    , pair "elf" "elves"
    , pair "fez" "fezes"
    , pair "half" "halves"
    , pair "hive" "hives"
    , pair "hoof" "hooves"
    , pair "knife" "knives"
    , pair "leaf" "leaves"
    , pair "life" "lives"
    , pair "loaf" "loaves"
    , pair "proof" "proofs"
    , pair "quiz" "quizzes"
    , pair "roof" "roofs"
    , pair "safe" "saves"
    , pair "scarf" "scarves"
    , pair "self" "selves"
    , pair "shelf" "shelves"
    , pair "thief" "thieves"
    , pair "wharf" "wharves"
    , pair "wife" "wives"
    , pair "wolf" "wolves"

    --
    , pair "neuropathy" "neuropathies"
    , pair "ability" "abilities"
    , pair "address" "addresses"
    , pair "agency" "agencies"
    , pair "box" "boxes"
    , pair "cape" "capes"
    , pair "case" "cases"
    , pair "category" "categories"
    , pair "dash" "dashes"
    , pair "fix" "fixes"
    , pair "flux" "fluxes"
    , pair "movie" "movies"
    , pair "process" "processes"
    , pair "query" "queries"
    , pair "search" "searches"
    , pair "shoe" "shoes"
    , pair "switch" "switches"
    , pair "wish" "wishes"

    -- (-o)
    , pair "potato" "potatoes"
    , pair "tomato" "tomatoes"
    , pair "hero" "heroes"
    , pair "torpedo" "torpedoes"
    , pair "veto" "vetoes"
    , pair "oasis" "oases"

    -- (-us) (-i)
    , pair "alumnus" "alumni"
    , pair "bacillus" "bacilli"
    , pair "cactus" "cacti"
    , pair "corpus" "corpora"
    , pair "focus" "foci"
    , pair "fungus" "fungi"
    , pair "genus" "genera"
    , pair "graffito" "graffiti"
    , pair "hippopotamus" "hippopotami"
    , pair "locus" "loci"
    , pair "nucleus" "nuclei"
    , pair "octopus" "octopi"
    , pair "opus" "opera"
    , pair "radius" "radii"
    , pair "stimulus" "stimuli"
    , pair "syllabus" "syllabi"
    , pair "virus" "viri"

    -- (-es)
    , pair "alias" "aliases"
    , pair "analysis" "analyses"
    , pair "antithesis" "antitheses"
    , pair "apex" "apices"
    , pair "axis" "axes"
    , pair "basis" "bases"
    , pair "bus" "buses"
    , pair "census" "censuses"
    , pair "circus" "circuses"
    , pair "codex" "codices"
    , pair "crisis" "crises"
    , pair "database" "databases"
    , pair "diagnosis" "diagnoses"
    , pair "ellipsis" "ellipses"
    , pair "estatus" "estatuses"
    , pair "fax" "faxes"
    , pair "fox" "foxes"
    , pair "hypothesis" "hypotheses"
    , pair "nemesis" "nemeses"
    , pair "neurosis" "neuroses"
    , pair "parenthesis" "parentheses"
    , pair "sense" "senses"
    , pair "status" "statuses"
    , pair "synopsis" "synopses"
    , pair "testis" "testes"
    , pair "thesis" "theses"

    -- (-ss)
    , pair "abyss" "abysses"
    , pair "foss" "fosses"

    -- (-on)
    , pair "phenomenon" "phenomena"
    , pair "criterion" "criteria"

    -- (-ae)
    , pair "alumna" "alumnae"
    , pair "antenna" "antennae"
    , pair "larva" "larvae"
    , pair "minutia" "minutiae"
    , pair "nebula" "nebulae"
    , pair "vertebra" "vertebrae"
    , pair "vita" "vitae"
    , pair "formula" "formulae"

    -- (-um)
    , pair "addendum" "addenda"
    , pair "agendum" "agenda"
    , pair "aquarium" "aquaria"
    , pair "bacterium" "bacteria"
    , pair "curriculum" "curricula"
    , pair "datum" "data"
    , pair "erratum" "errata"
    , pair "medium" "media"
    , pair "memorandum" "memoranda"
    , pair "millennium" "millennia"
    , pair "ovum" "ova"
    , pair "phylum" "phyla"
    , pair "referendum" "referenda"
    , pair "stadium" "stadia"
    , pair "stratum" "strata"
    , pair "symposium" "symposia"

    -- (-x)
    , pair "appendix" "appendices"
    , pair "index" "indices"
    , pair "matrix" "matrices"
    , pair "vertex" "vertices"
    , pair "vortex" "vortices"

    -- vowel change
    , pair "foot" "feet"
    , pair "tooth" "teeth"
    , pair "goose" "geese"
    , pair "man" "men"
    , pair "woman" "women"

    -- very irregular
    , pair "louse" "lice"
    , pair "child" "children"
    , pair "die" "dice"
    , pair "mouse" "mice"
    , pair "ox" "oxen"
    , pair "person" "people"
    , pair "faeces" "feces"
    , pair "salesperson" "salespeople"
    , pair "spokesman" "spokesmen"

    -- uncountable
    , pair "advice" "advice"
    , pair "aircraft" "aircraft"
    , pair "barracks" "barracks"
    , pair "bison" "bison"
    , pair "buffalo" "buffalo"
    , pair "deer" "deer"
    , pair "duck" "duck"
    , pair "equipment" "equipment"
    , pair "fish" "fish"
    , pair "gallows" "gallows"
    , pair "grouse" "grouse"
    , pair "hovercraft" "hovercraft"
    , pair "information" "information"
    , pair "jeans" "jeans"
    , pair "lux" "lux"
    , pair "means" "means"
    , pair "miniseries" "miniseries"
    , pair "moose" "moose"
    , pair "news" "news"
    , pair "offspring" "offspring"
    , pair "pants" "pants"
    , pair "pike" "pike"
    , pair "police" "police"
    , pair "rice" "rice"
    , pair "salmon" "salmon"
    , pair "scissors" "scissors"
    , pair "series" "series"
    , pair "sheep" "sheep"
    , pair "shrimp" "shrimp"
    , pair "spacecraft" "spacecraft"
    , pair "species" "species"
    , pair "squid" "squid"
    , pair "swine" "swine"
    , pair "trout" "trout"
    , pair "tuna" "tuna"

    -- regular
    , pair "archive" "archives"
    , pair "day" "days"
    , pair "document" "documents"
    , pair "edge" "edges"
    , pair "experience" "experiences"
    , pair "foo" "foos"
    , pair "horse" "horses"
    , pair "house" "houses"
    , pair "move" "moves"
    , pair "perspective" "perspectives"
    , pair "photo" "photos"
    , pair "portfolio" "portfolios"
    , pair "prize" "prizes"
    , pair "service" "services"
    , pair "shout" "shouts"
    , pair "slice" "slices"
    , pair "stack" "stacks"
    , pair "taxi" "taxis"
    , pair "underscore" "underscores"
    , pair "zombie" "zombies"

    -- other
    , pair "beau" "beaux"
    , pair "tableau" "tableaux"
    , pair "bureau" "bureaux"
    , pair "château" "châteaux"
    , pair "faux pas" "faux pas"
    , pair "concerto" "concerti"
    , pair "libretto" "libretti"
    , pair "comment" "comments"
    , pair "foobar" "foobars"
    , pair "newsletter" "newsletters"

    -- compound
    , pair "runner-up" "runners-up"
    , pair "son-in-law" "sons-in-law"

    -- with adjective
    , pair "quick fox" "quick foxes"
    , pair "the illustrated man" "the illustrated men"

    -- underscore, dash or symbols
    , pair "my_analysis" "my_analyses"
    , pair "node_child" "node_children"

    -- , pair "old-news" "old-news"
    , pair "matrix_fu" "matrix_fus"
    , pair "status_code" "status_codes"
    , pair "|ice" "|ices"
    , pair "|ouse" "|ouses"
    ]


suite : Test
suite =
    describe "Inflect"
        [ describe "toSingular"
            [ test "lower case string" <|
                \_ ->
                    Expect.all
                        (List.map
                            (\( singular, plural ) ->
                                always <|
                                    Expect.equal singular
                                        (Inflect.toSingular plural)
                            )
                            examples
                        )
                        ()
            , test "title case string" <|
                \_ ->
                    Expect.all
                        (List.map
                            (\( singular, plural ) ->
                                always <|
                                    Expect.equal (toTitleCase singular)
                                        (Inflect.toSingular
                                            (toTitleCase plural)
                                        )
                            )
                            examples
                        )
                        ()
            , test "upper case string" <|
                \_ ->
                    Expect.all
                        (List.map
                            (\( singular, plural ) ->
                                always <|
                                    Expect.equal (toUpper singular)
                                        (Inflect.toSingular (toUpper plural))
                            )
                            examples
                        )
                        ()
            ]
        , describe "toPlural"
            [ test "lower case string" <|
                \_ ->
                    Expect.all
                        (List.map
                            (\( singular, plural ) ->
                                always <|
                                    Expect.equal plural
                                        (Inflect.toPlural singular)
                            )
                            examples
                        )
                        ()
            , test "title case string" <|
                \_ ->
                    Expect.all
                        (List.map
                            (\( singular, plural ) ->
                                always <|
                                    Expect.equal (toTitleCase plural)
                                        (Inflect.toPlural
                                            (toTitleCase singular)
                                        )
                            )
                            examples
                        )
                        ()
            , test "upper case string" <|
                \_ ->
                    Expect.all
                        (List.map
                            (\( singular, plural ) ->
                                always <|
                                    Expect.equal (toUpper plural)
                                        (Inflect.toPlural (toUpper singular))
                            )
                            examples
                        )
                        ()
            ]
        ]
