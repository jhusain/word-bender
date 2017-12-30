module Main exposing (..)

import Html exposing (Html, text, div, img, input, select, option, button, table, tr, td, tbody)
import Html.Attributes exposing (style, src, value, href)
import Html.Events exposing (onClick, onInput)
import List exposing (map, sortBy, foldl, take, drop, indexedMap)
import Keyboard
import Random exposing (int, initialSeed, generate, step)
import Maybe
import Platform.Cmd
import Http exposing (encodeUri)
import List.Extra exposing (groupsOf)
import Date exposing (millisecond, Date)
import Task


words =
    [ "people", "history", "way", "art", "world", "information", "map", "family", "government", "health", "system", "computer", "meat", "year", "thanks", "music", "person", "reading", "method", "data", "food", "understanding", "theory", "law", "bird", "literature", "problem", "software", "control", "knowledge", "power", "ability", "economics", "love", "internet", "television", "science", "library", "nature", "fact", "product", "idea", "temperature", "investment", "area", "society", "activity", "story", "industry", "media", "thing", "oven", "community", "definition", "safety", "quality", "development", "language", "management", "player", "variety", "video", "week", "security", "country", "exam", "movie", "organization", "equipment", "physics", "analysis", "policy", "series", "thought", "basis", "boyfriend", "direction", "strategy", "technology", "army", "camera", "freedom", "paper", "environment", "child", "instance", "month", "truth", "marketing", "university", "writing", "article", "department", "difference", "goal", "news", "audience", "fishing", "growth", "income", "marriage", "user", "combination", "failure", "meaning", "medicine", "philosophy", "teacher", "communication", "night", "chemistry", "disease", "disk", "energy", "nation", "road", "role", "soup", "advertising", "location", "success", "addition", "apartment", "education", "math", "moment", "painting", "politics", "attention", "decision", "event", "property", "shopping", "student", "wood", "competition", "distribution", "entertainment", "office", "population", "president", "unit", "category", "cigarette", "context", "introduction", "opportunity", "performance", "driver", "flight", "length", "magazine", "newspaper", "relationship", "teaching", "cell", "dealer", "finding", "lake", "member", "message", "phone", "scene", "appearance", "association", "concept", "customer", "death", "discussion", "housing", "inflation", "insurance", "mood", "woman", "advice", "blood", "effort", "expression", "importance", "opinion", "payment", "reality", "responsibility", "situation", "skill", "statement", "wealth", "application", "city", "county", "depth", "estate", "foundation", "grandmother", "heart", "perspective", "photo", "recipe", "studio", "topic", "collection", "depression", "imagination", "passion", "percentage", "resource", "setting", "ad", "agency", "college", "connection", "criticism", "debt", "description", "memory", "patience", "secretary", "solution", "administration", "aspect", "attitude", "director", "personality", "psychology", "recommendation", "response", "selection", "storage", "version", "alcohol", "argument", "complaint", "contract", "emphasis", "highway", "loss", "membership", "possession", "preparation", "steak", "union", "agreement", "cancer", "currency", "employment", "engineering", "entry", "interaction", "mixture", "preference", "region", "republic", "tradition", "virus", "actor", "classroom", "delivery", "device", "difficulty", "drama", "election", "engine", "football", "guidance", "hotel", "owner", "priority", "protection", "suggestion", "tension", "variation", "anxiety", "atmosphere", "awareness", "bath", "bread", "candidate", "climate", "comparison", "confusion", "construction", "elevator", "emotion", "employee", "employer", "guest", "height", "leadership", "mall", "manager", "operation", "recording", "sample", "transportation", "charity", "cousin", "disaster", "editor", "efficiency", "excitement", "extent", "feedback", "guitar", "homework", "leader", "mom", "outcome", "permission", "presentation", "promotion", "reflection", "refrigerator", "resolution", "revenue", "session", "singer", "tennis", "basket", "bonus", "cabinet", "childhood", "church", "clothes", "coffee", "dinner", "drawing", "hair", "hearing", "initiative", "judgment", "lab", "measurement", "mode", "mud", "orange", "poetry", "police", "possibility", "procedure", "queen", "ratio", "relation", "restaurant", "satisfaction", "sector", "signature", "significance", "song", "tooth", "town", "vehicle", "volume", "wife", "accident", "airport", "appointment", "arrival", "assumption", "baseball", "chapter", "committee", "conversation", "database", "enthusiasm", "error", "explanation", "farmer", "gate", "girl", "hall", "historian", "hospital", "injury", "instruction", "maintenance", "manufacturer", "meal", "perception", "pie", "poem", "presence", "proposal", "reception", "replacement", "revolution", "river", "son", "speech", "tea", "village", "warning", "winner", "worker", "writer", "assistance", "breath", "buyer", "chest", "chocolate", "conclusion", "contribution", "cookie", "courage", "desk", "drawer", "establishment", "examination" ]


randInt =
    int 1 100


main : Program Never Model Msg
main =
    Html.program
        { init = ( { seed = initialSeed 42, players = [], phone = "", playerType = Moderator, game = Nothing, words = words, admin = False }, now )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


randomizeList seed list =
    let
        ( nextSeed, newList ) =
            list
                |> foldl
                    (\word ( nextSeed, accWords ) ->
                        let
                            ( num, newSeed ) =
                                step randInt nextSeed
                        in
                            ( newSeed, ( word, num ) :: accWords )
                    )
                    ( seed, [] )
    in
        let
            sortedWords =
                newList
                    |> sortBy (\( _, num ) -> num)
                    |> map (\( word, _ ) -> word)
        in
            ( nextSeed, sortedWords )



-- TYPES


type SquareType
    = Kill
    | TeamASquare
    | TeamBSquare
    | Neutral


type alias Square =
    { word : String, squareType : SquareType, visible : Bool }


type alias Game =
    Maybe (List Square)


type PlayerType
    = Moderator
    | Normal


type alias Player =
    { phone : String, playerType : PlayerType }


type alias Model =
    { seed : Random.Seed, players : List Player, phone : String, playerType : PlayerType, game : Game, words : List String, admin : Bool }


type Msg
    = AddPlayer
    | StartGame
    | ChangePhone String
    | ChangeType String
    | SelectSquare String
    | ToggleAdmin
    | StartApp (Maybe Date)



-- MODEL
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePhone phoneValue ->
            ( { model | phone = phoneValue }, Cmd.none )

        ChangeType playerTypeString ->
            ( { model
                | playerType =
                    case playerTypeString of
                        "Moderator" ->
                            Moderator

                        _ ->
                            Normal
              }
            , Cmd.none
            )

        AddPlayer ->
            ( { model
                | players = { phone = model.phone, playerType = model.playerType } :: model.players
              }
            , Cmd.none
            )

        ToggleAdmin ->
            ( { model | admin = not model.admin }, Cmd.none )

        SelectSquare word ->
            case model.game of
                Just g ->
                    ( { model
                        | game =
                            Just
                                (map
                                    (\card ->
                                        { card
                                            | visible =
                                                if card.word == word then
                                                    True
                                                else
                                                    card.visible
                                        }
                                    )
                                    g
                                )
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        StartGame ->
            let
                ( brandNewSeed, sortedWords ) =
                    randomizeList model.seed model.words
            in
                let
                    ( teamASquares, teamBSquares, killSquares, neutralSquares ) =
                        ( sortedWords
                            |> take 9
                            |> map (\word -> { word = word, squareType = TeamASquare, visible = False })
                        , sortedWords
                            |> drop 9
                            |> take 8
                            |> map (\word -> { word = word, squareType = TeamBSquare, visible = False })
                        , sortedWords
                            |> drop 17
                            |> take 1
                            |> map (\word -> { word = word, squareType = Kill, visible = False })
                        , sortedWords
                            |> drop 18
                            |> take 7
                            |> map (\word -> { word = word, squareType = Neutral, visible = False })
                        )
                in
                    let
                        ( finalSeed, game ) =
                            randomizeList brandNewSeed (teamASquares ++ teamBSquares ++ killSquares ++ neutralSquares)
                    in
                        ( { model | seed = finalSeed, game = Just game }, Cmd.none )

        StartApp maybeDate ->
            case maybeDate of
                Just date ->
                    ( { model | seed = initialSeed (millisecond date) }, Cmd.none )

                Nothing ->
                    ( { model | seed = initialSeed 33 }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ button [ onClick StartGame ]
            [ text "Start Game" ]
        , button
            [ onClick ToggleAdmin ]
            [ text "Reveal All" ]
        , case model.game of
            Nothing ->
                text ""

            Just game ->
                table
                    [ style [ ( "width", "100%" ), ( "height", "100%" ) ] ]
                    [ tbody
                        []
                        (map
                            (\row ->
                                tr []
                                    (map
                                        (\col ->
                                            let
                                                color =
                                                    case col.squareType of
                                                        Neutral ->
                                                            "yellow"

                                                        Kill ->
                                                            "#CCCCCC"

                                                        TeamASquare ->
                                                            "cyan"

                                                        TeamBSquare ->
                                                            "red"
                                            in
                                                td
                                                    [ onClick (SelectSquare col.word)
                                                    , style
                                                        [ ( "font-size", "150%" )
                                                        , ( "background-color"
                                                          , if (model.admin || col.visible) then
                                                                color
                                                            else
                                                                "white"
                                                          )
                                                        ]
                                                    ]
                                                    [ text col.word ]
                                        )
                                        row
                                    )
                            )
                            (groupsOf 5 game)
                        )
                    ]
        ]


now =
    Task.perform (Just >> StartApp) Date.now
