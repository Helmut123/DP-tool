port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import File exposing (File)
import Html.Events exposing (..)
import Json.Decode as Json
import Task
import String as Str
import List
import Basics
import Regex exposing (..)
import Elm.Parser exposing (..)
import Elm.Processing as Processing exposing (..)
import Elm.RawFile exposing (..)
import Round
import Graph exposing (..)
import Graph.DOT exposing (..)

import Elm.Syntax.Import exposing (..)
import Elm.Syntax.File exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node exposing (..)
import Elm.Syntax.Range exposing (..)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.ModuleName exposing (..)

---- MODEL ----


type alias Model =
    { files : List UploadedFile
    , loc : Int
    , min_loc : Int
    , max_loc : Int
    , locwe : Int
    , min_locwe : Int
    , max_locwe : Int
    , units : Int
    , min_units : Int
    , max_units : Int
    , units_loc : Int
    , min_units_loc : Int
    , max_units_loc : Int
    , show_loaded : Bool
    , imports : Int
    , min_imports : Int
    , max_imports : Int
    , show_diagram : Bool
    , show_boiler : Bool
    , graph_nodes : List (Graph.Node ModuleName)
    , graph_edges : List (Graph.Edge String) }

type alias UploadedFile = 
    { buff : File.File, content : String, name : ModuleName, loc : Int, locwe : Int, units : Int, units_loc : Int, aff : Int, eff : Int, imports : Int }


init : () -> ( Model, Cmd msg )
init _ =
    ( { files = [], loc = 0, min_loc = 99999, max_loc = 0, locwe = 0, min_locwe = 99999, max_locwe = 0, units = 0, min_units = 99999, max_units = 0, units_loc = 0, min_units_loc = 99999, max_units_loc = 0, show_loaded = False, imports = 0, min_imports = 99999, max_imports = 0, show_diagram = False, show_boiler = False, graph_nodes = [], graph_edges = [] }, Cmd.none )


---- UPDATE ----


type Msg
    = ChooseDirectory (List File.File)
    | ReadFiles
    | FileReadSuccess String Int String
    | DisplayDiagram
    | AnalyzeBoilerplate
--    | AnalyzeApplication (List UploadedFile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseDirectory files ->
            let
                filterFiles = List.filter (\fil -> isElmFile (File.name fil)) files
            in
                ( { model | files = List.map (\file -> UploadedFile file "" [""] 0 0 0 0 0 0 0) filterFiles, loc = 0, min_loc = 99999, max_loc = 0, locwe = 0, min_locwe = 99999, max_locwe = 0, units = 0, min_units = 99999, max_units = 0, units_loc = 0, min_units_loc = 99999, max_units_loc = 0, show_loaded = False, imports = 0, min_imports = 99999, max_imports = 0, show_diagram = False, show_boiler = False, graph_nodes = [], graph_edges = [] }, Cmd.none )
        
        ReadFiles ->
            ( { model | show_loaded = True }
            , List.map
                (\file ->
                    File.toString file.buff |> Task.perform (FileReadSuccess (File.name file.buff) (File.size file.buff))
                )
                model.files
                |> Cmd.batch
            )

        FileReadSuccess name size content ->
            let
                fileName = getFileName content
                linesOfCode = (List.length (Str.split "\n" content))
                linesOfCodeWithoutEmpty = (List.length (List.filter (\x -> not (Str.isEmpty x) && Str.length x > 1) (Str.split "\n" content))) - (getCommentsLines content)
                unitsCount = (List.length (List.filter (\unit -> getFunctionDeclaration unit) (getUnits content)))
                unitsLinesOfCode = (getUnitsLoc content)
            in
            ( { model
                | files =
                    List.map
                        (\file ->
                            if File.size file.buff == size then
                                { file | content = content
                                    , name = fileName
                                    , loc = linesOfCode
                                    , locwe = linesOfCodeWithoutEmpty
                                    , units =  unitsCount
                                    , units_loc = unitsLinesOfCode
                                    , imports = getAllImports content
                                }

                            else
                                file
                        )
                        model.files
                , loc = model.loc + linesOfCode
                , max_loc = getMax model.max_loc linesOfCode
                , min_loc = getMin model.min_loc linesOfCode
                , locwe = model.locwe + linesOfCodeWithoutEmpty
                , max_locwe = getMax model.max_locwe linesOfCodeWithoutEmpty
                , min_locwe = getMin model.min_locwe linesOfCodeWithoutEmpty
                , units = model.units + unitsCount
                , max_units = getMax model.max_units unitsCount
                , min_units = getMin model.min_units unitsCount
                , units_loc = model.units_loc + unitsLinesOfCode
                , max_units_loc = getMax model.max_units_loc unitsLinesOfCode
                , min_units_loc = getMin model.min_units_loc unitsLinesOfCode
               -- , min_imports = getMin model.min_imports (getAllImports content)
               -- , max_imports = getMax model.max_imports (getAllImports content)
               -- , imports = model.imports + getAllImports content
              }
            , Cmd.none
            )

        DisplayDiagram ->
            ( { model | show_diagram = True, graph_nodes = getNodes model.files
   --             ,graph_edges = 
            }, Cmd.none )

        AnalyzeBoilerplate ->
            ( { model | show_boiler = True }, Cmd.none )


nodeName : ModuleName -> Maybe String
nodeName name =
    case List.length(name) of
        0 ->
            Nothing
        _ ->
            List.head (List.reverse name)
            --Just (Str.replace "[" "" (Str.replace "]" "" (Str.replace "\"" "" (Debug.toString name))))

edgeName : ModuleName -> Maybe String
edgeName name =
    Nothing
{-    case List.length(name) of
        0 ->
            Nothing
        _ ->
            Just (Str.replace "[" "" (Str.replace "]" "" (Str.replace "\"" "" (Debug.toString name)))) -}
        

getEdgeImports : ModuleName -> List UploadedFile -> Bool
getEdgeImports name files =
    if (List.length (List.filter (\file -> file.name == name) files)) > 0 then
        True
    else
        False

getModuleImports : String -> List UploadedFile -> List ModuleName
getModuleImports input files = 
    let
        imps = getImports input
    in
        (List.filter (\imp -> getEdgeImports imp files) imps)

getNodeId : ModuleName -> List (Graph.Node ModuleName) -> Graph.NodeId
getNodeId name nodes =
    let
        nod = Maybe.withDefault ({id = 0, label = getMN "zero"}) (List.head (List.filter (\node -> name == node.label) nodes))
    in
        nod.id

{- createEdge : ModuleName -> ModuleName -> Graph.Edge n
createEdge a b =
    { from = (getNodeId a), to = (getNodeId b), label = "edge" } -}

getEdges : List UploadedFile -> List (Graph.Node ModuleName) -> List (List (Graph.Edge ModuleName))
getEdges files nodes =
    --List.map (\file -> (List.map (\imp -> createEdge (getFileName file.content) imp ) (getModuleImports file.content files))) files
    List.map (\file -> (List.map (\imp -> { from = (getNodeId (getFileName file.content) nodes), to = (getNodeId imp nodes), label = imp } ) (getModuleImports file.content files))) files

getNodes : List UploadedFile -> List (Graph.Node ModuleName)
getNodes files =
    List.indexedMap (\i file -> { id = i, label = getFileName file.content } ) files

isElmFile : String -> Bool
isElmFile name =
    Debug.log name
    String.contains (Str.right 4 name) ".elm"

getElement : Int -> List a -> Maybe a
getElement nth list =
    list 
        |> List.drop (nth - 1)
        |> List.head

findImport : String -> Regex.Regex
findImport fileName = 
    Maybe.withDefault Regex.never <|
        Regex.fromString ("import Components\\.[a-zA-Z\\.]*" ++ fileName ++ " ")

getMax : Int -> Int -> Int
getMax a b =
    if a > b then
        a
    else
        b

getMin : Int -> Int -> Int
getMin a b =
    if a > b then
        b
    else
        a

getAllImports : String -> Int
getAllImports input =
    case Elm.Parser.parse input of
        Err e ->
            0
        Ok v ->
            List.length (imports v)

getFileName : String -> ModuleName
getFileName input =
    case Elm.Parser.parse input of
        Err e ->
            [Debug.toString e]
        Ok v ->
            Elm.RawFile.moduleName v

returnFile : RawFile -> Elm.Syntax.File.File
returnFile rawfile =
    process (Processing.init) rawfile


getFunctionDeclaration : Elm.Syntax.Node.Node Declaration -> Bool
getFunctionDeclaration dec =
    case Elm.Syntax.Node.value dec of
        FunctionDeclaration _ ->
            True
        PortDeclaration _ ->
            True
        _ ->
            False

getUnits : String -> List (Elm.Syntax.Node.Node Declaration)
getUnits input =
    case Elm.Parser.parse input of
        Err e ->
            []
        Ok v ->
            let
                mod = (returnFile v)
            in
                mod.declarations

getEndRange : Elm.Syntax.Range.Range -> Int
getEndRange input =
    let
        locat = input.end
    in
        locat.row +1

getStartRange : Elm.Syntax.Range.Range -> Int
getStartRange input =
    let
        locat = input.start
    in
        locat.row

getCommentsLines : String -> Int
getCommentsLines input =
    case Elm.Parser.parse input of
        Err e ->
            0
        Ok v ->
            let
                mod = (returnFile v)
            in
                List.sum (List.map (\comment -> getEndRange (Elm.Syntax.Node.range comment) - getStartRange (Elm.Syntax.Node.range comment)) (mod.comments))

getUnitsLoc : String -> Int
getUnitsLoc input =
    case Elm.Parser.parse input of
        Err e ->
            0
        Ok v ->
            let
                mod = (returnFile v)
            in
                List.sum (List.map (\declaration -> getEndRange (Elm.Syntax.Node.range declaration) - getStartRange (Elm.Syntax.Node.range declaration)) (List.filter (\declar -> getFunctionDeclaration declar) mod.declarations))

getImportName : Elm.Syntax.Node.Node Import -> ModuleName
getImportName input = 
    let
        imp = Elm.Syntax.Node.value input
    in
        Elm.Syntax.Node.value imp.moduleName

getMN : String -> ModuleName
getMN input =
    [input]

getImports : String -> List ModuleName
getImports input = 
    case Elm.Parser.parse input of
        Err e ->
            [getMN ""]
        Ok v ->
            let
                mod = (returnFile v)
            in
                (List.map (\imp -> getImportName imp ) mod.imports)

getInternalImports : ModuleName -> Model -> Bool
getInternalImports name model =
    if (List.length (List.filter (\modul -> modul.name == name) model.files)) > 0 then
        True
    else
        False

getEff : String -> Model -> Int
getEff input model = 
    let
        imps = getImports input
    in
        List.length (List.filter (\imp -> getInternalImports imp model) imps)

getInternalConnections : ModuleName -> List ModuleName -> Bool
getInternalConnections name names =
    if List.length (List.filter (\nameModule -> nameModule == name) names) > 0 then
        True
    else
        False

getAff : String -> Model -> Int
getAff input model =
    let
        files = model.files
        name = getFileName input
    in
        List.length (List.filter (\file -> getInternalConnections name (getImports file.content)) files)

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
    [ 
        label [
            for "file-upload"
            , class "custom-file-upload"
        ]
        [
            if (List.length model.files) == 0 then
                h1 [][ text "Click to upload elm-app folder" ]
            else
                if model.show_loaded then
                    h1 [][ text "Click to upload different elm-app folder" ]
                else
                    h1 [][ text ("Found " ++ Str.fromInt (List.length model.files) ++ " .elm files") ]
        ]
        , input
        [ 
            type_ "file"
            , id "file-upload"
            , attribute "webkitdirectory" ""
            , on "change" (Json.map ChooseDirectory filesDecoder)
        ]
        []
        , div []
        [
            div [] [
                if (List.length model.files) > 0 && not model.show_loaded then
                    button [ onClick ReadFiles, class "read-files" ] [ text ("Analyze code") ]
                else
                   div [] []
            ]
        ] 
        , div [ class "wrapper" ]
        [
            div [ class "elm-staties" ]
            [
                if model.show_loaded then
                        div []
                        [
                            div [ class "elm-stats-shown" ]
                            [
                                p [ class "elm-stats-header" ] [ text "Elm application modules" ]
                                , div [ class "modules-heading" ]
                                [
                                    p [ class "modules-name" ] [ b [] [ text "Module" ] ]
                                    , p [ class "modules-value" ] [ b [] [ text "LOC" ] ]
                                    , p [ class "modules-value" ] [ b [] [ text "LOC we" ] ]
                                    , p [ class "modules-value" ] [ b [] [ text "UNITs" ] ]
                                    , p [ class "modules-value" ] [ b [] [ text "UNITs LOC" ] ]
                                    , p [ class "modules-value" ] [ b [] [ text "UNITs LOC average" ] ]
                                    , p [ class "modules-value" ] [ b [] [ text "External imports" ] ]
                                    , p [ class "modules-value" ] [ b [] [ text "Eff. connections" ] ]
                                    , p [ class "modules-value" ] [ b [] [ text "Aff. connections" ] ]
                                    , p [ class "modules-value" ] [ b [] [ text "Unstability" ] ]
                                ]
                                , div [] <| List.map
                            (\file ->
                                div []
                                [
                                    let
                                        efferent = getEff file.content model
                                        afferent = getAff file.content model
                                    in
                                        if efferent > afferent then
                                            div [ class "modules-warning" ]
                                            [ 
                                                p [ class "modules-name" ] [ text (Maybe.withDefault "unknown" (List.head (List.reverse (file.name)))) ]
                                                , p [ class "modules-value" ] [ text (Str.fromInt file.loc) ]
                                                , p [ class "modules-value" ] [ text (Str.fromInt file.locwe)]
                                                , p [ class "modules-value" ] [ text (String.fromInt file.units) ]
                                                , p [ class "modules-value" ] [ text (Str.fromInt file.units_loc) ]
                                                , p [ class "modules-value" ] [ text (Round.round 3 (toFloat file.units_loc/toFloat file.units)) ]
                                                , p [ class "modules-value" ] [ text (Str.fromInt (file.imports - (getEff file.content model))) ]
                                                , p [ class "modules-value" ] [ text (String.fromInt (efferent)) ]
                                                , p [ class "modules-value" ] [ text (Str.fromInt (afferent)) ]
                                                , p [ class "modules-value" ] [ text (Round.round 3 (toFloat efferent/ ((toFloat efferent)+(toFloat (afferent))))) ]
                                            ]
                                        else
                                            div [ class "modules-normal" ]
                                            [ 
                                                p [ class "modules-name" ] [ text (Maybe.withDefault "unknown" (List.head (List.reverse (file.name)))) ]
                                                , p [ class "modules-value" ] [ text (Str.fromInt file.loc) ]
                                                , p [ class "modules-value" ] [ text (Str.fromInt file.locwe) ]
                                                , p [ class "modules-value" ] [ text (String.fromInt file.units) ]
                                                , p [ class "modules-value" ] [ text (Str.fromInt file.units_loc) ]
                                                , p [ class "modules-value" ] [ text (Round.round 3 (toFloat file.units_loc/toFloat file.units)) ]
                                                , p [ class "modules-value" ] [ text (Str.fromInt (file.imports - (getEff file.content model))) ]
                                                , p [ class "modules-value" ] [ text (String.fromInt (efferent)) ]
                                                , p [ class "modules-value" ] [ text (String.fromInt (afferent)) ]
                                                , p [ class "modules-value" ] [ text (Round.round 3 (toFloat efferent/ ((toFloat efferent)+(toFloat (afferent))))) ]
                                            ]
                                ]
                            )
                            model.files
                                , div []
                                [
                                    if model.show_diagram then
                                        div [ class "boilerplate" ]
                                        [
                                            p [ class "elm-stats-header" ] [ text "Modules connections diagram" ]
                                        --    , p [] [ text (Debug.toString model.graph_nodes) ]
                                        --    , p [] [ text (Debug.toString (List.concat (getEdges model.files model.graph_nodes))) ]
                                        --    , p [] [ text (Debug.toString (Graph.fromNodesAndEdges model.graph_nodes (List.concat (getEdges model.files model.graph_nodes)))) ]
                                            , p [] [ text (Graph.DOT.output nodeName edgeName (Graph.fromNodesAndEdges model.graph_nodes (List.concat (getEdges model.files model.graph_nodes)))) ]
                                        ]
                                    else
                                        div []
                                        [
                                            button [ onClick DisplayDiagram, class "read-boiler" ] [ text ("Display modules connections") ]
                                        ]
                                ]
                            ]
                        ]
                else
                    div [][]
            ]
            , div [ class "elm-stats" ]
            [
                if model.show_loaded then
                    div [ class "elm-stats-shown" ]
                    [
                        div []
                        [
                            p [ class "elm-stats-header" ] [ text "Elm application" ]
                            , div [ class "table-heading" ]
                            [
                                p [ class "table-name" ] [ b [] [ text "Metric" ] ]
                                , p [ class "table-value" ] [ b [] [ text "Count" ] ]
                                , p [ class "table-value" ] [ b [] [ text "Min" ] ]
                                , p [ class "table-value" ] [ b [] [ text "Max" ] ]
                                , p [ class "table-value" ] [ b [] [ text "Average" ] ]
                            ]
                            , div [ class "table-row" ]
                            [
                                p [ class "table-name" ] [ text "Files"  ]
                                , p [ class "table-value" ] [ text (Str.fromInt (List.length (model.files))) ]
                            ]
                            , div [ class "table-row" ]
                            [
                                p [ class "table-name" ] [ text "LOC" ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.loc) ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.min_loc) ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.max_loc) ]
                                , p [ class "table-value" ] [ text (Round.round 3 ((toFloat model.loc)/(toFloat (List.length model.files)))) ]
                            ]
                            , div [ class "table-row" ]
                            [
                                p [ class "table-name" ] [ text "LOC we" ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.locwe) ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.min_locwe) ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.max_locwe) ]
                                , p [ class "table-value" ] [ text (Round.round 3 ((toFloat model.locwe)/(toFloat (List.length model.files)))) ]
                            ]
                            , div [ class "table-row" ]
                            [
                                p [ class "table-name" ] [ text "UNITs" ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.units) ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.min_units) ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.max_units) ]
                                , p [ class "table-value" ] [ text (Round.round 3 ((toFloat model.units) / (toFloat (List.length model.files)))) ]
                            ]
                            , div [ class "table-row" ]
                            [
                                p [ class "table-name" ] [ text "UNITs LOC" ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.units_loc) ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.min_units_loc) ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.max_units_loc) ]
                                , p [ class "table-value" ] [ text (Round.round 3 ((toFloat model.units_loc) / (toFloat (List.length model.files)))) ]
                            ]
                            , div [ class "table-row" ]
                            [
                                p [ class "table-name" ] [ text "UNITs/UNITs LOC" ]
                                , p [ class "table-value" ] [ text (Round.round 3 ((toFloat model.units_loc)/(toFloat model.units))) ]
                                , p [ class "table-value" ] [ text "" ]
                                , p [ class "table-value" ] [ text "" ]
                                , p [ class "table-value" ] [ text ""]
                            ]
                       {--     , div [ class "table-row" ]
                            [
                                p [ class "table-name" ] [ text "External imports" ]
                                , p [ class "table-value" ] [ text ("") ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.min_imports) ]
                                , p [ class "table-value" ] [ text (Str.fromInt model.max_imports) ]
                                , p [ class "table-value" ] [ text (Round.round 3 ((toFloat model.imports) / (toFloat (List.length model.files)))) ]
                            ] --}
                        ]
                        , div []
                        [
                            if model.show_boiler then
                                div [ class "boilerplate" ]
                                [
                                    p [ class "elm-stats-header" ] [ text "Boilerplate code" ]
                                    , div [] <| List.map
                                        (\file ->
                                            div [ class "parsed" ]
                                            [
                                                p [ class "parsed-name" ] [ text (File.name file.buff) ]
                                                , p [] [ text (Debug.toString (getUnits file.content)) ]
                                            ]
                                        )
                                        model.files
                                ]
                            else
                                div []
                                [
                                    button [ onClick AnalyzeBoilerplate, class "read-boiler" ] [ text ("Analyze boilerplate code") ]
                                ]
                        ]
                        

                        
                    ]
                else
                   div [] [] 
            ]
        ]
    ]

---- PROGRAM ----


filesDecoder =
    Json.at [ "target", "files" ] (Json.list File.decoder)

port sendDOT : String -> Cmd msg

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
