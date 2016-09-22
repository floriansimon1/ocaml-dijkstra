
(* Data types *)
type cellType = Empty | Full | Out

module Point = struct
    type point = int * int

    type t = point

    let hash    (x, y)        = "(" ^ (string_of_int x) ^ "_" ^ (string_of_int y) ^ ")"
    let compare pointA pointB = compare (hash pointA) (hash pointB)
end

module PointMap = Map.Make(Point)

(* Utils *)

let between lo hi value = lo <= value && hi >= value

let getGridElement grid (x, y) =
    if
        between 0 ((Array.length grid) - 1) y
        && between 0 ((Array.length (Array.get grid 0)) - 1) x
    then
        Array.get (Array.get grid y) x
    else
        Out

let indices length =
    let rec calculateIndices i current =
        match i with
        | 0 -> current
        | _ -> calculateIndices (i - 1) ((i - 1) :: current)
    in
        calculateIndices length []

let zipArrayWithIndices a =
    List.map
    (fun i -> (i, Array.get a i))
    (indices (Array.length a))

let initializeMap grid origin fixedValue originValue predicate =
    let getRowWithIndices row  = zipArrayWithIndices row in
    let gridWithIndices        = zipArrayWithIndices grid in
        PointMap.add
        origin
        originValue
        (
            List.fold_left
            (
                fun distances (y, row) ->
                    let rowWithIndices = getRowWithIndices row in
                    List.fold_left
                    (
                        fun distances (x, cell) ->
                            match predicate cell with
                            | true  -> PointMap.add (x, y) fixedValue distances
                            | false -> distances
                    )
                    distances
                    rowWithIndices
            )
            PointMap.empty
            gridWithIndices
        )

let printPath path = List.iter print_string (List.map Point.hash path)

(* Dijkstra *)

let getInitialDistances grid origin =
    initializeMap
    grid
    origin
    None
    (Some 0)
    (fun cell -> cell == Empty)

let getInitialPath grid origin =
    initializeMap
    grid
    origin
    None
    None
    (fun _ -> true)

let getInitialGridState grid origin =
    initializeMap
    grid
    origin
    false
    false
    (fun cell -> cell == Empty)

let getNeighbors grid (x, y) =
    List.filter
    (fun neighbor -> getGridElement grid neighbor == Empty)
    (
        List.map
        (fun (i, j) -> (x + i, y + j))
        [(-1, 0); (1, 0); (0, -1); (0, 1)]
    )

let findNearestPoint distances gridState =
    let sortedDistances =
        List.sort
        (
            fun (point1, distance1) (point2, distance2) ->
                match distance1, distance2 with
                | None, None                     -> 0
                | Some _, None                   -> 1
                | None, Some _                   -> -1
                | Some distance1, Some distance2 -> compare distance1 distance2
        )
        (
            List.filter
            (fun (point, _) -> not (PointMap.find point gridState))
            (PointMap.bindings distances)
        )
    in
        match sortedDistances with
        | (point, _) :: _ -> point
        | _               -> (0, 0) (* Cannot happen *)

let dijkstra grid origin target =
    let initialDistances = getInitialDistances grid origin in
    let initialGridState = getInitialGridState grid origin in
    let initialPath      = getInitialPath grid origin in
    let rec unwindPath node pathMap path =
        match PointMap.find node pathMap with
        | Some origin when origin == node -> path
        | None                            -> path
        | Some previous                   -> unwindPath previous pathMap (node :: path)
    in
        let nbNodes = PointMap.cardinal initialGridState in
            let rec dijkstraIteration distances path gridState nbVisitedNodes =
                if nbNodes == nbVisitedNodes then
                    unwindPath target path []
                else
                    let node = findNearestPoint distances gridState in
                        let neighbors = getNeighbors grid node in
                            let (newDistances, newPath) =
                                List.fold_left
                                (
                                    fun (distances, path) point ->
                                        let currentDistance  = PointMap.find point distances in
                                        let thisPathDistance =
                                            match PointMap.find node distances with
                                            | Some distance -> distance + 1
                                            | None          -> 1
                                        in
                                            match currentDistance with
                                            | Some distance when distance <= thisPathDistance -> (
                                                PointMap.add point (Some thisPathDistance) distances,
                                                PointMap.add point (Some node) path
                                            )
                                            | _ -> (distances, path)
                                )
                                (distances, path)
                                neighbors
                            in
                                let newGridState = PointMap.add node true gridState in
                                    dijkstraIteration newDistances newPath newGridState (nbVisitedNodes + 1)
            in
                dijkstraIteration initialDistances initialPath initialGridState 0
;;

let () =
    printPath
    (
        dijkstra
        [|
            [|Empty; Empty; Empty; Empty; Empty|];
            [|Empty; Full;  Full;  Full;  Empty|];
            [|Empty; Empty; Empty; Full;  Empty|];
            [|Empty; Empty; Empty; Full;  Empty|];
            [|Empty; Empty; Empty; Full;  Empty|];
            [|Empty; Empty; Empty; Empty; Empty|];
        |]
        (2, 2)
        (0, 4)
    )
