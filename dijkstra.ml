
(* Data types *)
type cellType = Empty | Full | Out

module Point = struct
    type point = int * int

    type t = point

    let hash    (x, y)        = "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")"
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
    (fun cell -> true)

let getInitialCellIsOnPath = getInitialGridState

let getNeighbors grid gridState (x, y) =
    List.filter
    (fun neighbor -> getGridElement grid neighbor == Empty && not (PointMap.find neighbor gridState))
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
                | None, Some _                   -> 1
                | Some _, None                   -> -1
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
        | None          -> origin :: path
        | Some previous -> unwindPath previous pathMap (node :: path)
    in
        let nbNodes = PointMap.cardinal initialGridState in
            let rec dijkstraIteration distances path gridState nbVisitedNodes =
                if nbNodes == nbVisitedNodes then
                    unwindPath target path []
                else
                    let node = findNearestPoint distances gridState in
                        let newGridState = PointMap.add node true gridState in
                        let currentNodeDistance = PointMap.find node distances in
                            let neighbors = getNeighbors grid newGridState node in
                                let (newDistances, newPath) =
                                    List.fold_left
                                    (
                                        fun (distances, path) point ->
                                            let currentDistance  = PointMap.find point distances in
                                            let thisPathDistance =
                                                1
                                                + (
                                                    match PointMap.find node distances with
                                                    | Some distance -> distance
                                                    | None          -> (
                                                        match currentNodeDistance with
                                                        | Some distance -> distance
                                                        | None          -> failwith "noPath"
                                                    )
                                                )
                                            in
                                                match currentDistance with
                                                | Some distance when distance < thisPathDistance -> (distances, path)
                                                | _                                              -> (
                                                    PointMap.add point (Some thisPathDistance) distances,
                                                    PointMap.add point (Some node) path
                                                )
                                    )
                                    (distances, path)
                                    neighbors
                                in
                                    dijkstraIteration newDistances newPath newGridState (nbVisitedNodes + 1)
            in
                try
                    Some (dijkstraIteration initialDistances initialPath initialGridState 0)
                with error -> None
;;

(* Printing *)

let rec printPath path =
    match path with
    | Some []              -> ()
    | Some (point :: [])   -> print_string (Point.hash point)
    | None                 -> print_string "No path from origin to destination"
    | Some (point :: rest) -> print_string ((Point.hash point) ^ " -> "); printPath (Some rest)

let printPathOnGrid grid origin target path =
    let cellIsInPath =
        let pathOrDefault =
            match path with
            | None      -> []
            | Some path -> path
        in
            List.fold_left
            (fun cellIsInPath point -> PointMap.add point true cellIsInPath)
            (getInitialCellIsOnPath grid origin)
            pathOrDefault
    in
        List.iter
        (
            fun (y, row) ->
                List.iter
                (
                    fun (x, cell) ->
                        if PointMap.find (x, y) cellIsInPath then
                            print_string "\027[42m"
                        else
                            print_string "\027[0m"
                        ;
                        match cell with
                        | _ when compare (x, y) origin == 0 -> print_string "1"
                        | _ when compare (x, y) target == 0 -> print_string "X"
                        | Empty                             -> print_string "_"
                        | _                                 -> print_string "#"
                )
                (zipArrayWithIndices row)
                ;
                print_endline ""
        )
        (zipArrayWithIndices grid)

(* Entry point *)

let () =
    let grid =
        [|
            [|Empty; Empty; Empty; Empty; Empty|];
            [|Empty; Full;  Full;  Full;  Empty|];
            [|Empty; Empty; Empty; Full;  Empty|];
            [|Empty; Empty; Empty; Full;  Empty|];
            [|Empty; Empty; Empty; Full;  Empty|];
            [|Empty; Empty; Empty; Empty; Empty|];
        |]
    in
        let origin = (2, 2) in
        let target = (4, 0) in
            let path = dijkstra grid origin target in
                printPath path;
                print_endline "";
                print_endline "";
                printPathOnGrid grid origin target path
