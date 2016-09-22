(* Data types *)

type cellType = Empty | Full

type cellTraversalState = {
    visited:  bool;
    cellKind: cellType;
}

module Point = struct
    type point = int * int

    type t = point

    let hash    (x, y)        = (string_of_int x) ^ "_" ^ (string_of_int y)
    let compare pointA pointB = compare (hash pointA) (hash pointB)
end

module PointMap = Map.Make(Point)

(* Utils *)

let indices length =
    let rec calculateIndices i current =
        match i with
        | 0 -> current
        | _ -> calculateIndices (i - 1) ((i - 1) :: current)
    in
    calculateIndices length []

let zipArrayWithIndices a = List.map
    (fun i -> (i, Array.get a i))
    (indices (Array.length a))

(* Dijkstra *)

let getInitialDistances grid origin =
    let getRowWithIndices row  = zipArrayWithIndices row  in
    let gridWithIndices        = zipArrayWithIndices grid in
        PointMap.add
            origin
            (Some 0)
            (
                List.fold_left
                    (
                        fun distances (y, row) ->
                            let rowWithIndices = getRowWithIndices row in
                                List.fold_left
                                    (
                                        fun distances (x, cell) ->
                                            match cell with
                                            | Full -> distances
                                            | Empty -> PointMap.add (x, y) None distances
                                    )
                                    distances
                                    rowWithIndices
                    )
                    PointMap.empty
                    gridWithIndices
            )

let dijkstra grid origin target =
    let gridState = Array.map
        (fun row -> Array.map (fun kind -> { visited = false; cellKind = kind }) row)
        grid
    in
        3

let () =
    let origin = (2, 2) in
    let target = (0, 4) in
    let grid = [|
        [|Empty; Empty; Empty; Empty; Empty|];
        [|Empty; Full;  Full;  Full;  Empty|];
        [|Empty; Empty; Empty; Full;  Empty|];
        [|Empty; Empty; Empty; Full;  Empty|];
        [|Empty; Empty; Empty; Full;  Empty|];
        [|Empty; Empty; Empty; Empty; Empty|];
    |] in
        let initialDistances = getInitialDistances grid origin in
            print_int (dijkstra grid origin target)
