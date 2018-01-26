(* Implementation of topol.mli           *)
(* Author: Mateusz Gienieczko 394302     *)
(* Reviewer: Wiktor Marszalkowski 394461 *)

(* Topological sort for directed acyclic graphs            *)
(* Denote number of vertices as V and number of edges as E *)
(* Time and memory complexity O(V + E)                     *)

(* Exception thrown when the graph is not acyclic *)
exception Cykliczne

(* Graphs are implemented as an array of adjacency lists      *)
(* along with an array that maps integers to vertices' labels *)
(* so internally vertices are labeled 0 through V - 1,        *)
(* but the original labels can be recovered for external use  *)
type 'a graph = { edges: int list array; labels: 'a array }

(* Vertex flag type for graph searching algorithms *)
type vstate = Unvisited | Visited | Processed

(* Number of vertices of the graph *)
(* Time complexity O(1)            *)
let size g =
  Array.length g.labels

(* Pair of:                                                                     *)
(* Array containing all unique labels in passed labels list                     *)
(* Hashtbl mapping labels to integers 0 through V - 1                           *)
(* Labels are unique if no other label compares to 0 using Pervasives.compare   *)
(* Average time complexity O(length labels), memory complexity O(length labels) *)
let map_labels labels =
  let n = List.length labels
  in
  let hashtbl = Hashtbl.create n
  and i = ref 0
  and list = ref []
  in
  let add x =
    try ignore (Hashtbl.find hashtbl x)
    with Not_found ->
      Hashtbl.add hashtbl x !i;
      list := x::(!list);
      incr i
  in
  List.iter add labels;
  (Array.of_list (List.rev !list), hashtbl)

(* Converts (int * (int list)) to array of lists of edges *)
(* All labels must be from 0 to n - 1                     *)
(* Time complexity O(V + E), memory complexity O(V + E)   *)
let make_edges_array g n =
  let array = Array.make n []
  in
  List.iter (fun (v, l) -> array.(v) <- l @ array.(v)) g;
  array

(* Converts labels in the adjacency lists to mappings from labels_hashtbl *)
(* Average time complexity O(V + E), memory complexity O(V + E)           *)
let convert_labels g labels_hashtbl =
  let find = Hashtbl.find labels_hashtbl
  in
  List.map (fun (v, l) -> (find v, List.map find l)) g

(* Makes a graph out of a (label * (label list)) list           *)
(* The list contains pairs of: vertex label, neighbors list     *)
(* The same vertex may appear more than once, but two identical *)
(* Labels always represent the same vertex                      *)
(* Vertices with no outgoing edges can be ommited on the list   *)
(* Time complexity O(V + E), memory complexity O(V + E)         *)
let make_graph g =
  let (labels, edges) = List.split g
  in
  let (unique_labels, hashtbl) = map_labels (labels @ (List.flatten edges))
  in
  let n = Array.length unique_labels
  and mapped_g = convert_labels g hashtbl
  in
  { edges = make_edges_array mapped_g n;
    labels = unique_labels }

(* List of vertices in reversed postorder, which is *)
(* a correct topological order                      *)
(* Raises Cykliczne if the graph is not acyclic     *)
(* Time complexity O(V + E), memory complexity O(V) *)
let topological_order g =
  let n = size g
  in
  let state = Array.make n Unvisited
  and result = ref []
  in
  let rec dfs v =
    if state.(v) = Visited then raise Cykliczne
    else if state.(v) = Unvisited then begin
      state.(v) <- Visited;
      List.iter dfs g.edges.(v);
      result := v::(!result);
      state.(v) <- Processed;
    end;
  in
  for i = 0 to n - 1 do
    dfs i
  done;
  !result
    
(* List of topologically sorted vertices (their original labels) *)
(* Raises Cykliczne if the graph is not acyclic                  *)
(* Average time complexity O(V + E), memory complexity O(V + E)  *)
let topol l =
  let g = make_graph l
  in
  List.map (Array.get g.labels) (topological_order g)
  
;;
