open! Core
module Interstate = String
module City = String

module Network = struct
  module Connection = struct
    module T = struct
      type t = Interstate.t * (City.t * City.t) [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    let rec make_city_pairs_from_list
              (pair_city : string)
              (city_list : string list)
              (city_pairs : (string * string) list)
      =
      let fix_strings str =
        String.map str ~f:(fun a ->
          if Char.equal a '.' || Char.equal a ' ' then '_' else a)
      in
      let city = fix_strings pair_city in
      if Int.equal (List.length city_list) 1
      then (
        match city_list with
        | x :: [] ->
          (City.of_string city, City.of_string (fix_strings x)) :: city_pairs
        | _ -> failwith "There should only be one element")
      else (
        let add_cityp =
          List.map city_list ~f:(fun a ->
            City.of_string city, City.of_string (fix_strings a))
        in
        let new_city_pairs = List.append city_pairs add_cityp in
        make_city_pairs_from_list
          (List.hd_exn city_list)
          (List.tl_exn city_list)
          new_city_pairs)
    ;;

    let make_connections_from_list
          (interstate : string)
          (cities : string list)
      =
      let city_conns =
        make_city_pairs_from_list
          (List.hd_exn cities)
          (List.tl_exn cities)
          []
      in
      List.map city_conns ~f:(fun a -> interstate, a)
    ;;

    let of_string s =
      match String.split s ~on:',' with
      | x :: y ->
        Some (make_connections_from_list (Interstate.of_string x) y)
      | _ -> None
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s ->
        match Connection.of_string s with
        | Some conns_list ->
          List.concat_map conns_list ~f:(fun (a, (b, c)) ->
            [ a, (b, c); a, (c, b) ])
        | None -> failwith "Could not make line into connections")
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values (whose types
           have [sexp_of_t] implemented). *)
        printf !"%{sexp: Network.t}\n" network]
;;

module G = Graph.Imperative.Graph.Concrete (City)

module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        ignore (input_file : File_path.t);
        ignore (output_file : File_path.t);
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (interstate, (city1, city2)) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
             they don't already exist. *)
          ignore interstate;
          G.add_edge graph city1 city2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
