open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice thing about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a"
  |> to_list
  |> List.filter_map ~f:(fun anchor ->
    match attribute "href" anchor with
    | None -> None
    | Some link -> Some (String.strip link))
  |> List.filter ~f:(fun link -> String.is_prefix link ~prefix:"/wiki/")
  |> List.filter ~f:(fun link ->
    match Wikipedia_namespace.namespace link with None -> true | _ -> false)
  |> List.dedup_and_sort ~compare:String.ascending
;;

let%expect_test "get_linked_articles" =
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Cat"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
    /wiki/Carnivore
    /wiki/Domestication_of_the_cat
    /wiki/Mammal
    /wiki/Species
    |}]
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

let get_contents_from_url
      (url : string)
      ~(how_to_fetch : File_fetcher.How_to_fetch.t)
  =
  match how_to_fetch with
  | Local path -> File_fetcher.fetch_exn (Local path) ~resource:url
  | Remote -> File_fetcher.fetch_exn Remote ~resource:url
;;

let get_title_from_contents contents =
  let open Soup in
  let title = parse contents $ "title" |> R.leaf_text in
  match String.chop_suffix title ~suffix:" - Wikipedia" with
  | None -> ""
  | Some title ->
    String.map title ~f:(fun a ->
      if Char.equal a '.' || Char.equal a ' ' then '_' else a)
    |> String.map ~f:(fun a ->
      if Char.equal a '(' || Char.equal a ')' then '_' else a)
;;

module Links = struct
  type t = string * string list [@@deriving compare, sexp, hash, equal]
end

module Links_Sets = Hash_set.Make (Links)

module Article = struct
  type t =
    { title : string
    ; url : string
    }
  [@@deriving sexp, compare]

  let make_into_article url ~how_to_fetch =
    let contents = get_contents_from_url url ~how_to_fetch in
    let title = get_title_from_contents contents in
    { title; url }
  ;;
end

module Network = struct
  module Connection = struct
    module T = struct
      type t = Article.t * Article.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    let get_connections_from_list
          (list : Links.t list)
          ~(how_to_fetch : File_fetcher.How_to_fetch.t)
      =
      List.concat_map list ~f:(fun (link, adjacent_links) ->
        List.map adjacent_links ~f:(fun a_link ->
          ( Article.make_into_article link ~how_to_fetch
          , Article.make_into_article a_link ~how_to_fetch )))
    ;;

    let get_pairs_of_linked_articles origin ~how_to_fetch depth =
      let visited = Links_Sets.create () in
      let rec dfs current_node layer : unit =
        let adjacent_nodes =
          get_linked_articles
            (get_contents_from_url current_node ~how_to_fetch)
        in
        Hash_set.add visited (current_node, adjacent_nodes);
        List.iter adjacent_nodes ~f:(fun next_node ->
          if not (Int.equal layer 0) then dfs next_node (layer - 1))
      in
      dfs origin (depth + 1);
      let pairs_list = Hash_set.to_list visited in
      get_connections_from_list pairs_list ~how_to_fetch
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_url
        (url : string)
        (depth : int)
        ~(how_to_fetch : File_fetcher.How_to_fetch.t)
    =
    let connections =
      List.concat_map
        (Connection.get_pairs_of_linked_articles url ~how_to_fetch depth)
        ~f:(fun (a, b) -> [ a, b ])
    in
    Connection.Set.of_list connections
  ;;
end

module G = Graph.Imperative.Graph.Concrete (String)

module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `Forward ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let network = Network.of_url origin max_depth ~how_to_fetch in
  let graph = G.create () in
  Set.iter network ~f:(fun (article1, article2) ->
    (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
       they don't already exist. *)
    G.add_edge graph article1.title article2.title);
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
