open! Core
module City = String

module Highway = struct
  include String

  let default = ""
end

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = City.t * Highway.t * City.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let rec tuples highway ~cities acc =
    match cities with
    | city_one :: city_two :: tail ->
      let city_one =
        String.substr_replace_all
          (String.substr_replace_all city_one ~pattern:"." ~with_:"")
          ~pattern:" "
          ~with_:""
      in
      let city_two =
        String.substr_replace_all
          (String.substr_replace_all city_two ~pattern:"." ~with_:"")
          ~pattern:" "
          ~with_:""
      in
      tuples
        highway
        ~cities:([ city_two ] @ tail)
        (acc @ [ city_one, city_two, highway ])
    | _ -> acc
  ;;

  let load_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
    in
    List.concat_map connections ~f:(fun x ->
      let line = String.split x ~on:',' in
      let head =
        String.substr_replace_all
          (String.substr_replace_all
             (List.hd_exn line)
             ~pattern:"."
             ~with_:"")
          ~pattern:" "
          ~with_:""
      in
      let tail = List.tl_exn line in
      tuples head ~cities:tail [])
  ;;

  let of_file input_file =
    let tuple =
      load_file input_file
      |> List.concat_map ~f:(fun (a, b, c) -> [ a, b, c ])
    in
    Connection.Set.of_list tuple
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
        (* This special syntax can be used to easily sexp-serialize values
           (whose types have [sexp_of_t] implemented). *)
        printf !"%{sexp: Network.t}\n" network]
;;

module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (City) (Highway)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
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
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (city1, city2, highway) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          let edge = G.E.create city1 highway city2 in
          G.add_edge_e graph edge);
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
