open Base
open Utils

module RangeMap = struct
    type 'a t = ((int * int) * 'a) array

    let find (m: 'a t) (i: int) : 'a option =
        Array.find_map ~f:(fun ((l,h),v) -> if l <= i && i <= h then Some v else None) m

    let find_with_range (m: 'a t) (i: int) : ((int * int) * 'a) option =
        let f ((l,h),v) =
            if l <= i && i <= h then Some ((l,h),v) else None
        in
        Array.find_map ~f:f m

    let of_list x =
        Array.of_list x
end

module IntRangeMap = struct
    open RangeMap

    type t = int RangeMap.t

    let parse_line (s: string) : ((int * int) * int) =
        match String.split ~on:' ' s with
        | [dest;source;len] -> (
            let source' = Int.of_string source in
            let dest' = Int.of_string dest in
            let len' = Int.of_string len in
            (source', source'+(len'-1)), dest')
        | _ -> failwith "failed to split line"

    let parse_lines (s: string list) : t  =
        List.map ~f:parse_line s
        |> RangeMap.of_list

    let get_val (m: t) (i: int) : int =
        match find_with_range m i with
        | None -> i
        | Some ((l,_),v) ->
                let dist = i - l in
                v + dist
end

module MapsCollection = struct
    type t = {
        seed_to_soil: IntRangeMap.t;
        soil_to_fertilizer: IntRangeMap.t;
        fertilizer_to_water: IntRangeMap.t;
        water_to_light: IntRangeMap.t;
        light_to_temperature: IntRangeMap.t;
        temperature_to_humidity: IntRangeMap.t;
        humidity_to_location: IntRangeMap.t;
    }

    let seed_to_location (m: t) (seed: int) : int =
        let {seed_to_soil;
             soil_to_fertilizer;
             fertilizer_to_water;
             water_to_light;
             light_to_temperature;
             temperature_to_humidity;
             humidity_to_location} = m in
        IntRangeMap.get_val seed_to_soil seed
            |> IntRangeMap.get_val soil_to_fertilizer
            |> IntRangeMap.get_val fertilizer_to_water
            |> IntRangeMap.get_val water_to_light
            |> IntRangeMap.get_val light_to_temperature
            |> IntRangeMap.get_val temperature_to_humidity
            |> IntRangeMap.get_val humidity_to_location
end

module MapsArray = struct
    type t = IntRangeMap.t array

    let seed_to_location (m: t) (seed: int) : int =
        let f acc m =
            let v = IntRangeMap.get_val m acc in
            v
        in
        Array.fold ~init:seed ~f:f m

    let of_list l =
        Array.of_list l
end

let parse_seeds_line s : int list =
    String.substr_replace_first s ~pattern:"seeds: " ~with_:""
        |> String.split ~on:' '
        |> List.map ~f:Int.of_string

let parse_maps s : MapsArray.t =
    let current_lines = ref [] in
    let maps = ref [] in
    let make_new_map () =
        match !current_lines with
        | [] -> () (* do nothing, at beginning *)
        | _ -> begin
            let new_map = IntRangeMap.parse_lines !current_lines in
            current_lines := [];
            maps := new_map::!maps
        end
    in
    let f line =
        let first_char = line.[0] in
        if Char.is_digit first_char
        then current_lines := line::!current_lines
        else make_new_map ()
    in
    List.iter ~f:f s;
    make_new_map ();
    MapsArray.of_list (List.rev !maps)

let parse_input (s:string) : (int list * MapsArray.t) =
    match String.split_lines s with
    | seeds_line::maps -> 
            let seeds = parse_seeds_line seeds_line in
            let maps' = List.filter ~f:(fun x -> String.length x > 0) maps in
            let maps'' = parse_maps maps' in
            (seeds, maps'')
    | _ -> failwith "shouldn't happen"

let part1 (s:string) =
    let (seeds, maps) = parse_input s in
    List.map ~f:(fun seed -> MapsArray.seed_to_location maps seed) seeds
    |> ListUtils.min_by_exn ~key_fn:(fun x -> x) 

let group2 (l: 'a list) : ('a * 'a) list =
    let rec helper l acc = 
        match l with
        | a::b::rest ->
                let acc' = (a,b)::acc in
                helper rest acc'
        | [] -> List.rev acc
        | _ -> failwith "uneven number of elements"
    in
    helper l []

let get_location_for_range m (start,len) : int list =
    start @.. start+(len-1)
    |> List.map ~f:(MapsArray.seed_to_location m)

let get_min_for_range m x : int =
    get_location_for_range m x
    |> ListUtils.min_by_exn ~key_fn:(fun x -> x) 


let part2 (s:string) =
    let (seeds, maps)  = parse_input s in
    let seeds' = group2 seeds in
    List.map ~f:(get_min_for_range maps) seeds'
    |> ListUtils.min_by_exn ~key_fn:(fun x -> x) 
