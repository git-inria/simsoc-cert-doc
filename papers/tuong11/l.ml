module BatList =
struct
  include BatList

  let nsplit f =
    let rec aux acc (f1, f2) = function
      | [] -> List.rev acc
      | l -> aux (take_while f1 l :: acc) (f2, f1) (drop_while f1 l) in
    aux [] (f, (fun x -> not (f x)))
end

module L = (* latex missing functions *)
struct
  let (@) ?packages name ?opt (x, mode) = command ?packages name ?opt (BatList.map (fun x -> mode, x) x) mode

  module type COLOR =
  sig
    type color
    type 'a tup3

    val of_float_1 : float tup3 -> color
    val of_int_255 : int tup3 -> color

    val definecolor : color -> Latex.t
    val definecolor_used : (color -> Latex.t -> Latex.t) -> Latex.t -> Latex.t (* fold colors stored by [textcolor_] *)
    val definecolor_reset : unit -> unit (* empties the cache, behaves as if [textcolor_] has never been called *)
    val textcolor_ : color -> Latex.t -> Latex.t (* store color used, [definecolor_used] will fold it *)
(*    val textcolor : color -> Latex.t -> Latex.t (* nothing is stored *)*)
    val cellcolor_ : color -> Latex.t (* store color used, [definecolor_used] will fold it *)
    val color_ : color -> Latex.t (* store color used, [definecolor_used] will fold it *)
    val color_name_ : color -> Latex.t (* store color used, [definecolor_used] will fold it *)
(*    val color_compare : color -> color -> int*)

    val comment : color
    val alpha_keyword : color
    val nonalpha_keyword : color
    val string : color
    val construct : color
    val black : color
    val green_light : color
    val green : color
    val grey : color
    val blue : color
    val yellow : color
    val violet : color
    val red_light : color
    val red : color
  end

  module Color : COLOR with type 'a tup3 = 'a * 'a * 'a =
  struct
    type 'a tup3 = 'a * 'a * 'a
    type raw_color =
      | N_255 of int tup3
      | N_1 of float tup3

    module Float3Map = BatMap.Make (struct type t = float tup3  let compare = compare end)

    let n1_of_n255 (r, g, b) =
      let f x = float x /. 255. in
      f r, f g, f b

    let raw_color = function
      | N_1 (r, g, b) -> r, g, b
      | N_255 (r, g, b) -> n1_of_n255 (r, g, b)

    let raw_color_compare a b = compare (raw_color a) (raw_color b)

    let get_color_id, fold =
      let refe = ref (Float3Map.empty, 0) in
      (fun col ->
        let map, dim_map = !refe in
        let o, i =
          let col = raw_color col in
          match Float3Map.Exceptionless.find col map with
            | None ->
              Some (Float3Map.add col dim_map map, Pervasives.succ dim_map), dim_map
            | Some i -> None, i in
        let _ = match o with None -> () | Some r -> refe := r in
        i, col),
      fun f -> let map, _ = !refe in Float3Map.fold f map

    type color = int (* id *) * raw_color
    let color_compare (a, _) (b, _) = compare a b
    module ColorMap = BatMap.Make (struct type t = color let compare = color_compare end)

    let of_float_1 (r, g, b) = get_color_id (N_1 (r, g, b))
    let of_int_255 (r, g, b) = get_color_id (N_255 (r, g, b))

    module L =
    struct
      let definecolor name rgb x = \"definecolor\" @ ([ name ; rgb ; x ], A)
      let textcolor n x = \"textcolor\" @ ([ n ; x ], A)
      let cellcolor x = \"cellcolor\" @ ([x], A)
      let color x =  \"color\" @ ([x], A)
    end
    let color_of_name x = text (Printf.sprintf \"color%d\" x)

    let definecolor (id, col) =
      let msg, colo =
        match col with
          | N_1 (r, g, b) -> "rgb", text (Printf.sprintf \"%f, %f, %f\" r g b)
          | N_255 (r, g, b) -> "RGB", text (Printf.sprintf \"%d, %d, %d\" r g b) in
      L.definecolor (color_of_name id) msg colo

    let textcolor (id, _) = L.textcolor (color_of_name id)
    let cellcolor (id, _) = L.cellcolor (color_of_name id)
    let color (id, _) = L.color (color_of_name id)
    let color_name (id, _) = color_of_name id

    let definecolor_used, definecolor_reset, textcolor_, cellcolor_, color_, color_name_ =
      let col_map = ref ColorMap.empty in
      (fun f -> ColorMap.fold (fun k _ -> f k) !col_map),
      (fun _ -> col_map := ColorMap.empty),
      (fun c ->
        let _ = col_map := ColorMap.add c () !col_map in
        textcolor c),
      (fun c ->
        let _ = col_map := ColorMap.add c () !col_map in
        cellcolor c),
      (fun c ->
        let _ = col_map := ColorMap.add c () !col_map in
        color c),
      (fun c ->
        let _ = col_map := ColorMap.add c () !col_map in
        color_name c)

    let black = of_int_255 (let i = 0 in i, i, i)
    let green_light = of_int_255 (216, 255, 241)
    let green = of_int_255 (60, 179, 113)
    let grey = of_int_255 (*(0x24, 0xD3, 0xD5)*) (*(0, 139, 141)*) (*(0, 118, 39)*) (let i = 156 in i, i, i)
    let blue = of_int_255 (0, 5, 105)
    let yellow = of_int_255 (178, 121, 53)
    let violet = of_int_255 (206, 100, 255)
    let red_light = of_int_255 (255, 216, 224)
    let red = of_int_255 (200, 72, 0)
    module C =
    struct
      open Caml2html.Output_latex

      let comment = of_int_255 (153, 0, 0)
      let alpha_keyword = of_int_255 (128, 128, 128)
      let nonalpha_keyword = black
      let string = of_int_255 (170, 68, 68)
      let construct = of_int_255 (0, 51, 204)
    end
    include C
  end

  module type TH =
  sig
    type label
    val newtheorem : ?opt:t -> t -> label * t
    val newtheorem' : ?opt:t -> t -> label * t
    val environment : ?packages:(string * string) list -> label -> ?opt:mode * t -> ?args:(mode * t) list -> mode * t -> mode -> t
    val env : label -> t -> t (* shortcut *)
  end

  module Th : TH =
  struct
    open Printf

    type label = L of int

    let mk_new =
      let r = ref 0 in
      fun _ -> let _ = incr r in !r

    let string_of_name (L l) = sprintf \"latex_libth_label_%d\" l

    let newtheorem_ star ?opt param =
      let lab = mk_new () in
      L lab,
      unusual_command
        (Printf.sprintf \"newtheorem%s\" (if star then \"*\" else \"\"))
        ((A, brace, text (string_of_name (L lab)))
         :: (A, brace, param)
         :: match opt with Some t -> [ A, bracket, t ] | None -> []) A

    let newtheorem = newtheorem_ false
    let newtheorem' = newtheorem_ true

    let environment ?packages name ?opt ?args body mode =
      environment ?packages (string_of_name name) ?opt ?args body mode

    let env name body = environment name (A, body) A
  end

  let cite refe = \"cite\" @ ([concat (list_insert "," refe)], A)

  let url x = \"url\" @ ([text x], A)
  let nolinkurl x = \"nolinkurl\" @ ([text x], A)

  type 'a mail =
    | Mail of 'a
    | Http of 'a
    | Https of 'a

  let href x1 x2 = \"href\" @ ([text (match x1 with Mail x -> \"mailto:\" ^ x | Http x -> \"http://\" ^ x | Https x -> \"https://\" ^ x) ; nolinkurl x2], A)

  let http s = href (Http s) s
  let https s = href (Https s) s
  let mail s = href (Mail s) s

  let multirow2 x = \"multirow\" @ ([ "2" ; "*" ; x ], A)
  let multicolumn n align x = \"multicolumn\" @ ([ latex_of_int n ; align ; x ], A)

  let latex_of_array_column = function
    | `L -> text \"l\"
    | `C -> text \"c\"
    | `R -> text \"r\"
    | `Vert -> text \"|\"
    | `Raw s -> text \"!\" ^^ within_braces s
    | `P s -> concat [ "p" ; text \"{\" ; latex_of_size s ; text \"}\" ]
    | `Sep t -> concat [ text \"@{\" ; t ; text \"}\"]

  type 'a row_line =
    | Data of 'a
    | Hline
    | Cline of int * int

  let title f_sz l_no o_lg (* should be greater than -> *) l_yes =
    match
      List.fold_left
        (fun (acc, nb_blank, pos) x ->
          List.fold_left
            (fun acc x ->
              let x = f_sz x in
              Data (BatList.flatten
                      [ BatList.init nb_blank (fun _ -> multicolumn 1 "l|" "")
                      ; [ if pos = 1 then x else multicolumn pos "l" x ] ]) :: acc) acc x,
          Pervasives.succ nb_blank,
          pred pos)
        ([], 0, match o_lg with None -> List.length l_yes | Some nb -> nb)
        l_yes
    with
      | Data l :: xs, _, _ ->
        List.rev
          (Data (BatList.flatten [ l_no ; l ])
           ::
           BatList.map (let l_no = BatList.map (fun _ -> "") l_no in
                        fun (Data l) -> Data (BatList.flatten [ l_no ; l ])) xs)
      | [], _, _ -> []

  let hline = \"hline\" @ ([], A)

  let cline i1 i2 = \"cline\" @ ([text (Printf.sprintf \"%d-%d\" i1 i2)], A)

  let tabular x body =
    environment \"tabular\" ~args:[A, concat (BatList.map latex_of_array_column x)]
      (A, concat (BatList.map (function Data l -> concat (BatList.interleave (text \" & \") l) ^^ newline | Hline -> hline ^^ text \"\n\" | Cline (i1, i2) -> cline i1 i2 ^^ text \"\n\") body)) A

  let bibliographystyle x = \"bibliographystyle\" @ ([x], A)
  let bibliography x = \"bibliography\" @ ([x], A)
  let subparagraph x = \"subparagraph\" @ ([x], A)
  let footnote () = footnote (* we change the type of Latex.footnote to remember that it has the side effect of resizing fonts *)
  let alltt x = environment \"alltt\" (A, x) A
  let abstract x = environment \"abstract\" (A, x) A
  let mbox x =  \"mbox\" @ ([x], A)
  let fbox x =  \"fbox\" @ ([x], A)
  let spacing x =
    environment \"spacing\" ~args:[A, "0.80"]
      (A, x) A
  let longtable x body =
    space ^^ (* WARNING [longtable] can have a bad behaviour if we delete this [space] *)
    environment \"longtable\" ~opt:(A, "l") ~args:[A, concat (BatList.map latex_of_array_column x)]
      (A, concat (BatList.map (function Data l -> concat (BatList.interleave (text \" & \") l) ^^ newline | Hline -> hline ^^ text \"\n\" | Cline (i1, i2) -> cline i1 i2 ^^ text \"\n\") body)) A
  (*let vdots = \"vdots\" @ ([], A)*)
  let textcircled x = \"textcircled\" @ ([x], A)
  let ding x = \"ding\" @ ([text (string_of_int x)], A)

  let upper_important =
    let open BatSet in
    let module StringSet = BatSet.Make (String) in
    let l_not = StringSet.of_enum (BatList.enum [ \"with\" ; \"the\" ]) in

    BatList.map (fun s ->
      let esp = \" \" in
      BatIO.to_string (BatList.print ~first:\"\" ~last:\"\" ~sep:esp BatString.print)
        (BatList.map
           (function
             | s when s = \"\" || StringSet.mem s l_not -> s
             | s -> BatString.splice s 0 1 (String.make 1 (Char.uppercase s.[0]))) (BatString.nsplit s esp)))

  let concat_line_t l =
    concat (BatList.interleave newline l)

  let concat_line_string l = concat_line_t (BatList.map (fun s -> text s) l)
  let vrule =  \"vrule\" @ ([], A)
(*
  let fontdimen n = text (Printf.sprintf \"\\fontdimen%d\" n) (*(Printf.sprintf \"fontdimen%d\" n) @ ([], A)*)
  let font = \"font\" @ ([], A)
  let font_ = text \"\\font\"
  let hyphenchar = \"font\" @ ([], A)
  let fontdimen_font n sz = fontdimen n ^^ font_ ^^ text \"=\" ^^ (latex_of_size sz) ^^ text \"\n\"
  let hyphenchar_font_ = text \"\\hyphenchar\\font=`\\-\"  ^^ text \"\n\"

  let justify x = text \"\\justify \" ^^ x
  let justify2 x = text \"\\justify \" ^^ x
(*
  let justify x =
    List.fold_left (fun acc (n, sz) -> acc ^^ fontdimen_font n sz) ""
    [ 2, `Em 0.4
    ; 3, `Em 0.2
    ; 4, `Em 0.1
    ; 7, `Em 0.1 ]
    ^^ x

  let justify2 x =
    List.fold_left (fun acc (n, sz) -> acc ^^ fontdimen_font n sz) ""
    [ 2, `Em 0.4
    ; 3, `Em 0.2
    ; 4, `Em 0.1
    ; 7, `Em 0.1 ]
    ^^
      hyphenchar_font_
    ^^ x
*)
*)

  let hypersetup l =  \"hypersetup\" @ ([ concat (BatList.interleave "," (BatList.map (fun (s1, s2) -> s1 ^^ "=" ^^ s2) l)) ], A)
  let infty =  \"infty\" @ ([], M)
  let textvisiblespace = \"textvisiblespace\" @ ([], A)
  let sqcup =  \"sqcup\" @ ([], A)
  let curvearrowright = \"curvearrowright\" @ ([], A)
  let overset x y = \"overset\" @ ([x ; y], A)
  let rightsquigarrow = \"rightsquigarrow\" @ ([], A)
  let rightarrowtriangle = \"rightarrowtriangle\" @ ([], A)

  let description l = itemize (BatList.map (fun (i, msg) -> textbf i ^^ newline ^^ msg) l)

  let forceline = newline
end
