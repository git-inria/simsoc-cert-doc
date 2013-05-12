(**
SimSoC-Cert, a toolkit for generating certified processor simulators
See the COPYRIGHTS and LICENSE files.
*)

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
    val colorbox_ : color -> Latex.t -> Latex.t (* store color used, [definecolor_used] will fold it *)
    val color_name_ : color -> Latex.t (* store color used, [definecolor_used] will fold it *)
    val arrayrulecolor_ : color -> Latex.t (* store color used, [definecolor_used] will fold it *)
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
      let colorbox n x =  \"colorbox\" @ ([n ; x], A)
      let arrayrulecolor x = \"arrayrulecolor\" @ ([x], A)
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
    let colorbox (id, _) = L.colorbox (color_of_name id)
    let color_name (id, _) = color_of_name id
    let arrayrulecolor (id, _) = L.arrayrulecolor (color_of_name id)

    let definecolor_used, definecolor_reset, textcolor_, cellcolor_, color_, color_name_, colorbox_, arrayrulecolor_ =
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
        color_name c),
      (fun c ->
        let _ = col_map := ColorMap.add c () !col_map in
        colorbox c),
      (fun c ->
        let _ = col_map := ColorMap.add c () !col_map in
        arrayrulecolor c)


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

      let comment = of_int_255 (0x79, 0x2D, 0xB5)
      let alpha_keyword = of_int_255 (128, 128, 128)
      let nonalpha_keyword = black
      let string = of_int_255 (170, 68, 68)
      let construct = of_int_255 (0, 51, 204)
    end
    include C
  end

  module type TH =
  sig
    val newtheorem : ?opt:t -> t -> (t -> t) * t
    val newtheorem' : ?opt:t -> t -> (t -> t) * t
  end

  module Th : TH =
  struct
    open Printf

    type label = L of int

    let mk_new =
      let r = ref 0 in
      fun _ -> let _ = incr r in !r

    let string_of_name (L l) = sprintf \"latex_libth_label_%d\" l

    let environment ?packages name ?opt ?args body mode =
      environment ?packages (string_of_name name) ?opt ?args body mode

    let env name body = environment name (A, body) A

    let newtheorem_ star ?opt param =
      let lab = mk_new () in
      env (L lab),
      unusual_command
        (Printf.sprintf \"newtheorem%s\" (if star then \"*\" else \"\"))
        ((A, brace, text (string_of_name (L lab)))
         :: (A, brace, param)
         :: match opt with Some t -> [ A, bracket, t ] | None -> []) A

    let newtheorem = newtheorem_ false
    let newtheorem' = newtheorem_ true
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
    | Arrayrulecolor of Color.color
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
      (A, concat (BatList.map (function Data l -> concat (BatList.interleave (text \" & \") l) ^^ newline | Hline -> hline ^^ text \"\n\" | Cline (i1, i2) -> cline i1 i2 ^^ text \"\n\" | Arrayrulecolor c -> Color.arrayrulecolor_ c) body)) A

  let multiline l = tabular [`C] (BatList.map (fun x -> Data [x]) l)
  let multiline_ s = multiline (BatList.map text (BatString.nsplit s ~by:\"\n\"))

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
    let l_not = StringSet.of_enum (BatList.enum [ \"with\" ; \"the\" ; \"for\" ; \"from\" ; \"to\" ; \"through\" ; \"of\" ; \"a\" ]) in

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
  let st x = \"st\" @ ([x], A)

  let description l = itemize (BatList.map (fun (i, msg) -> textbf i ^^ newline ^^ msg) l)

  let forceline = newline

  let frame_fragile x =
    environment \"frame\" ~opt:(A, "fragile")
      (A, x) A
  let pause =  \"pause\" @ ([], A)

  let brack l = concat [ text \"{\" ; l ; text \"}\" ]
  let scope ~x ~y cts = environment \"scope\" ~opt:(A, "shift={brack "({latex_of_float x},{latex_of_float y})"}") (A, cts) A
  let tikzpicture ?shift opt cts = environment \"tikzpicture\" ~opt:(A, concat [ ">=latex,line width=1pt, join=rounded," ; opt ]) (A, match shift with None -> cts | Some (x, y) -> scope ~x ~y cts) A
  let usetikzlibrary x = \"usetikzlibrary\" @ ([x], A)

  let blacktriangleleft =  \"blacktriangleleft\" @ ([], M)
  let blacktriangleright =  \"blacktriangleright\" @ ([], M)

  let s_concat s =
    let rec aux = function
      | [] -> ""
      | [x] -> x
      | x :: xs -> x ^^ s ^^ (aux xs) in
    aux

  let titlegraphic x = \"titlegraphic\" @ ([x], A)
  let usebackgroundtemplate x = \"usebackgroundtemplate\" @ ([x], A)

  open Printf

  let includegraphics =
    let i o filename = command ~packages:[\"graphicx\", \"\"] ~opt:(A, o) \"includegraphics\" [ A, text filename ] T in
    let n = \"node\" @ ([], A) in
    fun ?trim ?page ?scale ~x ~y filename ->
      tikzpicture
        "overlay"
        (concat [ n ; "[shift={brack "({latex_of_float x},{latex_of_float y})"}] at (current page.south) {brack (i
          (s_concat "," (List.flatten [ (match trim with None -> [] | Some (i1,i2,i3,i4) -> [ "clip" ; "trim=" ^^ concat (BatList.map (fun s -> latex_of_size s ^^ " ") [i1 ; i2 ; i3 ; i4])])
                                      ; (match page with None -> [] | Some s -> [text (sprintf \"page=%d\" s)])
                                      ; (match scale with None -> [] | Some s -> [text (sprintf \"scale=%f\" s)]) ])) filename)};" ])

  let includetext txt =
    let n = \"node\" @ ([], A) in
    fun ~x ~y ->
      tikzpicture
        "overlay"
        (concat [ n ; "[shift={brack "({latex_of_float x},{latex_of_float y})"}] at (current page.south) {brack txt};" ])

  module B =
  struct
    type 'a frame =
    | Abr of 'a frame list
    | Center of 'a (* center *)
    | Top of 'a (* top *)
    | Bottom of 'a (* bottom *)
    | Allowframebreaks of 'a (* center by default *)

    let optcmd name = function
      | Some arg -> command name [T, arg] T
      | None -> empty

    let frame ?title ?subtitle ~opt body =
      let x = concat [
        optcmd \"frametitle\" title;
        optcmd \"framesubtitle\" subtitle;
        body;
      ] in
      environment ~opt \"frame\" (T, x) T

    let rec mk_frame = function
      | Center (title, body) -> [Beamer.frame ~title body]
      | Top (title, body) -> [frame ~opt:(A, "t") ~title body]
      | Bottom (title, body) -> [frame ~opt:(A, "b") ~title body]
      | Allowframebreaks (title, body) -> [frame ~opt:(A, "allowframebreaks") ~title body]
      | Abr l -> BatList.flatten (BatList.map mk_frame l)

    let setbeamertemplate template ~l_bracket ~l_brace =
      unusual_command \"setbeamertemplate\" (BatList.flatten [ [ T, brace, text template ]
                                                             ; BatList.map (fun b -> T, bracket, b) l_bracket
                                                             ; BatList.map (fun b -> T, brace, b) l_brace ]) T
    let usecolortheme x = \"usecolortheme\" @ ([x], A)
    let usetheme x = \"usetheme\" @ ([x], A)
    let useoutertheme o x = command \"useoutertheme\" ~opt:(A, o) [A, x] A
    let useinnertheme o x = command \"useinnertheme\" ~opt:(A, o) [A, x] A
    let setbeamersize x = \"setbeamersize\" @ ([x], A)
    let setbeamercolor l_brace =
      unusual_command \"setbeamercolor\" (BatList.map (fun b -> T, brace, b) l_brace) T
    let addtobeamertemplate l_brace =
      unusual_command \"addtobeamertemplate\" (BatList.map (fun b -> T, brace, b) l_brace) T
    let setbeamerfont l_brace =
      unusual_command \"setbeamerfont\" (BatList.map (fun b -> T, brace, b) l_brace) T

(*    let stepcounter *)


    let texpic = picture_of_latex

    let texbox ?dx ?dy ?name ?brush ?stroke ?pen ?dash ?fill ?style x =
      Mlpost.Box.pic (*?dx ?dy ?name ?brush ?stroke ?pen ?dash ?fill*) ?style (texpic x)

    let lines_box l =
      let l = List.map texbox l in
      Mlpost.Box.vbox l

    let blue_tit = `RGB (0., 0., 0.7)

    let list_env f_esc l name =
      if l = [] then failwith \"itemize or enumerate: no item given\";
      let items = List.map (fun (o, x) -> (^^) (text (sprintf \"\\item%s \" (match o with None -> \"\" | Some s -> f_esc s))) x) l in
      let body = concat (list_insert (text \"\n\") items) in
      environment name (T, body) T

    let itemize l = list_env (sprintf \"<%s>\") l \"itemize\"
    let itemize_ l = list_env (fun s -> sprintf \"[%s]\" (Latex.to_string s)) l \"itemize\"
    let enumerate l = list_env (sprintf \"<%s>\") l \"enumerate\"

    type 'a index = Pos of 'a | From_to of 'a * 'a

    let onslide s = unusual_command (\"onslide<\") [A, nobr, text (s ^ \">\")] A
    let on_slide l s = onslide (sprintf \"%s\" (String.concat \",\" (BatList.map (function Pos x -> sprintf \"%d\" x | From_to (mi, ma) -> sprintf \"%d-%d\" mi ma) l))) ^^ s
    let on_after n s = onslide (sprintf \"%d-\" n) ^^ s
    let on_before n s = onslide (sprintf \"-%d\" n) ^^ s

    let _, section =
      (* produce a table of contents page, highlighting section number
         [index] (starting from 1) if specified *)
      let toc ?index all_sections =
        concat
          (List.fold_left
             (fun x f -> f x)
             all_sections
             [ BatList.map2
                 Beamer.color
                 ((* Colorize titles. *)
                   match index with
                   | None -> BatList.map (fun _ -> blue_tit) all_sections
                   | Some index ->
                     BatList.mapi
                       (fun i title -> if Pervasives.succ i = index then blue_tit else `Gray)
                       all_sections)
             ; BatList.map large2 (* make them bigger *)
             ; list_insert "{par}{bigskip}" (* insert some space *) ]) in

      let sections = variable [] in (* accumulate the list of section titles *)

      (* toc_slide *)
      (fun () -> Center ("", final sections toc)),

      (* section *)
      (fun title ->
        Center
          (setf sections (fun sections -> List.append sections [ title ]),
           get sections (fun now -> final sections (toc ~index:(List.length now)))))

    let grey = Beamer.color (let i = 0.3 in `RGB (i, i, i))
    let red = Beamer.color `Red
    let blue = Beamer.color `Blue
    let emph = red
    let white = Beamer.color (let i = 1. in `RGB (i, i, i))
  end

  type document =
  | Beamer of (size (* left *) * size (* right *)) option *
              Latex.t list (* prelude *) *
              (Latex.t (* title *) * Latex.t (* body *)) B.frame
  | PaperA4 of Latex.t list

  let latex_init = function
    | Beamer (page_width, prelude, l) ->
      B.mk_frame l,
      [],
      `Beamer,
      BatList.flatten
      [ (Beamer.setbeamertemplate `NavigationSymbols ""
    (*:: B.setbeamertemplate \"blocks\" "rounded"*)
    (*:: B.usecolortheme "fly"*)
    (*:: B.usetheme "Boadilla"
      :: B.setbeamertemplate \"Footline\" "default"*)
(*    :: B.useoutertheme "" "default"
      :: text \"\\defbeamertemplate*{footline}{default}
{}\"*)
      :: B.usecolortheme "rose"
      :: B.useinnertheme "shadow" "rounded"
      :: match page_width with
         | None -> []
         | Some (left, right) -> [ B.setbeamersize ("text margin left=" ^^ latex_of_size left ^^ ",text margin right=" ^^ latex_of_size right)
                                 ; usetikzlibrary "arrows" ])
      ; prelude ]
    | PaperA4 l -> l, [ `A4paper ; `Pt 11 ], `Article, []

  module Label =
  struct
    let example, newth_example = Th.newtheorem "Example" ~opt:"subsection"
    let example_, newth_example_ = Th.newtheorem' "Example"
    let notation_, newth_notation_ = Th.newtheorem' "Notation"
    let fact, newth_fact = Th.newtheorem "Fact"
    let definition_, newth_definition_ = Th.newtheorem' "Definition"
    let remark_, newth_remark_ = Th.newtheorem' "Remark"
    let warning_, newth_warning_ = Th.newtheorem' "Warning"
    let fix_, newth_fix_ = Th.newtheorem' "Fix"
    let question_, newth_question_ = Th.newtheorem' "Question"
    let problem_, newth_problem_ = Th.newtheorem' "Problem"
    let conjecture_, newth_conjecture_ = Th.newtheorem' "Conjecture"

    let prelude = [ newth_example ; newth_example_ ; newth_notation_ ; newth_fact ; newth_definition_ ; newth_remark_ ; newth_warning_ ; newth_fix_ ; newth_question_ ; newth_problem_ ; newth_conjecture_ ]
  end

  let latex_main ~packages ?author ?title ?date ?titlegraphic_content ?usebackgroundtemplate_content l =

    let l_hypersetup = (* WARNING side effect, if delta reduced *)
      BatList.flatten
        [ [ "colorlinks", "true" ]
        ; BatList.map
          (fun (n, r, g, b) -> n, Color.color_name_ (Color.of_int_255 (r, g, b)))
          [ "linkcolor", (*137, 59, 187*)144, 0, 24
          ; "citecolor", 0, 163, 100
          ; "urlcolor", 0, 60, 141 ] ] in

    let l, options, documentclass, prelude2 = latex_init l in

    emit
      (document
         ~options
         ~documentclass
         ~packages
         ?author
         ?title:(BatOption.map (fun title -> large3 (textbf (concat_line_string (upper_important title)))) title)
         ?date
         ~prelude:(concat (BatList.flatten
                             [ Label.prelude
                             ; [ Color.definecolor_used (fun c l -> l ^^ Color.definecolor c) ""
                               ; hypersetup l_hypersetup ]
                             ; prelude2
                             ; (match titlegraphic_content with None -> [] | Some c -> [titlegraphic c])
                             ; match usebackgroundtemplate_content with None -> [] | Some c -> [usebackgroundtemplate c] ]))
         (concat l))

(* \ifhevea\setboolean{footer}{false}\fi *)

(*  \newenvironment{fontsans} *)
(*    {\begin{divstyle}{fontsans}} *)
(*    {\end{divstyle}} *)

end
