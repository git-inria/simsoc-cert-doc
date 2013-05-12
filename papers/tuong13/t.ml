(**
SimSoC-Cert, a toolkit for generating certified processor simulators
See the COPYRIGHTS and LICENSE files.
*)

open Melt_lib open L
open Melt_highlight
open Simsoc_cert

##verbatim '?' = Code.Raw_.verbatim
##verbatim '!' = Code.Raw.verbatim
##verbatim '#' = Code.Coq.verbatim
##verbatim 'Z' = Code.Coq.verbatim
##verbatim 'U' = Code.Coq.verbatim
##verbatim '~' = Code.Ml.verbatim
##verbatim '@' = Code.Humc_.verbatim
##verbatim ':' = Code.Humc_.verbatim
##verbatim 'O' = Code.Dot.verbatim
##verbatim 'X' = Code.Raw__.verbatim

open Printf

module Argument = struct
  let dir_init = Sys.argv.(1)

  let dir_img = sprintf \"%s/doc/img/%d.png\" dir_init
  let file_arm6 = sprintf \"%s/../../simsoc-cert-cc1.9/arm6/arm6.pdf\" dir_init
  let page_middle = sprintf \"%s/doc/img/page_middle.png\" dir_init

  let img1 = dir_img 1
  let img2 = dir_img 2
  let img3 = dir_img 3
  let img4 = dir_img 4
end

module Label_small = struct
  let warning s = texttt "Warning:" ^^ space ^^ s
  let fix s = texttt "Fix:" ^^ space ^^ s
end

let red_ = Color.of_int_255 (0x9F, 0x00, 0x00)
let red = Color.textcolor_ red_
let orange = Color.of_int_255 (0xFF, 0xED, 0xDF)
let blue = Color.textcolor_ Color.blue

let sh4_intro = "In the generated {S.Manual.Sh.C.gcc} ({Version.Size.manual_sh4}K), several lines similar as:"
let sl_intro x = "In {x} ({Version.Cpp11_bis.simlight2_no_man_size}K without manual), several lines similar as:"
let comp_intro = "In {S.C.compcert} AST ({Version.Size.compcert} token words), several lines similar as:"

let index_ s x = index s (footnotesize x)

let draw_subset l_space colo =
  let rec aux n l = function
    | [] -> l
    | (hline, vert, id, title) :: xs ->
      aux
        (Pervasives.succ n)
        (minipage (`Em (float_of_int (7 * (Pervasives.succ n))))
           (tabular (`R :: vert)
              (hline
                 (BatList.map
                    (fun x -> Data [Color.cellcolor_ (colo (id n)) ^^ x])
                    (title :: l :: l_space)) )))
        xs in
  aux 0 ""

let degrad n = Color.of_int_255 (50*n, 0xFF, 0xFF)
let space_nb =
  let rec aux acc n =
    if n = 0 then
      acc
    else
      aux (space ^^ acc) (pred n) in
  aux ""

let () =
  main
    ~packages:[]
    ~author:[ footnotesize (mail \"tuong@users.gforge.inria.fr\") ]

    (Beamer (Some (`Em 1., `Em 1.), [], B.Abr
[
(* ********************************************************* *)
  B.Center ("{P.simsoc}, simulator of Systems-on-Chip",
            itemize
              [ "A library for fast bit accurate simulation:
                 {itemize
                   [ "many processors simulated: ARM (> 10 000 LoC), PowerPC, MIPS (with GDB servers)"
                   ; "others components ({approx} 5 000 LoC): Memory, UART, Ethernet card, Interrupt controller, Serial Flash, Timers, Bus..."
                   ] }"
              ; "Processors are the most complex components to simulate: optimizations are essential for high speed of simulation."
              ; "Project developped in C++ and SystemC/TLM~{cite ["ossc09"]}
                 {https \"gforge.inria.fr/projects/simsoc\"}"
              ; "Application to the ARMv6 instruction set simulator: this piece of code correctly compiles with GCC.
{Label.notation_ "``{red S.C.gcc}'': programs compiled successfully with {Version.gcc}~{cite ["gcc"]}"}"
])

(* ********************************************************* *)
; B.Center ("A lot of repetitive information in reference manuals",
            let mk_tab l_title l_sh l_arm =
              tabular (`L :: `Sep "" ::  `L :: `Sep space :: l_title)
                [ Data ("SH4" :: ":" :: l_sh)
                ; Data ("ARMv6" :: ":" :: l_arm) ] in
            itemize
              [ mk_tab [`R;`Sep space;`L] ["449 pages in total in the PDF" ; cite ["sh4refman"]] ["1138 pages in total in the PDF" ; cite ["arm6refman"]]
              ; mk_tab [`R;`Sep space;`R;`Sep space;`R] [phantom "1" ^^ "300 pages describing 205 instructions:";"2/3";"of the PDF"] [phantom "1" ^^ "600 pages describing 220 instructions:";"50%";"of the PDF"]
              ; "Both are 32 bits RISC"
              ; "No addressing mode for SH4, 5 for ARMv6"
              ; "In each PDF, the formatting style of each instruction looks identical."
              ; "To take advantage of the repetition: the automated generation.~{cite ["arm"]}" ])

(* ********************************************************* *)
; B.Abr (BatList.map
           (fun (page, y, trim_top) ->
             B.Center
               ("Example: one page in the ARMv6 manual (p. " ^^ latex_of_int page ^^ ")",
                includegraphics ~x:(-1.) ~y ~trim:(`Mm 0., `Mm 0., `Mm 0., `Mm trim_top) ~page ~scale:0.9 Argument.file_arm6))
           [ 158, -5.5, 25.
           ; 159, -6., 20. ])

(* ********************************************************* *)
; B.Center ("{P.simcert}, code generation from the manual",
            let open Code.Dot in
            let deepskyblue = Color.of_int_255 (0x00, 0xBF, 0xFF) in
            let module S = S_sz (struct let normal = normalsize let footnote = footnotesize let tiny = tiny let color_keyword = Some deepskyblue end) in
            let dodgerblue = Color.color_name_ (Color.of_int_255 (0x1E, 0x90, 0xFF)) in
            let floralwhite = B "deepskyblue" in
            let style_back = B "[style=\"angle 45-\"]" in
"<OO{ Header { shift_x = 0.
             ; shift_y = -8.3
             ; scale = Some 0.6
             ; node = { n_color = \"darksalmon\" ; shape = Note }
             ; edge = { e_color = Some dodgerblue ; style = Some \"-angle 45\" } } }O

  arm_pdf [texlbl="ARMv6.pdf"]
  arm_txt [texlbl="ARMv6.txt"]

  patch1 [shape=none, texlbl="patch \& extract"]
  patch2 [shape=none, texlbl="patch \& extract"]
  patch3 [shape=none, texlbl="patch \& extract"]

  subgraph cluster_ocaml {
    style="dashed, rounded"
    color=O{floralwhite}O
    ast_deco [texlbl="IS encoding"]
    ast_asm [texlbl="ASM syntax"]
    ast_pseudo [texlbl="pseudo-code"]

    intern_merge [shape=none, texlbl="merge \& preprocess"]
    intern_ocaml [texlbl="internal representation of the AST in O{textcolor_ deepskyblue P.ocaml}O"]

    out_coq_ [shape=none, texlbl="O{multiline_ \"monadic higher order\nfactoring\"}O"]
    out_simlight_ [shape=none, texlbl="O{multiline_ \"normalization, flattening\nand specialization\"}O"]
  }

  out_coq [style=filled, fillcolor=papayawhip, texlbl="shallow embedding to O{textcolor_ deepskyblue "Coq"}O"]
  out_coq_to [shape=none, texlbl="copy"]
  out_simlight [style=filled, fillcolor=papayawhip, texlbl="fast ISS (O{textcolor_ deepskyblue S.C.gcc}O/C++)"]
  out_simlight_to [shape=none, texlbl="copy"]

  subgraph cluster_simsoc {
    style="dashed, rounded"
    color=O{floralwhite}O
    cluster_simsoc_title [shape=none, texlbl="O{multiline_ \"SimSoC\n(C++/SystemC)\"}O"]
    subgraph cluster_simsoc_arm {
      style=dotted
      cluster_simsoc_arm_title [shape=none, texlbl="O{multiline ["ARMv6" ; S.SL.C.gcc ]}O"]
      simsoc_mmu [texlbl="MMU"]
      simsoc_iss [style=filled, fillcolor=papayawhip, texlbl="fast ISS (O{textcolor_ deepskyblue S.C.gcc}O/C++)"]
    }
    simsoc_peri [texlbl="O{multiline_ \"memory\nand peripherals\"}O"]
  }

  subgraph cluster_simcoq {
    style="dashed, rounded"
    color=O{floralwhite}O
    cluster_simcoq_title [shape=none, texlbl="O{multiline [ "ARMv6" ; S.SL.coq ]}O"]
    simcoq_iss [style=filled, fillcolor=papayawhip, texlbl="O{multiline [ "shallow" ; "embedding" ; "to {Color.textcolor_ deepskyblue "Coq"}" ]}O"]
  }

  /* */
  cluster_simsoc_title -> cluster_simsoc_arm_title -> simsoc_iss -> simsoc_mmu -> simsoc_peri -> out_simlight_to [style=invis]
  cluster_simcoq_title -> simcoq_iss [style=invis]

  /* */
  arm_pdf -> arm_txt [label="pdftotext", constraint=false]
  arm_txt -> patch1 -> ast_deco
  arm_txt -> patch2 -> ast_asm
  arm_txt -> patch3 -> ast_pseudo

  ast_deco -> intern_merge
  ast_asm -> intern_merge
  ast_pseudo -> intern_merge

  intern_merge -> intern_ocaml

  intern_ocaml -> out_coq_ -> out_coq
  intern_ocaml -> out_simlight_ -> out_simlight

  simcoq_iss -> out_coq_to O{style_back}O
  out_coq_to -> out_coq O{style_back}O

  simsoc_iss -> out_simlight_to O{style_back}O
  out_simlight_to -> out_simlight O{style_back}O

O>")

(* ********************************************************* *)
; B.Center ("{P.simcert}, designing a framework for proofs",
            Label.problem_
              (itemize [ "How to be sure that the semantic is preserved during the compilation of {S.SL.C.gcc}?"
                       ; "How to represent {S.SL.C.gcc} in {P.coq}~{cite ["Coq:manual"]} in order to prove its correction with {S.SL.coq}?" ]))

(* ********************************************************* *)
; B.Abr (let open Code.Dot in
         let edge_to_normal = \"-triangle 45\" in
         BatList.map
           (fun (fct_edge, darksalmon, draw_red, attr_error) ->
             B.Bottom
               ("CompCert, semantic preservation proved in {P.coq}",
                minipage (`Cm 3.5)
                  (Label.notation_ (itemize [ "``{red S.C.compcert}'': first AST defined in Coq"
                                            ; "``{red S.C.asm}'': last AST defined in Coq"
                                            ]))
                ^^
                let edge_to_fct dir = B (concat [ "[" ; text (sprintf \"style=%S,\" (match dir with `normal -> \"-stealth\" | `back -> \"stealth-\")) ; "color={darksalmon}]" ]) in
                let edge_to dir = B (concat [ "[" ; text (sprintf \"style=%S,\" (match dir with `normal -> edge_to_normal | `back -> \"triangle 45-\")) ; "color={draw_red}]" ]) in
"<OO{ Header { shift_x = -2.9
             ; shift_y = -0.2
             ; scale = None
             ; node = { n_color = \"mediumseagreen\" ; shape = Box true }
             ; edge = { e_color = None ; style = None } } }O

  /* nodes */
  compcert_c [texlbl="O{B S.C.compcert}O"]
  clight [texlbl="Clight"]
  c_minor [texlbl="CO{symbolc '#'}Ominor"]
  cminor [texlbl="Cminor"]
  cminorsel [texlbl="CminorSel"]
  rtl [texlbl="RTL"]
  ltl [texlbl="LTL"]
  ltlin [texlbl="LTLin"]
  linear [texlbl="Linear"]
  mach [texlbl="Mach"]
  asm [texlbl="O{B S.C.asm}O"]

  error O{attr_error}O

  /* nodes: "fct" version */
  compcert_c_fct [shape=point, style=invis]
  clight_fct [shape=point, style=invis]
  c_minor_fct [shape=point, style=invis]
  cminor_fct [shape=point, style=invis]
  cminorsel_fct [shape=point, style=invis]
  rtl_fct [shape=point, style=invis]
  ltl_fct [shape=point, style=invis]
  ltlin_fct [shape=point, style=invis]
  linear_fct [shape=point, style=invis]
  mach_fct [shape=point, style=invis]

  /* */
  compcert_c -> rtl [style=invis]
  clight -> cminorsel [style=invis]
  cminorsel -> ltlin [style=invis]
  cminor -> linear [style=invis]
  ltlin -> asm [style=invis]

  compcert_c_fct -> cminorsel_fct [style=invis]
  clight_fct -> cminor_fct [style=invis]
  cminorsel_fct -> ltl_fct [style=invis]
  cminor_fct -> ltlin_fct [style=invis]
  ltlin_fct -> mach_fct [style=invis]

  /* */
  { rank = same ;
    compcert_c -> compcert_c_fct O{edge_to_fct `normal}O
    compcert_c_fct -> clight O{edge_to `normal}O
    clight -> clight_fct O{edge_to_fct `normal}O
    clight_fct -> c_minor O{edge_to `normal}O }
  c_minor -> c_minor_fct O{edge_to_fct `normal}O
  c_minor_fct -> cminor O{edge_to `normal}O
  { rank = same ;
    rtl -> cminorsel_fct O{edge_to `back}O
    cminorsel_fct -> cminorsel O{edge_to_fct `back}O
    cminorsel -> cminor_fct O{edge_to `back}O
    cminor_fct -> cminor O{edge_to_fct `back}O }
  rtl -> rtl_fct O{edge_to_fct `normal}O
  rtl_fct -> ltl O{edge_to `normal}O
  { rank = same ;
    ltl -> ltl_fct O{edge_to_fct `normal}O
    ltl_fct -> ltlin O{edge_to `normal}O
    ltlin -> ltlin_fct O{edge_to_fct `normal}O
    ltlin_fct -> linear O{edge_to `normal}O }
  linear -> linear_fct O{edge_to_fct `normal}O
  linear_fct -> mach O{edge_to `normal}O
  { rank = same ;
    asm -> mach_fct O{edge_to `back}O
    mach_fct -> mach O{edge_to_fct `back}O }

  /* */
  compcert_c_fct -> error O{fct_edge}O
  clight_fct -> error O{fct_edge}O
  c_minor_fct -> error O{fct_edge}O
  cminor_fct -> error O{fct_edge}O
  cminorsel_fct -> error O{fct_edge}O
  rtl_fct -> error O{fct_edge}O
  ltl_fct -> error O{fct_edge}O
  ltlin_fct -> error O{fct_edge}O
  linear_fct -> error O{fct_edge}O
  mach_fct -> error O{fct_edge}O

O>"))
           (let monad = index_ bot "monad" in
            [ ( let darksalmon = Color.color_name_ (Color.of_int_255 (0x3C, 0xB3, 0x71)) in
               B "[style=invis]"
             , darksalmon
             , darksalmon
             , B "[texlbl=\"{phantom monad}\", color=white]" )

           ; ( B ("[style=\"" ^^ text edge_to_normal ^^ "\",color={Color.color_name_ (Color.of_int_255 (0xF5, 0xA7, 0x5F))}]")
             , Color.color_name_ (Color.of_int_255 (0x3C, 0xB3, 0x71))
             , Color.color_name_ (let i = 200 in Color.of_int_255 (i, i, i))
             , B "[texlbl=\"{monad}\"]" ) ]))

(* ********************************************************* *)
; B.Top ("CompCert, the generation{newline}from {P.coq} to {P.ocaml}",
         minipage (`Cm 5.)
           (Label.notation_ (itemize [ "``{red S.C.human}'': an arbitrary sequence of character"
                                     ; "``{red S.C.compcert}'': programs compiled successfully with {Version.compcert} <!-dc!>~{cite ["Leroy-Compcert-CACM"]}"
                                     ; "``{red S.C.asm}'': programs compiled successfully with {Version.compcert} <!-dasm!>" ]))
         ^^
         let open Code.Dot in
         let darksalmon_ = Color.of_int_255 (0xF5, 0xA7, 0x5F) in
         let darksalmon = color_name_ darksalmon_ in
         let mediumseagreen_ = Color.of_int_255 (0x3C, 0xB3, 0x71) in
         let mediumseagreen = color_name_ mediumseagreen_ in
         let deepskyblue = Color.of_int_255 (0x00, 0xBF, 0xFF) in
         let floralwhite = B "deepskyblue" in
         let ocaml = texttt (Color.textcolor_ deepskyblue P.ocaml) in
         let coq = texttt (Color.textcolor_ deepskyblue P.coq) in
         let gcc = texttt (Color.textcolor_ deepskyblue S.C.gcc) in
         let col_fct = Color.textcolor_ darksalmon_ in
         let black s = small (emph (Color.textcolor_ (let i = 0x86 in Color.of_int_255 (i, i, i)) s)) in
         let greentriangleright = Color.textcolor_ mediumseagreen_ "{blacktriangleright}" in
"<OO{ Header { shift_x = -2.0
             ; shift_y = -6.2
             ; scale = Some 0.9
             ; node = { n_color = \"mediumseagreen\" ; shape = Box true }
             ; edge = { e_color = None ; style = Some \"-triangle 45\" } } }O

  subgraph cluster_coq {
    style="dashed, rounded"
    color=O{floralwhite}O

    compcert_c [texlbl="O{B S.C.compcert}O"]
    asm [texlbl="O{B S.C.asm}O"]

    compcert_c_fct [texlbl="O{multiline [ col_fct (ocaml ^^ "-compiling") ; black ("(generated from " ^^ coq ^^ ")")]}O", color=darksalmon]

    compcert_c -> compcert_c_fct [color=O{mediumseagreen}O]
  }

  subgraph cluster_other1 {
    style="dashed, rounded"
    color=O{floralwhite}O

    human_c [texlbl="O{B S.C.human}O"]
    human_c_fct [texlbl="O{multiline [ col_fct (gcc ^^ "-preprocessing {greentriangleright} " ^^ ocaml ^^ "-parsing") ; black ("(not yet generated from " ^^ coq ^^ ")") ]}O", color=darksalmon]
    human_c -> human_c_fct [color=O{mediumseagreen}O]

  }

  subgraph cluster_other2 {
    style="dashed, rounded"
    color=O{floralwhite}O

    asm_fct [texlbl="O{multiline [ col_fct (ocaml ^^ "-printing {greentriangleright} " ^^ gcc ^^ "-linking") ; black ("(not yet generated from " ^^ coq ^^ ")") ] }O", color=darksalmon]
    exec [texlbl="executable"]
    asm_fct -> exec [color=O{mediumseagreen}O]
  }

  human_c_fct -> compcert_c [color=O{mediumseagreen}O]
  compcert_c_fct -> asm [color=O{mediumseagreen}O]
  asm -> asm_fct [color=O{mediumseagreen}O]

  error [texlbl="O{B (index_ bot "monad")}O"]
  compcert_c_fct -> error [color=O{darksalmon}O]
  human_c_fct -> error [color=O{darksalmon}O]
  asm_fct -> error [color=O{darksalmon}O]

O>")

(* ********************************************************* *)
; B.Center ("{P.simcert}, towards the correctness proof",
            let open Code.Dot in
            let deepskyblue = Color.of_int_255 (0x00, 0xBF, 0xFF) in
            let module S = S_sz (struct let normal = normalsize let footnote = footnotesize let tiny = tiny let color_keyword = Some deepskyblue end) in
            let module SL_p = S.SL_gen (struct let sl = "PROGRAM" end) in
            let dodgerblue = Color.color_name_ (Color.of_int_255 (0x1E, 0x90, 0xFF)) in
            let firebrick = color_name_ (Color.of_int_255 (0xB2, 0x22, 0x22)) in
            let floralwhite = B "deepskyblue" in
            let style_back = B "style=\"angle 45-\"" in
"<OO{ Header { shift_x = -0.8
             ; shift_y = -6.6
             ; scale = Some 0.8
             ; node = { n_color = \"darksalmon\" ; shape = Box false }
             ; edge = { e_color = Some dodgerblue ; style = Some \"-angle 45\" } } }O

  pdf_sh [shape=note, texlbl="O{multiline [S.Manual.Sh.C.human ; "(pdf)"]}O"]
  pdf_txt_sh [shape=note, style=filled, fillcolor=papayawhip, texlbl="O{multiline [S.Manual.Sh.C.human ; "(txt)"]}O"]

  pseudocode [shape=ellipse, style=dashed, color=dodgerblue, texlbl="O{ multiline [ "pseudo-code and" ; "decoder generator" ] }O"]

  subgraph cluster_simsoc {
    style="dashed,rounded"
    color=O{floralwhite}O

    subgraph cluster_simlight {
      style=bold
      color=darksalmon
      iss [shape=note, style=filled, fillcolor=papayawhip, texlbl="O{B S.Manual.Sh.C.compcert}O"]
      simlight_dots [shape=note, texlbl="O{ multiline [ S.SL.C.compcert ; "libraries" ] }O"]
    }
  }

  subgraph cluster_coq {
    style="dashed,rounded"
    color=O{floralwhite}O

    mid [shape=ellipse, style=dashed, color=dodgerblue, texlbl="O{ multiline [ "deep embedded" ; "pretty-printer" ; "for {S.C.compcert}" ] }O"]

    subgraph cluster_compcert_simlight {
      style=bold
      color=darksalmon
      bgcolor=papayawhip
      coq_src2 [shape=note, style=filled, fillcolor=papayawhip, texlbl="O{B S.Manual.Sh.Coq.Deep.compcert}O"]
      coq_src_dot [shape=note, texlbl="O{ multiline [ S.SL.Coq.Deep.compcert ; "libraries" ] }O"]
    }

    subgraph cluster_simulateur {
      style=bold
      color=darksalmon
      coq_src1 [shape=note, style=filled, fillcolor=papayawhip, texlbl="O{B S.Manual.Sh.coq}O"]
      coq_src_simul_dot [shape=note, texlbl="O{ multiline [ S.SL.coq ; "libraries" ] }O"]
    }

    coq_proof [shape=note, color=firebrick, texlbl="O{ multiline [ "Correctness proof :" ; S.SL.coq ; "{index_ cong "correct"}" ; "{S.SL.Coq.Deep.compcert} ?" ] }O"]
  }

  /* */
  iss -> simlight_dots [style=invis]
  coq_src2 -> coq_src_dot [style=invis]
  coq_src1 -> coq_src_simul_dot [style=invis]
  pdf_sh -> coq_src1 [style=invis]

  /* */
  pdf_sh -> pdf_txt_sh [label="pdftotext", constraint=false]
  pdf_txt_sh -> pseudocode [constraint=false]
  pseudocode -> iss
  pseudocode -> coq_src1
  simlight_dots -> mid [ltail=cluster_simlight]
  coq_src_dot -> mid [ltail=cluster_compcert_simlight, O{style_back}O]
  coq_src_dot -> coq_proof [color=O{firebrick}O, ltail=cluster_compcert_simlight]
  coq_src_simul_dot -> coq_proof [color=O{firebrick}O, ltail=cluster_simulateur]

O>")

(* ********************************************************* *)
; B.Abr (let title x = "Example: one page in the {S.Manual.Sh.C.human} (p. " ^^ x ^^ ")" in
         [ B.Center (let page = 234 in title (latex_of_int page), includegraphics ~x:(-0.5) ~y:(-.5.0) ~scale:0.7 \"sh4_and.pdf\")
         ; B.Center (title "234 middle", includegraphics ~x:(-0.5) ~y:(-1.) ~scale:0.4 Argument.page_middle) ])

(* ********************************************************* *)
; B.Abr
  (let sh4_example =
     BatList.map
       (fun (x, trim_top, img, title) ->
         B.Center ("Example: conversion to text" ^^ title, includegraphics ~x ~y:(-1.) ~trim:(`Mm 0., `Mm 0., `Mm 0., `Mm trim_top) ~scale:0.6 img)) in

   BatList.map (fun x -> B.Abr x)
     [ sh4_example
         [ 0., 55., Argument.img1, " and syntax error"
         ; -1., 52., Argument.img3, " and syntax error"
         ; 0., 52., Argument.img2, " and type error" ]

     ; (let l =
          let open English in
          let s1, s2 =
            "The compilation correctly terminates with {Version.gcc}.",
            Label.question_ "Does it compile with {P.compcert}?" in
          [ [ yes ; yes   ; maybe ], Some (s1 ^^ s2)
          ; [ yes ; yes   ; no    ], Some (s1
                                           ^^
                                           Label.problem_ "``<!unsigned long!>'' is not supported in {Version.compcert} (inside ``struct'')."
                                           ^^
                                           Label.fix_ "Generate every ``<!unsigned long!>'' to ``int'' so that {S.Manual.Sh.C.gcc} {in_} {S.C.compcert}.") ] in
        let lg = latex_of_int (BatList.length l) in
        BatList.mapi
          (fun pos (h_comment, msg) ->
            B.Top ("Example: limitation of {P.compcert}", (*  [{latex_of_int (Pervasives.succ pos)}/{lg}] *)
                   sh4_intro
                   ^^
                   "<@@{H_comment h_comment }@
struct S { unsigned long i:1 }
main () {}
@>"
                   ^^
                   (match msg with None -> "" | Some s -> s)))
          l)
     ; sh4_example [ 0., 55., Argument.img4, "" ] ])

(* ********************************************************* *)
; B.Center ("Example: fragment of patch in OCaml",
"<~
[ [ Replace_all ("&&", "&"), p [ 1065 ]
    (* not Coq well typed otherwise *)
  ; Replace_all ("(long i)", "i"), [ R (1077, 2) ] ]
; [ comment_first ", int *FPUL", p [ 4003 ; 4008 ; 6913 ; 6918 ]
  ; Replace_first (S "*FPUL", "FPUL"), p [ 4005 ; 4010 ; 6915 ; 6921 ; 2113 ; 2116 ] ]
; [ Replace_all ("TLB[MMUCR. URC] ", "TLB_MMUCR_URC"), [ R (4162, 13) ] ]
; [ Replace_first (S "MACL", "MACL_"), p [ 4222 ]
  ; Replace_first (S "STS", "STS_"), p [ 6924 ] ]
(* type error *)
; [ Replace_first (S "MACH&0x00008000",
                   "bool_of_word(MACH&0x00008000)"), p [ 4290 ] ]
(* simplifying *)
; [ Replace_first (All, "if_is_write_back_memory_and_look_up_in_operand_cache_eq_miss_then_allocate_operand_cache_block(R[n]);"), [ R (5133, 3) ]
  ; Replace_first (Re "(is_dirty_block(R\\[n\\])) +write_back(R\\[n\\]);?"
                  , "_is_dirty_block_then_write_back(R[n]);"), p [ 5502 ; 5543 ] ] ]
~>")
(*
"<~
[ [ Replace_tilde 2, p [ 1434 ; 1439 ; 4276 ; 4279 ]
  ; Replace_tilde 1, p [ 5424 ; 6115 ; 6277 ] ]
; [ Add_char_end ";", p [ 1502 ; 6932 ]
  ; Replace_first (S ",", ";"), p [ 3871 ]
  ; Replace_first (S "Lo", "lo"), p [ 3955 ]
  ; Replace_first (S "}", "{"), p [ 4555 ]
  ; Replace_first (S "long n", "long n)"), p [ 4979 ]
  ; Replace_first (S "((", "("), p [ 5280 ]
  ; Replace_all ("–=", "-="), [ R (6713, 151) ]
  ; Replace_first (S "H'", "H_"), p [ 7302 ] ]

; [ add_int_type, p [ 6103 ; 6269 ]
  ; Replace_first (S "d, n", "int d, int n"), p [ 4746 ] ] ]
~>"
*)

(* ********************************************************* *)
; B.Center ("Patching the {S.Manual.Sh.C.human}",
            "Patching data = 8K (without counting blanks)" ^^ vspace (`Em 1.)
            ^^
            let perc n =
              Color.textcolor_ (Color.of_int_255 (0xCF, 0x5C, 0x16)) ((latex_of_int n) ^^ "%") in
            tabular [`L;`Vert;`R;`Vert;`R;`Vert;`R;`Vert;`R]
              [ Data [S.Manual.Sh.C.human; "words" ; "common" ; "" ; "changed" ]
              ; Hline
              ; Data [""; "" ; "" ; "deleted" ; "" ]
              ; Data ["original.txt" ; "86980" ; "85070 {perc 98}" ; "499 {perc 1}" ; "1411 {perc 2}"]
              ; Hline
              ; Data [""; "" ; "" ; "inserted" ; "" ]
              ; Data ["patched.txt" ; "87372" ; "85070 {perc 97}" ; "872 {perc 1}" ; "1430 {perc 2}"] ])

(* ********************************************************* *)
; B.Abr
  (let l =
     let open English in
     [ [ yes ; yes ; maybe ; maybe ], Some (Label.problem_ "Does it compile with {P.compcert}?")
     ; [ yes ; yes ; no ; no ], Some (concat_line_t [ "No, 64 bits value are not supported in {Version.compcert}."
                                                    ; "Remark: {notin} {S.C.compcert} {rightarrow_} {notin} {S.C.asm}." ]
                                      ^^
                                      Label.fix_ "Emulate operations on 64 bits data structure with a 32 bits library, then {S.SL.ArmSh.C.gcc} {in_} {S.C.asm}.") ] in
   let lg = latex_of_int (BatList.length l) in
   BatList.mapi
     (fun pos (h_comment, msg) ->
       B.Top ("Example: typing {S.SL.ArmSh.C.gcc} with {P.compcert}",  (*[{latex_of_int (Pervasives.succ pos)}/{lg}]*)
              vspace (`Em 1.)
              ^^
              sl_intro S.SL.ArmSh.C.gcc
              ^^
              "<@@{H_comment h_comment}@
#include <inttypes.h>

void main() {
  int64_t x = 0;
}
@>"
              ^^
              (match msg with None -> "" | Some s -> s)))
     l)

(* ********************************************************* *)
; B.Abr
  (let (tit1, l1), (tit2, l2), (tit3, l3) =
     let open English in
     let in__ = "{in_}" in
     let tab l = tabular [ `L ; `Sep space ; `L ; `Sep space ; `L ] (BatList.map (fun l -> Data l) l) in
  (** *)
     ("",
      [ [ yes ; yes   ; maybe ; maybe ], Some (tab
                                                 [ [ "{S.SL.ArmSh.C.human} {in__}" ; S.C.gcc ; "" ]
                                                 ; []
                                                 ; [] ]
                                               ^^
                                               Label.problem_ "Does it compile with {P.compcert} ?")

      ; [ yes ; yes   ; yes   ; maybe ], Some (tab
                                                 [ [ "{S.SL.ArmSh.C.human} {in__}" ; S.C.gcc ; "" ]
                                                 ; [ "" ; "{S.SL.ArmSh.C.gcc} {in__}" ; S.C.compcert ]
                                                 ; [] ]
                                               ^^
                                               Label.fact "The compilation correctly terminates with {Version.compcert} <!-dc!>.")

      ; [ yes ; yes   ; yes   ; yes   ], Some (tab
                                                 [ [ "{S.SL.ArmSh.C.human} {in__}" ; S.C.gcc ; "" ]
                                                 ; [ "" ; "{S.SL.ArmSh.C.gcc} {in__}" ; S.C.compcert ]
                                                 ; [ "" ; "" ; "{S.SL.ArmSh.C.compcert} {in__} {S.C.asm}" ] ]
                                               ^^
                                               Label.fact "The compilation correctly terminates with {Version.compcert} <!-dasm!>.")

      ; [ yes ; yes   ; yes   ; yes   ], Some (Label.problem_ "Where is the problem?") ]),

  (** *)
     (", towards validation",
      [ [ yes ; yes   ; yes   ; yes   ],
        (Some "At runtime, tests fail. Indeed, <!line 1!> is not always equal to <!line 2!>:{
texttt (tabular (interl 4 `L)
    [ Data [ "" ; "gcc -m64" ; Color.cellcolor_ orange ^^ "gcc -m32 -O0" ; "gcc -m32 -O1" ]
    ; Data [ "" ; "" ; Color.cellcolor_ orange ^^ "{textrm P.compcert} ia32-linux" ; "gcc -m32 -O2" ]
    ; Data [ "" ; "" ; Color.cellcolor_ orange ; "gcc -m32 -O3" ]
    ; Hline
    ; Data [ "line 1" ; blue "100000000" ; Color.cellcolor_ orange ^^ blue "1" ; blue "0" ]
    ; Data [ "line 2" ; blue "100000000" ; Color.cellcolor_ orange ^^ blue "0" ; blue "0" ] ]) }
{newline}
Remark: {blue "<!Int32.shift_left 1_l 32!>"} ${overset P.ocaml longrightarrow}$ {blue "<!1_l!>"}.")

      ; [ yes ; yes   ; yes   ; yes   ], Some (Label.warning_ "From {S.C.human}, {Version.compcert} sometimes performs as heuristic an external call to the <!gcc -m32 -O0!> preprocessor (which wrongly optimizes here)."
                                               ^^
                                               Label.fix_ "Regenerate the {S.Manual.ArmSh.C.compcert} so that it computes the shift ``<!<<!>'' with 64 bits value as argument everywhere used.") ]),

  (** *)
     ("", [ (*[ yes ; yes   ; yes   ; yes   ],
       (Some "As remark:
{
texttt (tabular (interl 3 `L)
    [ Data [ "" ; "gcc -m64" ; Color.cellcolor_ orange ^^ "gcc -m32 -O0" ]
    ; Data [ "" ; "gcc -m32 -O1" ; Color.cellcolor_ orange ^^ textrm P.compcert ]
    ; Data [ "" ; "gcc -m32 -O2" ; Color.cellcolor_ orange ]
    ; Data [ "" ; "gcc -m32 -O3" ; Color.cellcolor_ orange ]
    ; Hline
    ; Data [ "line 1" ; blue "ffffffff" ; Color.cellcolor_ orange ^^ blue "0" ]
    ; Data [ "line 2" ; blue "ffffffff" ; Color.cellcolor_ orange ^^ blue "ffffffff" ] ]) }")*) ]) in
   let lg1, lg2 = BatList.length l1, BatList.length l2 in
   let lg = latex_of_int (lg1 + lg2 + BatList.length l3) in
   let title tit pos lg = "Example: limitation of {P.compcert}" ^^ tit (*^^ " [{latex_of_int (Pervasives.succ pos)}/{lg}]"*) in
   [ B.Abr
       (BatList.mapi
          (fun pos (h_comment, msg) ->
            B.Top (title tit1 pos lg,
                   sl_intro S.SL.ArmSh.C.gcc
                   ^^
                   "<@@{H_comment h_comment}@
#include <stdio.h>
void main() {                int i = 32 ;
  return(                1lu <<  i)     ;
                                          }
@>"
                   ^^
                   (match msg with None -> "" | Some s -> s)))
          l1)

  (* FIXME factorize with above  ***************************** *)
   ; B.Abr
     (BatList.mapi
        (fun pos (h_comment, msg) ->
          B.Top (title tit2 (lg1 + pos) lg,
                 sl_intro S.SL.ArmSh.C.gcc
                 ^^
                 "<@@{H_comment h_comment}@
#include <stdio.h>
void main() {                int i = 32 ;
  printf("line 1 %lx\n", 1lu <<  i)     ;
  printf("line 2 %lx\n", 1lu << 32)     ; }
@>"
                 ^^
                 (match msg with None -> "" | Some s -> s)))
        l2)

  (* FIXME factorize with above  ***************************** *)
   ; B.Abr
     (BatList.mapi
        (fun pos (h_comment, msg) ->
          B.Top (title tit3 (lg1 + lg2 + pos) lg,
                 sl_intro S.SL.ArmSh.C.gcc
                 ^^
                 "<@@{H_comment h_comment}@
#include <stdio.h>
void main() {                 int i = 32  ;
  printf("line 1 %lx\n", (1lu <<  i) - 1) ;
  printf("line 2 %lx\n", (1lu << 32) - 1) ; }
@>"
                 ^^
                 (match msg with None -> "" | Some s -> s)))
        l3) ])

(* ********************************************************* *)
; B.Abr (
  let title = "Printing the {S.C.compcert} AST from {P.coq} to {P.coq}" in
  let white = Color.textcolor_ (Color.of_int_255 (0xFF, 0xFF, 0xFF)) in
  let phantom_vert = phantom (index "" (footnotesize (texttt "p"))) in
  BatList.flatten
    [ BatList.map (fun body -> B.Center (title, body))
[ comp_intro ^^ "<#
Inductive   floatsize : Type :=
  |  F32  : floatsize |  F64  : floatsize.
Inductive   type : Type        :=
  |  Tvoid  : type |  Tfloat  : floatsize -> type
  |  Tfunction  : typelist -> type -> type
with typelist : Type :=
  |  Tnil  : typelist
  |  Tcons  : type -> typelist -> typelist.


Definition  ident :=  positive.
Record      program (A : Type) : Type := mkprogram
  {  prog_funct  : list (ident * A)
  ;  prog_main  : ident }.
Definition  ast :=  program  type.
#>"
; "Copy-paste constructors in the type definition." ^^ phantom_vert ^^ "<X            floatsize        :=
  |  F32              |  F64             .
            tt                 :=
  |  Tvoid         |  Tfloat
  |  Tfunction

  |  Tnil
  |  Tcons                                .


            ident :=  positive.
            program  A                :=
  {  prog_funct
  ;  prog_main          }.
            ast :=  program  type.X>"
; "Use recursors associated to types (automatically generated in {P.coq})." ^^ phantom_vert ^^ "<#
Definition _floatsize        := __floatsize
  | "F32"             | "F64"            .
Definition _tt_ T (ty : #{PPP}#) := ty _ _floatsize
  | "Tvoid"        | "Tfloat"
  | "Tfunction"

  | "Tnil"
  | "Tcons"                               .
  Definition _type := _tt_ _ (@__type).
  Definition _typelist := _tt_ _ (@__typelist).
Definition _ident := _positive.
Definition _program {A} #{PPP}#         := @__program #{PPP}#
  { "prog_funct"
  ; "prog_main"         }.
Definition _ast := _program _type.
#>"
; "Instanciate the previous functor with some monadic combinators~{cite ["peyton-jones-tackling-09"]}:" ^^ "<#
  Notation "A ** n" := (A ^^ n --> A) (at level 29) : type_scope.

Check _INDUCTIVE : string -> forall n, s ** n.
  Notation "| x" := (_INDUCTIVE x _) (at level 9).

Check _RECORD : forall n, vector string n -> s ** n.
  Notation "{ a ; .. ; b }" :=
    (_RECORD _ (Vcons _ a _ .. (Vcons _ b _ (Vnil _)) ..)).
#>" ^^
"<#
Check _floatsize : floatsize -> s.
Check _type : type -> s.
Check _typelist : typelist -> s.
Check _ident : ident -> s.
Check _program : forall A, (A -> s) -> program A -> s.
Check _ast : ast -> s.
#>"
; Label_small.warning "type reconstruction becomes undecidable."
     ^^ newline ^^
     Label_small.fix "manually set the arity of each constructors." ^^ "<#
  Notation "A [ a ; .. ; b ] -> C" :=
    (A ** a -> .. (A ** b -> C) ..) (at level 90).

Definition __floatsize {A} : A [ 0   (* F32       :           _ *)
                               ; 0 ] (* F64       :           _ *)
                             -> _ := #{PPP}#.
Definition _tt_ A B (f : _ -> Type) := f (
                             A [ 0   (* Tvoid     :           _ *)
                               ; 1   (* Tfloat    : _ ->      _ *)
                               ; 2   (* Tfunction : _ -> _ -> _ *)
                               ; 0   (* Tnil      :           _ *)
                               ; 2 ] (* Tcons     : _ -> _ -> _ *)
                             -> B).
Definition __type {A} : _tt_ A _ (fun ty => _ -> ty) := #{PPP}#.
Definition __typelist {A} : _tt_ A _ (fun ty => _ -> ty) := #{PPP}#.
#>"
]
    ; (let open Version.Size in
       let f_size size f =
         Data (let sz = float_of_int size /. 1024. in
               if sz < 1. then
                 f ".{text (String.make 1 (sprintf \"%.1f\" sz).[2])}"
               else
                 assert false) in
       let tab = tabular [ `L ; `Sep space
                         ; `R ; `Sep space
                         ; `R ; `Sep ""
                         ; `L ; `Sep space
                         ; `L ] in
       BatList.map (fun body ->
         B.Top (title,
                Label.problem_
                  (tab
                     [ f_size Slv6_iss.size (fun x -> [ "Size of the original" ; "{S.Pseudocode.C.compcert}:" ; "0" ; x ;"M" ])
                     ; Data [ "Size of the printed" ; "{S.Pseudocode.Coq.Deep.compcert}:" ; "{Slv6_iss.Old.no_indent}";"";"M" ]
                     ; Data [ "Size of the printed" ; "{S.Pseudocode.Coq.Deep.compcert}:" ; "{Slv6_iss.Old.with_indent}";"";"M (with indentation)" ] ])
                ^^
                body))
         [ pause ^^ Label.warning_ "Each constructor in the {S.C.compcert} AST carries as extra field a type information. So the printed term contains repetitive types everywhere, at every node position."
         ; Label.fix_
           ("Print a smaller code preserving the {English.bdiz}-convertibility with the previous one."
            ^^
            itemize [ "Rewrite with digit-notation for numbers: e.g. ``<!16!>'' instead of ``<!xO (xO (xO (xO xH)))!>''."
                    ; "Add notations or coercions for every constructor."
                    ; "Automatically pattern recurring definitions and types." ]

             ) ^^(tab
               [ f_size Slv6_iss.New.facto_ml (fun x -> [ "Size of the printed" ; "{S.Pseudocode.Coq.Deep.compcert}:" ; phantom "5" ^^ "0" ; x ; "M" ]) ]) ])
    ; BatList.map (fun body -> B.Center (title, body))
[ "Example of coercions and notations:" ^^ "<U
Coercion Int.repr : Z >-> int.
Coercion Vint : int >-> val.
Coercion Sdo : expr >-> statement.
Notation "`* e `: t" := (Ederef e t) (at level 20).
Notation "# v `: t" := (Eval v t) (at level 20).
Notation "$ id `: t" := (Evar id t) (at level 20).
Notation "\ id `: t" := (Evalof (Evar id t) t) (at level 20).
Notation "& e `: t" := (Eaddrof e t) (at level 20).
Notation "e1 ? e2 `: e3 `: t" := (Econdition e1 e2 e3 t) (at level 20).
Notation "e -- `: t" := (Epostincr Decr e t) (at level 20).
Notation "e ++ `: t" := (Epostincr Incr e t) (at level 20).
Notation "e1 `= e2 `: t" := (Eassign e1 e2 t) (at level 8).
Notation "e | id `: t" := (Efield e id t) (at level 20).
U>"
]]
)

(* ********************************************************* *)
; B.Center ("The generated {S.Decoder.Sh.coq}",
"Decode a given word (16, 32 bits...) to instruction to be executed later.
<Z
Local Notation "0" := false.
Local Notation "1" := true.
Definition decode (w : word) : Z{PPP}Z := match w16_of_word w with
Z{PPP}Z
(* 9.4.1 ANDI *)
| word16 1 1 0 0 1 0 0 1 _ _ _ _ _ _ _ _ => DecInst _ (ANDI w[n7#n0])
(* 9.4.2 ANDM *)
| word16 1 1 0 0 1 1 0 1 _ _ _ _ _ _ _ _ => DecInst _ (ANDM w[n7#n0])
Z{PPP}Z
| _ => DecUndefined_with_num inst 0
end.
Z>")
(* "<~
; ("Format                     Summary of Operation        Instruction Code       States    T Bit",
   "AND     Rm,Rn              Rm & Rm \226\134\146 Rn               ",
   [(I_1, 1); (I_0, 2); (I_1, 1); (I_m, 4); (I_n, 4); (I_0, 1); (I_1, 1); (I_0, 2); ])
; ("Format                     Summary of Operation        Instruction Code       States    T Bit",
   "AND     #imm,R0            R0 & imm \226\134\146 R0              ",
   [(I_i, 8); (I_1, 1); (I_0, 2); (I_1, 1); (I_0, 2); (I_1, 2); ])
; ("Format                     Summary of Operation        Instruction Code       States    T Bit",
   "AND.B #imm,@(R0,GBR) (R0+GBR) & imm \226\134\146                 ",
   [(I_i, 8); (I_1, 1); (I_0, 1); (I_1, 2); (I_0, 2); (I_1, 2); ])
~>" *)

(* ********************************************************* *)
; B.Center ("The patched {S.Pseudocode.Sh.C.human}",
"<::{let open English in H_comment [ yes ; maybe ; maybe ; maybe ]}:
ANDI(long i)         /* AND #imm,R0 */
  {
    R[0]&=(0x000000FF & (long)i);
    PC+=2;
  }

ANDM(long i)         /* AND.B #imm,@(R0,GBR) */
  {
    long temp;
    temp=(long)Read_Byte(GBR+R[0]);
    temp&=(0x000000FF & (long)i);
    Write_Byte(GBR+R[0],temp);
    PC+=2;
  }
:>")

(* ********************************************************* *)
; B.Center ("The generated {S.Pseudocode.Sh.C.asm}",
"<@@{let open English in H_comment [ yes ; yes ; yes ; yes ]}@
/* 9.4.1 ANDI */
void ANDI(struct SLSH4_Processor *proc, const long i) {
  set_reg(proc,0,(reg(proc,0) & (0x000000FF & (long)(i))));
  set_pc_raw(proc,(proc->pc + 2)); }

/* 9.4.2 ANDM */
void ANDM(struct SLSH4_Processor *proc, const long i) {
  long temp;
  temp = (long)(Read_Byte(proc->mmu_ptr, (proc->GBR + reg(proc,0))));
  temp = (temp & (0x000000FF & (long)(i)));
  Write_Byte(proc->mmu_ptr, (proc->GBR + reg(proc,0)), temp);
  set_pc_raw(proc,(proc->pc + 2)); }
@>")

(* ********************************************************* *)
; B.Top ("The generated {S.Pseudocode.Sh.coq}",
footnotesize "(shallow embedding)"
^^
"<#
  Notation "'<.' loc    '.>' A" := (_get_loc (fun loc => A))
                            (at level 200, A at level 100, loc ident).
  Notation "'<'      st  '>' A" := (_get_st  (fun st  => A))
                            (at level 200, A at level 100, st  ident).
  Notation "'<:' loc st ':>' A" := (<.loc.> <st> A) #{PPP}# .
(* 9.4.2 ANDM *)
Definition ANDM (i : word) : semfun _ := <s0>
  [ <     st > bind (Read_Byte (add (reg_content st GBR)
                                    (reg_content st (mk_regnum 0))))
                    (update_loc n0 (*temp*))
  ; <.loc   .> update_loc n0 (*temp*) (and (get_loc n0 (*temp*) loc)
                                           (and (repr (Zpos 255)) i))
  ; <:loc st:> Write_Byte (add (reg_content st GBR)
                               (reg_content st (mk_regnum 0)))
                          (get_loc n0 (*temp*) loc)
  ; <     st > set_reg PC (add (reg_content st PC) (repr 2)) ].
#>")

(* ********************************************************* *)
; B.Top ("The generated {S.Pseudocode.Sh.Coq.Deep.compcert}",
            let o, x = PPP_empty, PPP_full in
            let module Version = struct
              let v0 = "<Z
(* 9.4.2 ANDM *)
Definition ANDM :=
  {| fn_return := void;
     fn_params := [
proc -: `*` typ_struct_SLSH4_Processor;
i -: int32];
     fn_vars := [
temp -: int32];
     fn_body :=
($ temp`:T1) `= (Ecast (call (\Read_Byte`:T84) E[(valof ((`*(\proc`:T9)`:T17)|mmu_ptr`:T71) T71); (valof ((`*(\proc`:T9)`:T17)|GBR`:T2) T2)+(call (\reg`:T12) E[\proc`:T9; #0`:T1] T2)`:T2] T10) T1)`:T1;;
($ temp`:T1) `= ((\temp`:T1)&((#255`:T1)&(Ecast (\i`:T1) T1)`:T1)`:T1)`:T1;;
(call (\Write_Byte`:T95) E[(valof ((`*(\proc`:T9)`:T17)|mmu_ptr`:T71) T71); (valof ((`*(\proc`:T9)`:T17)|GBR`:T2) T2)+(call (\reg`:T12) E[\proc`:T9; #0`:T1] T2)`:T2; \temp`:T1] T10);;
(call (\set_pc_raw`:T18) E[\proc`:T9; (valof ((`*(\proc`:T9)`:T17)|pc`:T2) T2)+(#2`:T1)`:T2] T3) |}.
Z>"
              let v1 = "<Z
(* 9.4.2 ANDM *)
Definition ANDM :=
  {| fn_return := void
   ; fn_params := [proc -: `*` typ_struct_SLSH4_Processor; i -: int32]
   ; fn_vars   := [temp -: int32]
   ; fn_body   :=
($ temp`:Z{o}Z) `= (Ecast (call (\Read_Byte`:Z{o}Z) E[(valof ((`*(\proc`:Z{o}Z)`:Z{o}Z)|mmu_ptr`:Z{o}Z) Z{o}Z); (valof ((`*(\proc`:Z{o}Z)`:Z{o}Z)|GBR`:Z{o}Z) Z{o}Z)+(call (\reg`:Z{o}Z) E[\proc`:Z{o}Z; #0`:Z{o}Z] Z{o}Z)`:Z{o}Z] Z{o}Z) Z{o}Z)`:Z{o}Z;;
($ temp`:Z{o}Z) `= ((\temp`:Z{o}Z)&((#255`:Z{o}Z)&(Ecast (\i`:Z{o}Z) Z{o}Z)`:Z{o}Z)`:Z{o}Z)`:Z{o}Z;;
(call (\Write_Byte`:Z{o}Z) E[(valof ((`*(\proc`:Z{o}Z)`:Z{o}Z)|mmu_ptr`:Z{o}Z) Z{o}Z); (valof ((`*(\proc`:Z{o}Z)`:Z{o}Z)|GBR`:Z{o}Z) Z{o}Z)+(call (\reg`:Z{o}Z) E[\proc`:Z{o}Z; #0`:Z{o}Z] Z{o}Z)`:Z{o}Z; \temp`:Z{o}Z] Z{o}Z);;
(call (\set_pc_raw`:Z{o}Z) E[\proc`:Z{o}Z; (valof ((`*(\proc`:Z{o}Z)`:Z{o}Z)|pc`:Z{o}Z) Z{o}Z)+(#2`:Z{o}Z)`:Z{o}Z] Z{o}Z)
   |}.
Z>"
              let v2 = "<Z
(* 9.4.2 ANDM *)
Definition ANDM :=
  {| fn_return := void
   ; fn_params := [proc -: `*` typ_struct_SLSH4_Processor; i -: int32]
   ; fn_vars   := [temp -: int32]
   ; fn_body   :=
($ temp`:Z{o}Z) `= (Z{x}Z (call (\Read_Byte`:Z{o}Z)
  E[ (valof ((`*(\proc`:Z{o}Z)`:Z{o}Z)|mmu_ptr`:Z{o}Z) Z{o}Z)
   ;   (Z{x}Z ((`*(\proc`:Z{o}Z)`:Z{o}Z)|GBR`:Z{o}Z) Z{o}Z)
     + (Z{x}Z (\reg`:Z{o}Z) E[\proc`:Z{o}Z; #0`:Z{o}Z] Z{o}Z)`:Z{o}Z] Z{o}Z) Z{o}Z)`:Z{o}Z              ;;
($ temp`:Z{o}Z) `= ((\temp`:Z{o}Z)&((#255`:Z{o}Z)&(Ecast (\i`:Z{o}Z) Z{o}Z)`:Z{o}Z)`:Z{o}Z)`:Z{o}Z  ;;
(Z{x}Z (\Write_Byte`:Z{o}Z) E[(Z{x}Z ((`*(\proc`:Z{o}Z)`:Z{o}Z)|mmu_ptr`:Z{o}Z) Z{o}Z)
   ;   (Z{x}Z ((`*(\proc`:Z{o}Z)`:Z{o}Z)|GBR`:Z{o}Z) Z{o}Z)
     + (Z{x}Z (\reg`:Z{o}Z) E[\proc`:Z{o}Z; #0`:Z{o}Z] Z{o}Z)`:Z{o}Z; \temp`:Z{o}Z ] Z{o}Z)         ;;
(Z{x}Z (\set_pc_raw`:Z{o}Z) E[\proc`:Z{o}Z
   ;   (Z{x}Z ((`*(\proc`:Z{o}Z)`:Z{o}Z)|pc`:Z{o}Z) Z{o}Z)+ (#2`:Z{o}Z)`:Z{o}Z] Z{o}Z)              |}.
Z>"
            end in
footnotesize "(deep embedding) Note: ``{Melt_highlight.Code.latex_of_ppp PPP_full}'' and ``{Code.latex_of_ppp PPP_empty}'' are manually drawn to save spaces in this slide..."
^^
Version.v2)

(* ********************************************************* *)
; B.Abr
  (let phantom_vert = phantom (index "" (footnotesize (texttt "p"))) in
   BatList.map (fun (on_after, msg) ->
     B.Top ("Expressivity of {P.compcert} in practice",
            Label.fact ("Starting from {S.SL.C.compcert},"
                        ^^
                        itemize (BatList.map
                                   (fun x ->
                                     on_after 1 x ^^ newline ^^
                                     on_after 2 "can be approximated as an entire type inference process (by embedding the overall in a dependent type).")
                                   [ "the translation function that produces constructively {S.SL.C.asm}"
                                   ; "the preservation proof built upon {S.SL.C.compcert} and {S.SL.C.asm}" ]))

            ^^ msg))

           [ B.on_after, (B.on_after 3 (Label.warning_ ("In the first type-system, there is no extra warranty that the semantic of {S.SL.C.asm} has been derived from {S.SL.C.compcert}."))
                          ^^
                          B.on_after 4 (Label.fix_ "Besides the first, infer {S.SL.C.compcert} with the last type-system."))
           ; (fun _ x -> x), (Label.problem_ ("Any sound, decidable type system must be incomplete." ^^ phantom_vert))
                             ^^ pause ^^
                             (Label.question_ "{S.SL.C.compcert} is accepted by the first type-system.{newline}Is it accepted or rejected by the last one?") ])

(* ********************************************************* *)
; B.Abr
  (let s = "S" in
   let module SL_p = S.SL_gen (struct let sl = s end) in
   BatList.map
     (fun after ->
       B.Top ("{S.SL.coq} ${overset "?" "="}$ {S.SL.Coq.Deep.compcert}, towards {S.C.lambda_l}",
                 Label.warning_
                   (concat [ "The semantic preservation proof developed in {P.compcert} requires some non-trivial extra hypothesis to be provided:"
                           ; newline
                           ; "<!val compiler : !>{S.P.Coq.Deep.compcert}<! -> !>{langle}{S.P.Coq.Deep.asm}{index_ rangle "monad"}"
                           ; newline
                           ; "<!val proof    : !>{forall}<! behavior, !>{forall}<! P, !>{(*red*) "<!compiler P !>{ne} {index_ bot "monad"}"}<! -> !>"
                           ; newline
                           ; "{red "<!    exec_behaves behavior P!>"}<! -> !>{langle}<!P !>{approx}<! compiler P!>{index_ rangle "monad"}" ])
                 ^^
                 after ))
     [ Label.example
         (itemize
            [ "{blue "<!exec_behaves converge  P!>"} ${overset "<!proof!>" longrightarrow}$ {blue "<!P !>{approx}<! compiler P!>"}"
            ; "{blue "<!exec_behaves diverge   P!>"} ${overset "<!proof!>" longrightarrow}$ {blue "<!P !>{approx}<! compiler P!>"}"
            ; "{blue "<!exec_behaves get_stuck P!>"} ${overset "<!proof!>" longrightarrow}$ {blue "{index_ bot "monad"}"}" ])
     ; Label.problem_
                   "Is there some {red "<!b!>"} such that ``{red "<!exec_behaves b !>{S.SL.Coq.Deep.compcert}"}''?"
                 ^^ pause ^^
                 Label.definition_
                   ("``{red S.C.lambda_l}'': {S.C.asm} sources {texttt s} equipped with these proofs in Coq~:"
                    ^^
                      enumerate [ "{SL_p.Coq.Deep.asm} obtained successfully, i.e. {ne} {index_ bot "monad"}"
                                ; "{exists} <!b!> {in_} \{<!converge!>, <!diverge!>\}, <!exec_behaves b !>{SL_p.Coq.Deep.compcert}" ])])

(* ********************************************************* *)
; B.Abr
  (let l =
     let open English in
     let s =
       "The compilation to {S.C.asm} succeeds. Then to be in {S.C.lambda_l}, the first hypothesis is met."
       ^^
       Label.question_ "What about its behavior, does this program converge, diverge or get stuck?" in
     [ [ yes ; yes   ; yes   ; yes   ; maybe ], Some s
     ; [ yes ; yes   ; yes   ; yes   ; no    ], Some (s
                                                      ^^
                                                      Label.warning_ "The type of ``main'' is not of the form {texttt "unit {rightarrow} int"}. So {S.SL.C.asm} initially goes wrong in {Version.compcert}. {footnotesize "(Proved in {P.coq}.)"}") ] in
   let title = "The limit of {S.C.lambda_l}" in
   let lg = latex_of_int (BatList.length l) in
   BatList.mapi
     (fun pos (h_comment, msg) ->
       B.Top (title (* ^^ " [{latex_of_int (Pervasives.succ pos)}/{lg}]"*),
              sl_intro S.SL.ArmSh.C.asm
              ^^
              "<@@{H_comment h_comment}@
int main(int x) {
  return x;
}
@>"
              ^^
              (match msg with None -> "" | Some s -> s)))
     l)

(* ********************************************************* *)
; B.Abr (
  let f_hline x = Hline :: x in
  let f_vert = [`Vert] in
  let f_id x = x in
  let black = Color.of_int_255 (0, 0, 0) in
  let white = Color.of_int_255 (0xFF, 0xFF, 0xFF) in

  BatList.map
    (fun (hline, vert, id, msg, opt_box) ->
      B.Top
        ("The limit of {S.C.lambda_l}",
         draw_subset
           [space]
           degrad
           [ f_hline, f_vert, f_id, S.C.lambda_l
           ; hline, vert, id, msg
           ; f_hline, f_vert, f_id, S.C.asm
           ; f_hline, f_vert, f_id, S.C.compcert
           ; f_hline, f_vert, f_id, S.C.human ]
         ^^
         (match opt_box with None -> "" | Some s -> s)))
    (let box1, box2, box3 =
       Label.fact "Only programs with no arguments in <!main!> could possibly appear in {S.C.lambda_l}.",
       Label.problem_ "Some higher order {let module SL_p = S.SL_gen (struct let sl = "PROGRAMS" end) in SL_p.C.asm} are outside {S.C.lambda_l}. However some of them could have a sound semantic: {S.SL.C.asm} {notin} {S.C.lambda_l}.",
       Label.fix_ "Design a type system (extending {S.C.lambda_l}) that accepts more functional values in {S.C.asm}." in

     [ (fun x -> Arrayrulecolor (degrad 2) :: Hline :: Arrayrulecolor black :: x), [`Raw (Color.color_ (degrad 2) ^^ vrule)], Pervasives.succ, (Color.colorbox_ (degrad 2) (phantom S.SL.C.asm) ^^ phantom " ?"), Some box1
     ; (fun x -> Arrayrulecolor (degrad 2) :: Hline :: Arrayrulecolor black :: x), [`Raw (Color.color_ (degrad 2) ^^ vrule)], Pervasives.succ, (Color.colorbox_ (Color.of_int_255 (0xFF, 0xFF, 0xFF)) S.SL.C.asm ^^ phantom " ?"), Some box2
     ; f_hline, f_vert, f_id, Color.colorbox_ white S.SL.C.asm ^^ " ?", Some box3 ]) )

(* ********************************************************* *)
; B.Center ("{S.SL.coq} ${overset "?" "="}$ {S.SL.Coq.Deep.compcert}, towards {S.C.infty}",
            let module SL_a = S.SL_gen (struct let sl = "FUN" end) in
            let i_sqcup x = index sqcup (tiny x) in

            "Suppose the pretty-printer is defined as a morphism of categories with {i_sqcup "apply"} and {i_sqcup "APPLY"} as operators:"
            ^^
            (let module SL_p = S.SL_gen (struct let sl = "PROGRAMS" end) in
             align_ "$({SL_p.C.asm}, {i_sqcup "apply"}) {overset "pretty-print" longrightarrow} ({SL_p.Coq.Deep.asm}, {i_sqcup "APPLY"})$")
            ^^
            "Instead of the restricting result: {align_ "{blue "{S.SL.C.asm} {notin} {S.C.lambda_l}"},"}"
            ^^
            (let module SL_a1 = S.SL_gen (struct let sl = index "ARG" (emph "1") end) in
             let module SL_an = S.SL_gen (struct let sl = index "ARG" (emph "n") end) in
             "the problem widens to: {"{forall} {SL_a1.C.asm} {dots} {forall} {SL_an.C.asm}, { align_ "({blue S.SL.C.asm} {i_sqcup "apply"} {SL_a1.C.asm} {dots} {i_sqcup "apply"} {SL_an.C.asm}) {blue "{in_} {S.C.lambda_l}"}?"}"}")
            ^^
            Label.definition_
              ("``{red S.C.infty}'': the smallest set satisfying:"
               ^^
               enumerate (let module S = S_sz (struct let normal = normalsize let footnote = footnotesize let tiny = tiny let color_keyword = Some red_ end) in
                          [ "${S.C.lambda_l} {subseteq} {red S.C.infty}$"
                          ; "{forall} {SL_a.C.asm}, {forall} {S.P.C.infty}, {newline}
                             ({SL_a.C.asm} {i_sqcup "apply"} {S.P.C.infty}) {in_} {red S.C.infty} {longrightarrow_ } {SL_a.C.asm} {in_} {red S.C.infty}" ]))
            ^^ pause ^^
            blue (Label_small.warning "ongoing work!"))

(* ********************************************************* *)
; B.Abr
  (let title = "Future work" in
   [ (let f_hline x = Hline :: x in
      let f_vert = [`Vert] in
      let f_id x = x in
      let white = Color.of_int_255 (0xFF, 0xFF, 0xFF) in
      let colo n = if n <= 1 then degrad n else Color.of_int_255 (let i = 0xEF in i, i, i) in
      B.Center
        (title,
         let sp s =
           let space = space_nb 9 in
           space ^^ s ^^ space in
         let f_conc l = newline ^^ concat (BatList.interleave space l) in
         draw_subset
           []
           colo
           [ f_hline, f_vert, f_id, S.C.lambda_l
           ; f_hline, f_vert, f_id, S.C.infty
           ; f_hline, f_vert, f_id, S.C.asm
           ; f_hline, f_vert, f_id, S.C.compcert
           ; f_hline, f_vert, f_id, S.C.human ]
         ^^
         Label.warning_
           ("Being in {S.C.infty} means firstly that the behavior of {S.P.C.infty} is known: divergence or convergence. What about its correctness:"
            ^^ f_conc [ sp "{S.P.C.asm} {in_} {S.C.infty}" ; "${overset "?" longrightarrow_}$" ; "{S.P.coq} {index_ cong "correct"} {S.P.Coq.Deep.asm}" ])
         ^^
         Label.conjecture_
           ("Once a correctness proof is established, the corresponding progress proof (divergence or convergence) could be automatically generated in {P.coq}:"
            ^^ f_conc [ "{S.P.coq} {index_ cong "correct"} {S.P.Coq.Deep.asm}" ; "{longrightarrow_}" ; sp "{S.P.C.asm} {in_} {S.C.infty}" ]) ))
   ; B.Top
     (title,
      "Towards the correctness of the pseudocode: first steps with the ADC instruction, more investigated in {cite ["shi2011"]}"
      ^^ newline ^^
      (let open Code.Dot in
       let module St = S.SL_gen (struct let sl = "state" end) in
       let dodgerblue = Color.color_name_ (Color.of_int_255 (0x1E, 0x90, 0xFF)) in
"<OO{ Header { shift_x = 1.5
             ; shift_y = -3.
             ; scale = None
             ; node = { n_color = \"darksalmon\" ; shape = Box true }
             ; edge = { e_color = Some dodgerblue ; style = Some \"-triangle 45\" } } }O

comp1 [texlbl="O{B St.Coq.Deep.compcert}O"]
comp2 [texlbl="O{B St.Coq.Deep.compcert}O'"]
coq1 [texlbl="O{B St.coq}O"]
coq2 [texlbl="O{B St.coq}O'"]

{rank=same
  comp1 -> coq1 [label="projection"] }
{rank=same
  comp2 -> coq2 [label="projection"] }

comp1 -> comp2
coq1 -> coq2

O>"))
   ; B.Center
     (title,
      itemize [ "Generalization to the full {S.Pseudocode.ArmSh.Coq.Deep.compcert}."
              ; "Extension to the {S.Decoder.ArmSh.Coq.Deep.compcert} part."
              ; "Membership of {S.SL.C.asm} to {S.C.infty}?"
              ; "As optimization, by generating the {S.Manual.ArmSh.C.human} in {P.coq} instead of {P.ocaml}, does it simplify the correction?" ]) ])

(* ********************************************************* *)
; B.Center ("Thanks",
            let pos = 12 in
            let grey = Color.of_int_255 (let i = 0x52 in i, i, i) in
            concat
              (BatList.map
                 (fun s ->
                   let pre, name = BatString.split s ~by:\" \" in
                   space_nb pos ^^ small (Color.textcolor_ grey (text pre))
                   ^^ newline ^^
                   space_nb (pos + 2) ^^ large (text name)
                   ^^ newline)
                 [ \"Frédéric Blanqui\" ; \"Claude Helmstetter\" ; \"Vania Joloboff\" ; \"Jean-François Monin\" ; \"Xiaomu Shi\" ; \"Frédéric Tuong\" ; \"Yang Yu\" ; \"Daiwei Zhang\" ])
            ^^
            flushright (http \"raweb.inria.fr/rapportsactivite/RA2011/formes\")
)

(* ********************************************************* *)
; B.Allowframebreaks ("", bibliographystyle "alpha" ^^ bibliography "t")

]))
