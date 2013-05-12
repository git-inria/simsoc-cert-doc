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
##verbatim '~' = Code.Ml.verbatim
##verbatim '@' = Code.Humc_.verbatim

module Label =
struct
  include Label

  let simu_sh4, fast_certi, certi_sim, pretty_print, simgendef, oldarm, ctx_compil, th_init_state, concl, appendix_speed, appendix_eta, appendix_singl =
    label (), label (), label (), label (), label (), label (), label (), label (), label (), label (), label (), label ()
end

let () =
  main
    ~packages:[ "xcolor", "table" ]
    ~author:(BatList.map footnotesize
               [ (*st (texttt "frederic.tuong@inria.fr")
               ; *)mail \"tuong@users.gforge.inria.fr\" ])

    (PaperA4
[ abstract "The simulation of Systems-on-Chip (SoC) gains wider acceptance in the area of embedded systems, allowing to test exhaustively the hardware as soon as the prototyping step begins. {P.simsoc} is a simulator firstly optimized on the CPU component (ISS), as this part is a major bottleneck for the simulation speed. But a fast simulator is especially profitable for the debugging activity, if beyond speed it simulates faithfully the ISS. So the {P.simcert} project has formalized in {P.coq} a model of a real CPU: the ARMv6 processor is automatically generated from its reference manual.

However, to evaluate the scalability of this toolkit, we propose to enhance modularly the process behind the generator so that ideally a bundle of processors could be certified at the cost of one. In this report, we generate a well typed version of the SH4 processor's manual, verified by the {P.gcc} and {P.compcert} compilers, and we report our experimentations of using {P.compcert} as a generic platform for proofs in {P.coq} to deeply embed the resulting well typed simulator, both the ARMv6 and SH4 ISS. As a side effect, {P.compcert} being defined in {P.coq}, we develop in {P.coq} a parsing/printing loop from {P.coq} to {P.coq}. Based on a high level front-end as {S.C.compcert}, the goal is to easily mimic the correctness proof being established for one processor (e.g. ARMv6) to others (e.g. SH4), and at long-term, extend the overall to prove the correctness of a given {S.C.compcert} program, that would moreover have ARMv6 and SH4 as certified back-end."
; tableofcontents


(*****************************************************************************)
; section "Introduction"
(* ************************ french *)
(* La stabilité d'un système embarqué repose sur le bon fonctionnement de ses composants. Pour effectuer des tests complets sur l'ensemble, avant la phase de fabrication matérielle, les concepteurs s'intéressent à avoir des logiciels simulant le système, prenant en entré un binaire exécutable quelconque. Le projet {P.simsoc} vise à mettre à la disposition des développeurs un tel simulateur, ce dernier étant organisé en différents composants modulaires. Parmi les composants d'un système, le processeur est un élément important aussi bien lors de sa fabrication que lors de sa simulation. D'une part les erreurs de conception engendrent un coût élevé, et d'autre part la durée d'un programme est liée aux instructions du processeur. Pour obtenir une machine sûre, il y a donc une nécessité de travailler avec les simulateurs. Néanmoins, comment garantir qu'un simulateur se comporte {emph "exactement"} comme une machine ? *)
(* Le but de {P.simcert} est justement de répondre à cette question, en proposant de certifier chacune des parties de {P.simsoc}. S'agissant d'un projet ambitieux et d'envergure, en commençant par la correction du processeur, nous nous concentrons en premier sur le c{oe}ur du simulateur.  *)

(* Plus particulièrement, {P.simcert} contient un simulateur du processeur ARMv6 écrit en Coq. Pour le modéliser et l'intégrer au sein de Coq, il a été nécessaire de construire un modèle formel du processeur, ce modèle nous informe précisément sur la sémantique de chaque instruction.  *)
(* Notre travail s'intéresse à l'importation d'un autre type de processeur au projet {P.simcert} : il s'agit du processeur SH4. Comme pour l'ARM, nous montrons à la section [?] comment obtenir un modèle mathématique du SH4 à partir de son manuel de référence. *)
(* ************************ *)
; "Stability of embedded systems depends on the good behavior of their components. Before reaching the construction of the final material in factory, exhaustive testing is thus a strong requirement. At the same time, designer tends to be interested in software simulating the global system and taking a real binary as input. The goal of the {P.simsoc} project~{cite ["ossc09"]} is to permit engineers to develop systems with such a tool, it contains a simulator split modularly into several components. Among these components, the processor is an important element to consider not only during the production but also for the simulation. On one side, errors of conception are susceptible to have a high economical impact. On the other side, the time execution of an algorithm depends on the processor's optimizations. To get a safe system, the need to work with simulators are then going increasingly. However, how can we guarantee the {emph "exact"} behavior between simulator and embedded systems ?
The goal of {P.simcert} is precisely to answer this question, we propose to certify each part of {P.simsoc}. Being an ambitious project, we plan to begin with the correction of the processor, this targets directly the heart of the simulator.

In particular, {P.simcert} contains a simulator of the ARMv6 written in Coq~{cite ["arm6refman"; "arm"]}. The language Coq is a system which builds a constructive proof from a specification by looking at the programmer's hints~{cite ["Coq:manual"]}. One common application is the extraction procedure which translates the proof, considered as a {lambda}-term, to another functional language like OCaml~{cite ["OCaml"]}. To model and integrate the simulator within Coq, it was necessary to build a formal model of the processor, this model informs us precisely on the semantics of each instruction.
The formal model of the ARMv6 has been automatically generated. To prove that the {P.simcert} framework is completely modular, our work focuses on the automatic importation of another type of processor into the {P.simcert} project : the SH4 processor~{cite ["sh4refman"]}.

As for ARM, we show at section~{ref_ Label.simu_sh4} how to automatically generate a mathematical model of SH from his reference manual. Section~{ref_ Label.fast_certi} presents our first step into the certification problem. Our work begins now with some general definitions in section~{ref_ Label.certi_sim}.
"

(*****************************************************************************)
; section ~label:Label.certi_sim "Certified simulation"
; "
This section introduces the concept of software simulation by explaining how simulations work and what informations we need to import from the constructor manual. For the following, we restrict our attention to the simulation of the processor as it is the first and existing component studied in {P.simcert}. We also present the current state of {P.simsoc} and {P.simcert}, and explain at the end the principle and goal of the {P.compcert} compiler.

{Label.notation_ "
For the following, we will use the symbols
  {itemize
      [ "{S.C.human} to designate an arbitrary sequence of character, "
      ; "{S.C.gcc} for programs compiled successfully with {Version.gcc}~{cite ["gcc"]} (tested at least one time),"
      ; "{S.C.compcert} for programs for which a generated {P.compcert} C file~{cite ["Leroy-Compcert-CACM"]} can be successfully dumped (tested at least one time), in general before the end of the compilation. For a demonstration with {Version.compcert}, see the option <!-dc!>." ]
  }
"}

"
; subsection "Principle of a processor simulator"
(* ************************ french *)
(* Les spécifications du SH4 sont fournies dans un document disponible au format PDF et le fonctionnement de chaque instruction est décrit en langage naturel. Notre but est de construire un modèle mathématique du processeur à partir de cette documentation, un modèle qui soit le plus proche possible de la sémantique informelle fournie par le constructeur. *)

(* Pour l'ARM, un travail similaire a été accompli en extrayant les informations du manuel de référence avec un programme, et après une phase de correction préliminaire sur la syntaxe. *)

(* De manière générale, un simulateur de processeur a essentiellement besoin de deux informations pour fonctionner. *)
(* {itemize *)
(* ; La première est celle décrivant le comportement de chaque instruction, appelé ``pseudocode''. Chaque instruction du processeur (ADD, CPY, MOV ...) est précisément composé d'une séquence de primitives bas niveau : copie ou affectation de registres, opérations d'accès à la mémoire... Parfois, le manuel de référence précise que le comportement n'est pas défini ou incertain dans certaines situations (``unpredictable'' pour l'ARM, ``Appendix B'' pour SH4). *)
(* ; La deuxième information dirige la phase du ``décodage'' des instructions. Étant donné un mot binaire de taille fixe, il s'agit de retrouver à partir de ce mot l'instruction qui lui est associée (pour l'exécuter), et quels sont éventuellement ses arguments. À priori, il pourrait y avoir une ambiguïté sur la recherche d'une instruction correspondant à un mot donné, on pourrait trouver plusieurs instructions candidates pour un mot. L'ordre des instructions à tester en premier a également une importance. Normalement, les indications disponibles dans les manuels sont suffisament clairs pour éviter ces ambiguïtés. *)
(* %Chaque instruction s'accompagnant d'un motif prédéfini dans le manuel, cette recherche s'effectue en les testant un par un jusqu'à l'obtention d'un succès. Cependant, l'ordre des tests peut être important *)
(* %Dans le manuel de référence, ces indications s'obtiennent facilement, car rassemblées sous la forme d'un tableau pour chaque instruction. *)
(* } *)
(* ************************ *)


; "Generally, a processor simulator basically needs two informations in order to accomplish its task.
{itemize
[ "The ``pseudocode'' describes the behavior of each instruction. Each processor instructions (add, cpy, mov ...) are precisely composed of sequences of low-level primitives : copy, affectation of register or access operation to the memory... The unknown or uncertainty behaviors in some situations are in general stated in the reference {S.Manual.ArmSh.C.human} (for example, ``UNPREDICTABLE'' case in instruction for the ARM, the same is described in Appendix B for the SH). In any case, specified or not, when importing the semantics to some formal model, we will be forced to explicitly specify the meaning of all the unknown behavior."
; "The ``decoder'' pilots the association from binary word to instruction. Given a binary word of fixed size, the goal is to retrieve from this word the instruction associated to the word, in order to execute it, as well as his potential arguments.
The correction of the decoder is as important as the pseudocode for a good simulation. We want for example to be sure that there is only one instruction associated to a word." ]
}
Providing these two informations, we are able to model a simple simulator in Coq{let module S = Sfoot in footnote () "Coq being a strongly normalizing language, the simulation is performed within a fixed number of recursive step."}. This is exactly what the {P.simcert} project contains.
"
; subsection "The {P.simcert} project"
; "
The goal of {P.simcert}~{let module S = Sfoot in footnote () "{English.newrelease}~{cite ["urlsscert"]}."} is to certify {P.simsoc}~{cite ["arm"]}, because its large part is written in C++~{cite ["ossc09"]} and we require more safety about the C++ compiler we are using. Hopefully, for the processor simulator, it is possible to abstract most of the surrounding C++ features. We call then {S.SL.C.gcc} the {S.C.gcc} part restricted to the component which simulates the processor. Because we are currently in the same unpredictable situation with a {S.C.gcc} program, we want to have a formal proof that our {S.C.gcc} code behaves correctly. But what does ``formal proof'' mean in case we are comparing a virtual software and a real hardware ? Even if the hardware is produced with the help of computers, its specification depends directly on the format of documentation the constructor gives. For our subject of processor certification, the processor documentation is generally available in raw text. For example, specifications of the SH4 are published in a PDF document and inside, the behavior of each instruction are described in the {S.C.human} language. Our goal is to build a mathematical model of the processor from the processor documentation, one which is as closer as possible to the one furnished by the constructor. Then, understanding this definition of ``formal proof'', the {S.C.gcc} code behind {S.SL.C.gcc} needs to behave correctly with respect to the semantic of this mathematical model. Note that to be strict, because the {S.Manual.ArmSh.C.human} can contains some erroneous specifications, further validation tests with the real hardware will remain to be performed in the final end, in order to obtain an observationally equivalent behavior with the simulator.
"
; paragraph "Related work"
; "
A formalization of the ARMv7 processor is reported in {cite ["conf/itp/FoxM10"]}. The ARMv7 model has been constructed manually using the proof assistant HOL4, with monad as combinator for semantic functions.
We are interested to automatically generate a Coq model, which aim to be as understandable as possible to the {S.Manual.ArmSh.C.human} of reference. We will show later how coercions and notations in Coq are advantages for readability, but we will also notice some limitations of their use for a large proof project. Organizing the project modularly in Coq will also permit us to integrate simultaneously ARMv6 and SH4 in a generic simulator, and gain an easier interaction with {P.compcert}.
"
; subsubsection ~label:Label.simgendef "The simulator {S.SL.coq} {forceline}and the toolkit {S.simgen}"
; "
Now, before considering the SH, let us look at what we already have in {P.simsoc} and {P.simcert}. On one side, there is {S.SL.C.gcc}. On the other side, a mathematical model in Coq had automatically been built for the ARM~{cite ["arm"]} and thus a special simulator has especially been created to integrate as well as to test the model : {S.SL.coq}.
The behavior of {S.SL.C.gcc} and {S.SL.coq}, which have both been manually designed, is intentionally the same : for compiling, these two simulators respectively need the {S.Manual.Arm.C.gcc} and {S.Manual.Arm.coq} specification{let module S = Sfoot in footnote () "As for {S.SL.C.gcc} and {S.SL.coq}, we will indifferently write ``{S.Manual.ArmSh.C.gcc}'' and ``{S.Manual.ArmSh.coq}'' when the processor we implicitly think can be ARMv6 or SH4. Note that before~{ref_ Label.simu_sh4}, the processor we think is only ARMv6."}.

Therefore, {S.simgen}, which is also part of {P.simcert}, has especially been created for the importation of the {S.Manual.Arm.C.human}.
To illustrate this, we present here {S.Manual.Arm.coq}, which contains {S.Pseudocode.Arm.coq} and {S.Decoder.Arm.coq}, both automatically generated by the toolkit.
"
; subsubsection ~label:Label.oldarm "The generated {S.Pseudocode.Arm.coq}"

; "
This file contains the semantics of all the instructions written in the Coq language. Each instruction from the {S.Manual.Arm.C.human} of reference is translated in a similar Coq function, called ``<!..._step!>'' function.
Furthermore, as for modularity the generation of the {S.Decoder.Arm.coq} is currently done in a separate way than the {S.Pseudocode.Arm.coq}, we create an inductive type <!inst!> containing exactly a constructor for each ``<!..._step!>'' function. <!inst!> will be useful for interoperability between {S.Pseudocode.Arm.coq} and {S.Decoder.Arm.coq}. At the end of {S.Pseudocode.Arm.coq}, there is a main function <!step!> which role is, given a value of type <!inst!>, to retrieve the corresponding ``<!..._step!>'' function to run it.

Instructions from the ARMv6 are a bit special. Given a fixed sized word, we usually think about the instruction corresponding to this word, in particular during the decoding phase. But in ARMv6, we need to parameterized our thought one step higher. Indeed, some instructions take as argument a special parameter. This parameter is intended to be specially treated and a preliminary function need to be applied before the real execution of the instruction. This typical case is the {emph "addressing mode"}.

Because the {S.Manual.Arm.C.human} contains roughly 150~instructions (and about 75 special THUMB instructions), we will sketch an example with just 2 instructions : <!A4.1.21 LDM (2)!> and <!A4.1.81 SMLSLD!>. Without entering deeply in details, the reader is invited to have a look at the Coq code (more details in {cite ["urlsscert"]}) or to jump directly to the next section.
"
; paragraph "The addressing mode case"
; subparagraph "Type"
; "
According to the {S.Manual.Arm.C.human}, there are 5 special modes.
<#
Inductive mode1 : Type := #{PPP}#.
Inductive mode2 : Type := #{PPP}#.
Inductive mode3 : Type := #{PPP}#.
Inductive mode4 : Type :=
  | M4_Incr_after         #{PPP}#
  | M4_Incr_before        #{PPP}#
  | M4_Decr_after         #{PPP}#
  | M4_Decr_before        #{PPP}#.
Inductive mode5 : Type := #{PPP}#.
#>(to simplify, we have replaced some non-relevant informations by ``<!!{PPP}!!>'')
"
; subparagraph "Definition"
; "
For each constructor regrouped above, an executing function associated is furnished.
<#
(* A5.4.2 Load and Store Multiple - Increment after *)
Definition M4_Incr_after_step s0 W cond n r #{PPP}# : result * word * word :=
  let start_address := (reg_content s0 n) in
  let end_address := (sub #{PPP}#) in
  let r := block (
    (fun loc b st => if_then #{PPP}#
      (fun loc b st => set_reg n (add #{PPP}#) loc b st) loc b st) ::
    nil) nil true s0 in
    (r, start_address, end_address).
#>
"
; subparagraph "Correspondence between the type and definition"
; "
Finally, this simple part illustrates the symmetry between type and definitions.
<#
Definition mode1_step (s0 : state) (m : mode1) := #{PPP}#.
Definition mode2_step (s0 : state) (m : mode2) := #{PPP}#.
Definition mode3_step (s0 : state) (m : mode3) := #{PPP}#.
Definition mode4_step (s0 : state) (m : mode4) :=
  match m with
    | M4_Incr_after W c n r => M4_Incr_after_step s0 W c n r
    | M4_Incr_before W c n r => M4_Incr_before_step s0 W c n r
    | M4_Decr_after W c n r => M4_Decr_after_step s0 W c n r
    | M4_Decr_before W c n r => M4_Decr_before_step s0 W c n r
  end.
Definition mode5_step (s0 : state) (m : mode5) := #{PPP}#.
#>
Note that along the pattern matching, the state <!s0!> is always present at the right hand side. Even if semantically we understand this as a monadic construction~{cite ["peyton-jones-wadler-93" ; "peyton-jones-tackling-09"]}, we will see in the SH part how to rewrite the whole to explicitly hide <!s0!>.
"
; paragraph "The instruction case"
; "
Here, the same structure as the {emph "addressing mode"} is done for the {emph "instruction"} case, i.e. we define a structure for type, definitions, and at the end, we relate them with a main correspondence function.
"
; subparagraph "Type"
; "
<#
Inductive inst : Type :=
#{PPP}#
  | LDM2 (m_ : mode4) (cond : opcode) (register_list : word)
#{PPP}#
  | SMLSLD (X : bool) (cond : opcode)
      (dHi : regnum) (dLo : regnum) (m : regnum) (s : regnum)
#{PPP}#.
#>
Notice the additional parameter {texttt "m_"} of {texttt "LDM2"}, which contains the specification of the addressing mode.
"
; subparagraph "Definitions"
; "
<#
(* A4.1.21 LDM (2) *)
Definition LDM2_step (s0 : state) cond r s #{PPP}# : result :=
  if_then (ConditionPassed s0 cond)
    (fun loc b st => block (
      (fun loc b st => update_loc n0 (*address*) s loc b st) ::
      (fun loc b st => loop 0 n14 (fun i =>
        if_then (zeq (r[i]) 1)
          (fun loc b st => block (
            (fun loc b st => #{PPP}# (read st (#{PPP}# loc) Word) loc b st) ::
            (fun loc b st => #{PPP}# (add (#{PPP}# loc) (repr 4)) loc b st) ::
            nil) loc b st)) loc b st) ::
      nil) loc b st) nil true s0.
#{PPP}#
(* A4.1.81 SMLSLD *)
Definition SMLSLD_step (s0 : state) X cond dHi dLo m s #{PPP}# : result :=
  if_then (ConditionPassed s0 cond)
    (fun loc b st => block (
      (fun loc b st => update_loc n1 (*operand2*)
        (if zeq X 1 then Rotate_Right #{PPP}# else reg_content s0 s) loc b st) ::
      (fun loc b st => update_loc64 n0 (*accvalue*)
        (or64 (#{PPP}# ((*ZeroExtend*)(reg_content st dHi))) #{PPP}#) loc b st) ::
      (fun loc b st => update_loc n2 (*product1*)
        (mul #{PPP}# (get_signed_half0 (get_loc n1 (*operand2*) loc))) loc b st) ::
      (fun loc b st => update_loc n3 (*product2*)
        (mul #{PPP}# (get_signed_half1 (get_loc n1 (*operand2*) loc))) loc b st) ::
      (fun loc b st => update_loc64 n4 (*result*) (sub64 (add64
          (get_loc64 n0 (*accvalue*) loc)
          (get_loc n2 (*product1*) loc))
        (get_loc n3 (*product2*) loc)) loc b st) ::
      (fun loc b st => set_reg dLo (get_lo (get_loc64 n4 (*result*) loc)) loc b st) ::
      (fun loc b st => #{PPP}# (get_hi (get_loc64 n4 (*result*) loc)) loc b st) ::
      nil) loc b st) nil true s0.
#>
"
; subparagraph "Correspondence between the type and definition"
; "
<#
Definition step (s0 : state) (i : inst) : result :=
  match i with
#{PPP}#
    | LDM2 m_ cond r =>
      match mode4_step s0 m_ with (r, s, end_address) =>
        match r with
          | Ok _ _ s1 => LDM2_step s1 cond r s
          | _ => r
        end
      end
#{PPP}#
    | SMLSLD X cond dHi dLo m s => SMLSLD_step s0 X cond dHi dLo m s
#{PPP}#
  end.
#>
In contrast with {texttt "SMLSLD_step"}, the execution of {texttt "LDM2_step"} is preceded by an extra call to {texttt "mode4_step"}.
"
; subsubsection "The generated {S.Decoder.Arm.coq}"
; "
The structure of this file is rather simple. Given a word, we decompose it into a list of raw bits, then a pattern matching is applied to this list. The bits in each clause comes directly from the {S.Manual.Arm.C.human}, as well as the instruction associated to each list.
"
; subsection "The {P.compcert} project"
; "
The goal of {P.compcert} is precisely to transform most of the {S.C.human} programs to an assembler program, with a semantic preservation certificate.

Generally, if the compiler can produce an assembler file from a {S.C.human} program, the well behavior at runtime of the assembler produced depends on two facts :
{itemize
[ "how the generated assembler file is interpreted,"
; "and the well behavior of the program at {S.C.compcert} production time (as well as a heuristically{let module S = Sfoot in footnote () "Starting from {S.C.gcc}, there is currently no Coq proof of semantic preservation to {S.C.compcert}
. " (*, because*)} good translation from {S.C.human} to {S.C.compcert})." ]
}
The first point is feasible as supposed~{cite ["Leroy-Compcert-CACM"]}. Indeed, the section~{ref_ Label.fast_certi} will explain how such kind of interpretation or simulation could safely be performed.
About the generation of a formal model, one could also agree that a manual formalization is hard or unexciting. Here begins the automatic generation of the SH4 model.
"
(*****************************************************************************)

; section ~label:Label.simu_sh4 "Simulation of the SH4"
; "
The integration of SH4 in {P.simcert} follows the same algorithm as ARMv6. The first step is to transform the raw {S.Manual.Sh.C.human} into a more structured type. For the ARM, the transformation from the {S.Manual.Arm.C.human} to Coq has precisely crossed a quite structured abstract syntax tree, named ``{S.simgen_ast}''{let module S = Sfoot in footnote () "The {S.simgen_ast} is approximatively formed with 400 OCaml words."}. Because the ARM generation to Coq is done from {S.simgen_ast}, we plan to fix it as our target for SH. Consequently, we hope the algorithm generating to Coq can be reused rapidly for the SH.

{hspace (`Ex 1.)}

Besides the target fixed at this advanced {S.simgen_ast}, we show at the next part how the parsing of the {S.Manual.Sh.C.human} is performed, as well as another intermediate type which is finally used before getting {S.simgen_ast}.
"
; subsection "Parsing of the {S.Manual.Sh.C.human}"
; "
(* ************************ french *)
(* Le manuel SH4 contient au total environ 450 pages, la partie où se trouve les informations correspondant au pseudocode et au décodeur occupe une place assez importante, près de la moitié du fichier. Construire directement à la main un modèle en Coq est donc long ou avec risques possibles d'erreurs. De plus, les informations à importer sont à première vue organisées de façon régulière, et il semble donc accessible de les traiter automatiquement avec un programme. *)
(* ************************ *)
The {S.Manual.Sh.C.human} totals about 450 pages, informations corresponding to the pseudocode and decoder occupy an important part, approximately the half of these pages. Building directly a model at hand in Coq is thus long or with a non negligible risk of errors.
Furthermore, while reading it briefly, we have been having a strong conjecture that the {S.C.human} code specified in the instructions can be easily translated to {S.C.gcc}. To experiment our intuition, we have planned to use the FrontC package as type representing our futur {S.C.gcc} program in OCaml.
{Label.notation_ "
Let us name
{itemize
[ "{S.CP.frontc} for programs parsed successfully with the OCaml package {texttt "FrontC"} {Version.frontc} (tested at least one time). For a demonstration, check if the result of the function <!Frontc.parse_file!> is ``<!PARSING_OK _!>''." ]}
"}
Then, starting from the {S.Manual.Sh.C.human} considered as a string, the first step was to write the smallest patch possible in order to get a first well-typed {S.CP.frontc} AST in OCaml (surrounded by the other extra informations, such as for the decoder...).

The parsing of the decoder informations was merely fast, except for the <!9.33 FLDI1!> instruction : it was needed to introduce a new <!PR!> column as what is done for the most of floating instructions. Besides this case, bits to decode are clearly presented in an array. We explains now the process done for the instructions.

"
; paragraph "Patching phase"
; "
Interesting informations in the {S.Manual.Sh.C.human} are located at ``section 9, Instruction Descriptions''. It is formed by a preliminary header containing functions only specific to floating instructions, followed by a sequence of description of the instructions.

Corrections needed to bring to the {S.Manual.Sh.C.human} are essentially composed of missing symbol ``;'', missing type informations before some parameters and unbounded value encountered.
    "
; paragraph "More confidence by type-checking"
; "
Besides the obtaining of the {S.CP.frontc} AST, we think that all the {S.C.human} code behind each instructions are predestined to be ultimately executed. Hence, we can extend our test protocol by compiling them with {P.compcert}.
In fact, the program behind the {P.compcert} parsing step is taken from the CIL library (with some minor modifications)~{cite ["necula"]}. The CIL library includes itself a modified version of the {S.CP.frontc} files. Because our goal is to remove as soon as possible the most errors from {S.Manual.Sh.C.human}, it is then interesting to iterate the parse-checking among all these tools.
{Label.notation_ "
We introduce
{itemize
[ "{S.CP.cil} for programs parsed successfully with the OCaml package {texttt "cil"} {Version.cil} (tested at least one time). For a demonstration, check if the result of the function <!Frontc.parse!> returns a value of type <!file!> without raising an exception."
(*; "{S.cparserC} for programs parsed successfully with the {Version.compcert} parsing step (tested at least one time). For a demonstration, check if the result of the function <!Cparser.Parser.file!> returns a value of type <!definition list!> without raising an exception."*) ]
}
"}
"
; paragraph "Summary"
; "
For clarity, results can now be classified in two parts, we present first modifications needed to bring in order to obtain a {S.CP.frontc} and {S.CP.cil} AST, and secondly modifications performed to get a successful compilation with {P.compcert}.
"
; subparagraph "Corrections needed to be accepted in {S.CP.frontc}, {S.CP.cil}"
; description
  (let ppp = texttt (Code.latex_of_ppp PPP) in
   [ "Wrapping comments", "Some {S.C.human} code includes sentences commonly found from the english language. Everytime the situation occurs, we use our intuition to place <!/* !{PPP}! */!> around the appropriate part."
   ; "Character repetition", "For example, some parenthesis are opened twice, some are closed twice, and some are swapped or missed. In general, the cost performed is just one single addition or one single deletion."
   ; "Character mapping", "We replaced for example some wrong ``,'' by <!;!>. Note that the variable ``H'00000100'' contains an illegal character for a variable, in contrast to this accepted form : <!H_00000100!>."
   ; "Reduced syntax", "For a function taking more than one argument, it is frequent to encounter this reduced declaration : ``f (int x, y) \{{ppp}\}''. So, we expand the type at every argument position : <!f (int x, int y) {!{PPP}!}!>.

Every patterns matching " ^^ forceline ^^
"``case($value$) \{ $pattern_1$ : {ppp} $pattern_n$ : {ppp} \}'' have been replaced by " ^^ forceline ^^
"<!switch(!>$value$<!) { case !>$pattern_1$<! : !{PPP}! case !>$pattern_n$<! : !{PPP}! }!>."
   ; "Wrong overloading form", "A call to <!data_type_of!> is performed in <!9.28 FCNVDS!>, which seems to refer to the <!data_type_of!> defined at the beginning of section 9. However the called function takes one more argument than the defined. This situation has been handled by renaming the called <!data_type_of!> into a new function, which goal is to call the defined <!data_type_of!> with the right arguments. In the same spirit, we renamed <!FSUB!> to <!FSUB_!> in its pseudocode part because <!FSUB!> is in fact a directive already defined : <!#define FSUB !{PPP}!!>"
; "PDF translation errors", "The extracted text from a PDF document introduces some repetitive background side effects : every encoded string ``–='' (representing the OCaml string <!"\xE2\x80\x93="!>) needs to be replaced by a simple <!-=!>. Space separated words ``normal_ fcnvds'' are merged to form a single word <!normal_fcnvds!>..."
; "Arithmetical notations", "The mathematical expression ``4j'' is understood as <!4 * j!>."
 ])
; subparagraph "Corrections needed to be accepted by {P.compcert}"
; "
The typing process from the CIL AST to {S.C.compcert} has actually permitted us to correct new errors."
; description
  [ "Unbound value", "This case occurs frequently. For example, the function {texttt "LDCRn_BANK"} in the part <!9.50 LDC!> uses the unbound variable {texttt "Rn_BANK"}.  After a search, we finally found that it is a meta abbreviation for {texttt "R"}$n${texttt "_BANK"}, with $n {in_} [|0;7|]$, as described in the function's comment. Then this special expansion of $n$ has been specially handled by our SH4 parser."
  ; "Type error", "We have discovered some new misleading types (for instance we introduce manually <!bool_of_word!> and <!nat_of_word!> in the definition of <!FPSCR_PR!> and <!FPSCR_FR!>)."
  ; "Typing of bit fields", "From {P.compcert}, ``{texttt "the type of a bit field must be an integer type no bigger than 'int'"}''. In particular, the {S.Manual.Sh.C.human} contains a part behaving like this rejected program :
<@@{let open English in H_comment [ yes ; yes ; no ] }@
struct S { unsigned long i:1 }
main () {}
@> To correct that, it suffices to rename every wrong <!unsigned long!> to <!int!> for example."
  ; "Explicit prototype declaration", "It becomes mandatory to specify the type annotation of undefined functions (e.g. the <!9.91 SLEEP!> instruction use the unbound <!Sleep_standby!> function name)."
  ; "Mutual recursive function disabled", "The order of function declarations has an importance : like OCaml, permutations are not allowed for functions not specified as recursive. This problem has been resolved by automatically rearranging the order at top-level of SH functions. {itemize [ "For instructions, every list of declarations in every instructions has been generated in reversed form." ; "For the beginning of section 9, the structure is not as linear as for the instructions. In this case, we give a topological sequence describing the order to respect. Then it suffices to write a general sorting algorithm following this indication." ]}"
 ]
(*paragraph "existe-t-il des modifications pour ce cas : pas de 'return' dans le cas de type fonction renvoyant 'void' ? spécifique à simlight ?"*)
; th_same_source [ S.Manual.Sh.C.gcc ; S.Manual.Sh.C.compcert ]

; subsection "Grouping SH4 and ARMv6 into a common pseudocode generator"
; "
The next step after the obtaining of an OCaml value representing the {S.Manual.Sh.C.human} is its integration in {P.simcert}. To this end, we have performed two modifications :
{itemize
[ "at the {S.SL.coq} source by adding the model of the SH4 processor (model of the state, particular numbering of register, semantics of remaining undefined functions...),"
; "at {S.simgen} to generate the {S.Manual.Sh.coq} in the same way as what is done for ARMv6." ]
}
The goal was not only to accomplish these tasks but also to fusion the changes with ARMv6 using modules and functors, to permit future integration of processors.

{hspace (`Ex 1.)}

Generating the Coq code was rather long but easy, because a lot of factorization was necessary. We give briefly the modifications required in {S.simgen_ast}.
{itemize
[ "Unlike the ARM, parameters of each SH instruction are clearly specified in the pseudocode. We then modified {S.simgen_ast} to support these argument informations."
; "In term of semantics, there is a specific constructor, namely <!return!>, presents in the SH pseudocode. It has been represented in the semantic of the Coq code using monadic form."
; "The default case for the <!switch!> option is also present in several SH instructions, then the necessary changes in {S.simgen_ast} has been done." ]
}"
(* Replace_all ("&&", "&"), p [ 1065 ] (* Sh4_Inst.v is not Coq well typed otherwise *) *)
; Label.fact "
Besides some minor modifications, the existing framework generating the {S.Manual.Arm.coq} can completely be used in the same way to produce the {S.Manual.Sh.coq}."

; subsubsection "{lambda}-optimization of the {S.Manual.ArmSh.coq}"
; "
In section~{ref_ Label.oldarm}, we have seen that the body of the function <!LDM2_step!> and <!SMLSLD_step!> are explicitly informative. Indeed, the words <!fun loc b st!> are for example repeated a lot. However, the purpose of the variable <!st!> is to carry the transformation on the state, acting like an accumulator. <!b!> and <!loc!> are also accumulators but we remark that <!b!> is just used at a higher function call level, not inside each instruction. So we want to abstract all of them now. Initially, the code present for the ARM was designed and really thought using monadic specifications, but the current shape is currently not entirely satisfactory. Then we think instead, to simplify these accumulators by hiding them at the cost of rearranging the arguments of several functions, in particular by putting all the accumulators at the end.

Additionally, we also notice that the <!loc!> variable, used to model some local stack for variable, is used frequently even for variable which is affected only once. Indeed, we are attempted to replace them by a native Coq ``<!let!>'' construct when needed.

{hspace (`Ex 1.)}

We will explain later that our goal is to prove the well-defined behavior of the Coq simulator, as well as the {S.C.gcc} simulator. Modifications on the Coq code can thus be considered as superfluous, except for precisely readability of the proof we plan to do. In particular, it is close to some form of compilation to a factorized {lambda}-code. Without going formally, we are interested to minimize, as possible as it can be, the number of node representing the Coq AST for simplifying the future proof (for example, among the optimizations cited above, one can imagine transforming a
{texttt ("if {Code.latex_of_ppp PPP} then f f_true else f f_false")} into a {texttt ("f (if {Code.latex_of_ppp PPP} then f_true else f_false)") }).

{hspace (`Ex 1.)}

Here comes the file obtained after performing some simplifications and rewriting to monadic style. We present the ARMv6 file rather than the SH4 because its {S.Manual.Arm.C.human} contains rich examples. In any case, modifications affect both outputs in the same manner (for more details, the reader is invited to see both generated files~{cite ["urlsscert"]}).
"
; paragraph "The new {S.Pseudocode.Arm.coq}"
; "
Simplifications are done inside the semantic definition and the correspondence type-definition, in particular declaration for types remains the same.

{hspace (`Ex 1.)}

We assume below
{itemize
[ "having defined a notation for vectors as
<#
  Notation "{{ a ; .. ; b }}" :=
    (Vcons _ a _ .. (Vcons _ b _ (Vnil _)) ..).
#>
(see the Coq library <!Bvector!>). In particular, we rely on the type-checker to guess the <!nat!> associated to each element."
; "having these notations for semantic functions
<#
  Notation "'<.' loc '.>' A" := (_get_loc (fun loc => A))
    (at level 200, A at level 100, loc ident).
  Notation "'<' st '>' A" := (_get_st (fun st => A))
    (at level 200, A at level 100, st ident).
  Notation "'<:' loc st ':>' A" := (<.loc.> <st> A)
    (at level 200, A at level 100, loc ident, st ident).
  Notation "'do_then' A ; B" := (next A B)
    (at level 200, A at level 100, B at level 200).
#>
<!_get_loc!> and <!_get_st!> are two new monadic expressions which give access respectively to the accumulator <!loc!> and <!st!> in their body.
<!next!> is like the <!bind!> operator~{cite ["peyton-jones-tackling-09"]} except that the communicating value is ignored."
; "having a function <!_ret!> which is like the <!return!> combinator~{cite ["peyton-jones-tackling-09"]} except that it also returns <!true!> as a couple with the default value." ]
}
"
; subparagraph "(addressing mode) Definition"
; "
<#
(* A5.4.2 Load and Store Multiple - Increment after *)
Definition M4_Incr_after_step W cond n r #{PPP}# : semfun _ := <s0>
  let start_address := reg_content s0 n in
  let end_address := sub #{PPP}# in
  do_then [ if_then #{PPP}#
       (set_reg n (add #{PPP}#)) ];
  ret_ {{ start_address ; end_address }}.
#>
Intuitively, each ``mode <!..._step!>'' function returns a monadic vector (<!semfun!> being the monadic type) because we want later to perform some uniform treatment for each function and their type need then to be unifiable. This is illustrated below with the introduction of <!mode_step!>.
"
; subparagraph "(addressing mode) Correspondence type-definition"
; "
<#
Definition mode1_step (m : mode1) : _ -> semfun unit := mode_step #{PPP}#.
#>
<!mode_step!> is a kind of initialization function. In particular, the local stack for local variable are initialized to be empty here. <!mode_step!> is precisely used in <!mode1_step!>, <!mode2_step!>, {dots}, <!mode5_step!>.

"
; subparagraph "(instruction) Definitions"
; "
<#
(* A4.1.21 LDM (2) *)
Definition LDM2_step cond r s #{PPP}# : semfun _ := <s0>
  if_then (ConditionPassed s0 cond)
    ([ update_loc n0 (*address*) s
    ; loop 0 n14 (fun i =>
         if_then (zeq (r[i]) 1)
           ([ <:loc st:> #{PPP}# (read st (#{PPP}# loc) Word)
           ; <.loc.> #{PPP}# (add (#{PPP}# loc) (repr 4)) ])) ]).
#{PPP}#
(* A4.1.81 SMLSLD *)
Definition SMLSLD_step X cond dHi dLo m s : semfun _ := <s0>
  if_then (ConditionPassed s0 cond)
    ([ <st> let operand2 :=
              if zeq X 1 then Rotate_Right #{PPP}# else reg_content s0 s in
    let accvalue := or64 (#{PPP}# ((*ZeroExtend*)(reg_content st dHi))) #{PPP}# in
    let product1 := mul #{PPP}# (get_signed_half0 operand2) in
    let product2 := mul #{PPP}# (get_signed_half1 operand2) in
    let result := sub64 (add64 accvalue product1) product2 in
    [ set_reg dLo (get_lo result)
      ; <st> #{PPP}# (get_hi result) ]]).
#>
As for <!M4_Incr_after_step!>, the state parameter <!st!> is encapsulated in the monadic type <!semfun!>. The underscore in the return type <!semfun _!> is present not only to let the type-checker deduce the real answer but also the part corresponding to the generation of this type information in {S.simgen} are merged for <!M4_Incr_after_step!> and all the <!..._step!>. In particular, the type behind the <!_!> is not necessarily the same in the both cases.

In the body, {S.simgen} detects automatically the use of <!st!> or <!loc!>. Hence it adds the corresponding notation when needed.
"
; subparagraph "(instruction) Correspondence type-definition"
; "
<#
Definition step (i : inst) : semfun unit :=
  do_then conjure_up_true;
  match i with
#{PPP}#
    | LDM2 m_ cond r => mode4_step m_ (fun s end_adr => LDM2_step cond r s)
#{PPP}#
    | SMLSLD X cond dHi dLo m s => SMLSLD_step X cond dHi dLo m s
#{PPP}#
  end.
#>
The parameter <!st!> having been moved to the end, we observe a certain symmetry in the shape of correspondence between definition and type. Here, the symmetry is clearly highlighted in all the ``mode <!..._step!>'' function and even in the above <!step!>, except for specific instructions such as <!LDM2!> where a specific treatment for addressing mode remains to be done. However, the <!mode4_step!> call becomes simplified compared to the previous <!step!> in section~{ref_ Label.oldarm}.
"
; subsubsection "Discussions"
; "

"
; paragraph "Limit of the Coq generation"
; "
The packing system of module as a first-class value in {Version.ocaml} encourages us to organize rapidly the code for ARMv6 and SH4. The counterpart is the explicit typing required of the module contents.

The typing information becomes also mandatory in Coq, when sometimes we need to explicitly redefine by hand some record, especially when {English.coqkernel}. Remark that writing annotations in Coq is more complex than writing annotations in OCaml. For example, we present here a warning where some explicit type annotations need to be manipulated carefully. The datatype modeling the exception in the ARMv6 processor has been defined as follows :
<#
(* ARM *)
Inductive exception : Type :=
  Reset | UndIns | SoftInt | ImpAbort | PFAbort | DataAbort | IRQ | FIQ.
#>
At the SH4 part, we began to copy this definition (with the hope to extend with further constructors later) :
<#
(* SH *)
Inductive exception : Type :=
  UndIns.
#>
However, contrarily as the ARMv6, the OCaml compilation of the extracted SH4 code fails. This is mainly due to a subtlety in the Coq type-checking :
{itemize
[ "<!exception!> has type <!Prop!> in the SH case, whereas it has type <!Set!> in the ARM case. More details about the minimality solution of typing rules of inductive declarations are given in the Coq manual {cite ["Coq:manual"]}. In both cases, ``<!Check exception : Type.!>'' succeeds because of the {index le English.bdiz} convertibility rule."
; "In Coq, it exists modules in which the extraction must completely keep the proofs (like our SH4 problem). In the current {Version.coq}, the extraction wrongly removes the maximum of propositions in modules. This leads to wrong OCaml files and there is no options at extraction time to avoid this." ]
}
The appendix {ref_ Label.appendix_singl} contains more explanations.

{hspace (`Ex 1.)}

We can wonder if the {S.Manual.ArmSh.coq} we generate can be written in a total monadic style~{cite ["conf/itp/FoxM10"]}. For example, the <!if_then!> constructor can become a <!if_then!> monadic combinator, as well as the several encountered <!ConditionPassed!>. Then we hope one can rewrite {forceline}
``<!<s0> if_then (ConditionPassed s0 cond) !{PPP}!!>'' as {forceline}
 ``<!if_then (ConditionPassed_s0 cond) !{PPP}!!>'' annihilating the need to use <!_get_loc!> and <!_get_st!>{let module S = Sfoot in footnote () "Because the first call to {texttt "_get_st"} at the start affects the variable {texttt "s0"}, this variable needs to be treated carefully, in a monadic reference for example, if we delete {texttt "_get_st"}."}. However we are in front of a difficulty. Indeed, the automatically generated {S.Manual.ArmSh.coq} uses extensively the coercion facility, for instance to consider a boolean value as a word (or integer)~{cite ["arm"]}. Hence, the Coq code is finally very close to the original {S.Manual.ArmSh.C.gcc}. Because sometimes the <!st!> is deeply embedded in arithmetical expressions, we
have a problem of coercion at higher-order level. Currently an
expression like
<!zeq 1 true!> is implicitly translated to {forceline}<!zeq 1 (Z_of_bool true)!>
with <!zeq : Z -> Z -> bool!>.
If now, we have a <!zeq_!> of type <!semfun Z -> semfun Z -> semfun bool!>,
this is not well-typed :
<!zeq_ (ret 1) (ret true)!>,
unless we give explicitly the hint annotation
<!zeq_ (ret (1 : Z)) (ret (true : Z))!>.
Note that for readability we can also introduce a notation and write
something similar to {forceline}<!zeq_ { 1 } { true }!>.

Because the final goal of the {S.Manual.ArmSh.coq} is also the proof, we can now wonder if it is finally convenient to perform some proof with notations rather than concrete definitions.
"

; paragraph "Performance"
; "
Currently, the generation of the {S.Manual.Sh.coq} is complete except for floating and MMU instructions. {S.Manual.Arm.coq} contains 148 instruction cases (without THUMB) and 204 for the SH4 (by considering also the expanding of the {texttt "R$n$_BANK"}) but the {S.Pseudocode.Sh.coq} size occupies only 2/3 of {S.Pseudocode.Arm.coq}. The SH output for instructions is very simple because it does not contain precomputation for addressing mode. Consequently, the correspondence between type and definition is completely symmetric, for example we can easily delete the type and replace them by their equivalent ``<!..._step!>'' function.

The compilation of {S.Decoder.Sh.coq} is long~: about 30s on a recent computer while for {S.Decoder.Arm.coq} we succeed after 4s on the same machine. Its time is spent mostly on the pattern-matching for retrieving an instruction given a word, the non redundancy detection may be a possible cause of the long analysis. In particular, by performing some sorting on the clause, we get a better compilation time.

{hspace (`Ex 1.)}

In any case, these results is susceptible to be changed, because we mainly need to be sure that the SH model is correct with respect to the hardware, so further modifications on the model is susceptible to be brought.

"
; paragraph "Correction of the simplification"
; "
Inside each instructions in the {S.Manual.ArmSh.C.human}, we suspect there is at most one access to modify the environment per line. It concerns the storing of value inside memory or to the local stack <!loc!>. We have also encountered several read of mutable variable in one line, which does not seem to conflict with storing at the same time. Because the {S.Manual.ArmSh.coq} may changed in the future (if we discover some erroneous specifications), the current correctness guarantee relies first on its good type-checking.
Note that for SH4, there is a specific instruction <!Read_Byte!> to access a particular location. We found convenient to set the type of this function to a monadic one, i.e. <!word -> semfun word!> as well as its companion <!Write_Byte!>. Because <!Read_Byte!> can be present several times in a line, it was necessary to coat it with an explicit call to <!bind!>. Then the computation ``sequentially'' continues inside the body of <!bind!>.

About the correction of our simplification process of the accumulator <!st!>, <!loc!> and <!b!>,
{itemize
[ "we can add more <!_get_loc!> or <!_get_st!> at any positions (accepting at least a well-typed monadic expression), because we already know there is no conflict between these names and other variable names coming from the {S.Manual.ArmSh.coq}, thanks to the good type-checking of the previous {S.Manual.ArmSh.coq}. Moreover, we also know that any variable (generally leaving in the {lambda}-calculus theory) is captured by the nearest abstraction."
; "If we add less <!_get_loc!> or <!_get_st!> at any positions (requiring at least one), the semantics can wrongly changed. Because the new {S.Manual.ArmSh.coq} type-checks correctly for ARM and SH, if for example we add less <!_get_loc!> at a certain position, then it means necessarily that the <!loc!> used refers wrongly to an external <!_get_loc!>." ]
}
Hence, in any case, we hope the test validation of the {S.Manual.ArmSh.coq} will conclude the discussion. However, by seeing the success of the current validation tests for the ARMv6 (by comparing the output of {S.SL.C.gcc} and {S.SL.coq}) before the simplification and after, we have a good confidence about its accuracy. We are now going to develop the problem of certification of the {S.SL.C.gcc} simulator, the {S.Manual.ArmSh.C.gcc}, and their validation.

(*****************************************************************************)
"
; section ~label:Label.fast_certi "Towards a ``fast'' certified simulator"
; "
Recall that our objective is to guarantee the correction of any program simulated by {S.SL.C.gcc}, written in ARMv6 or SH4, and the requirement of the same state of registers for {S.SL.C.gcc} and {S.SL.coq} after each instructions.

We plan to prove this in Coq, because the sources of {S.SL.coq} are already and directly accessible in Coq. Thus, first we need to get a Coq representation of a {S.C.gcc} code, in particular to be able to work further with the {S.SL.C.gcc}.
Because the {S.SL.coq} is Coq well-typed, we already know its termination (after any supposed number of step given). Hence, it just remains to show that its behavior is coherent with respect to {S.SL.C.gcc}, that there is some correspondence between {S.SL.coq} and {S.SL.C.gcc}. Indeed, the equivalence proof we plan to write will contain implicitly the termination property for every instruction simulated by {S.SL.C.gcc}, as this last aim to mimic {S.SL.coq}.

Now, for representing the {S.SL.C.gcc} code in Coq and reason with, we think about using the {S.C.compcert} type, because we suspect that its {S.C.gcc} code can be easily transformed to {S.C.compcert}. Above all, the {emph "not wrongly"} behavior (in the sense of~{cite ["Leroy-Compcert-CACM"]}) of {S.SL.C.gcc} implies the obtaining of a {emph "not wrongly"} assembler file, observationally equivalent to its source, thanks to the {P.compcert} semantic preservation. The {S.C.compcert} AST is also the first AST in the {P.compcert} translation chain where the {P.compcert} proof begins, and is thus the closest Coq AST to any {S.C.gcc}.

{hspace (`Ex 1.)}

Globally, we can approximate the {S.Manual.ArmSh.coq} as a {emph "shallow embedding"} because by definition, instructions in {S.Manual.ArmSh.coq} are generated using constructors belonging to the native Coq language. Similarly, the Coq value obtained from the {S.C.gcc} source by {P.compcert} is view as a {emph "deep embedding"}, because this time, the information about the instructions are build in Coq with the {S.C.compcert} constructor, the {S.C.compcert} AST having been defined in Coq. The main goal is then to prove the equivalence between the {emph "deep"} and {emph "shallow"} embedding.

We have explained this concept of embedding with the {S.Manual.ArmSh.coq}, but it is not specially limited to, we plan indeed to generalize for the whole simulator. As the {S.C.compcert} type is defined in Coq, the first task is, given a {S.C.gcc} file, to find a way to parse this {S.C.gcc} file in Coq. As first application, we could then import {S.SL.C.gcc} to Coq, and thus we will be ready to start the correspondence proof.

{hspace (`Ex 1.)}

This leads us to the section explaining how such a parser can be constructed, and more precisely, constructively realized.


"
; subsection ~label:Label.pretty_print "A Coq parser for {S.C.compcert}"
; "
Our goal is to import a {S.C.gcc} code in Coq via the {S.C.compcert} AST. Assume we have an arbitrary {S.P.C.compcert} program that we need to work with a parsed representation in Coq during the development process of the proof. Such parser could naturally be written in a Turing complete point of view. But Coq is a language where the input and output with {English.outworld} can not easily be done in its proper language." (*let module S = Sfoot in footnote () "Enriching Gallina with new tactics implies to modify the source of Coq."*)
; " At the same time, a program well-typed by the Coq type system can be extracted in a more complete programming language to interact easier with {English.outworld}. Because the language chosen for the extraction of {P.compcert} is OCaml, one can modify the sources of the extracted program by adding an OCaml pretty-printer, which target language is Coq ; this in order to get back the value inhabiting the {S.C.compcert} AST of {S.P.C.compcert}. But to be less dependent of the extracting algorithm (which can perform some renaming on variables), as well as the target language of extraction (which may change in the future), we plan to redact the most achievable part of the pretty printer in Coq. Then, we will be closer to the {S.C.compcert} AST and furthermore, we will have the possibility to evolve in a rich dependent type system framework.
"
; subsubsection "The algorithm of parsing and pretty-printing"
; "
In our case, the origin and the target language of the printer are Coq. The type we plan to export{let module S = Sfoot in footnote () "``export'' or ``import'', depending the view we have as the origin language is the target language. Moreover, the origin language is so close to the target that the ``parsing'' notion is a synonym of ``pretty-printing'' here."}, <!AST.program fundef type!> is formed with a lot of constructors. To illustrate this with a simple example, assume we have defined the following type, inspired from the sources of {P.compcert}~:
<#
Inductive floatsize : Type :=
  | F32 : floatsize
  | F64 : floatsize.

Inductive type : Type :=
  | Tvoid : type
  | Tfloat : floatsize -> type
  | Tfunction : typelist -> type -> type
with typelist : Type :=
  | Tnil : typelist
  | Tcons : type -> typelist -> typelist.

Definition ident := positive.

Record program (A : Type) : Type := mkprogram {
  prog_funct : list (ident * A);
  prog_main : ident
}.

Definition ast := program type.
#>
For each declarations $ty$ above, we would like to have the following printers defined in Coq~:
<#
Check _floatsize : floatsize -> s.
Check _type : type -> s.
Check _typelist : typelist -> s.
Check _ident : ident -> s.
Check _program : forall A, (A -> s) -> program A -> s.
Check _ast : ast -> s.
#>
they transform $ty$ into a simple abstract datatype <!s!>, where <!s!> can be thought as a type similar to the well-known $string$, found in Coq or OCaml.

Dependent types are precisely useful for performing a uniform operation on all the constructor, namely by allowing us to abstract the arity of the constructor we are folding. Indeed, when seeing the declarations above, our first will is to literally copy-paste them to produce these declarations :
<#
Definition _floatsize := __floatsize
  | "F32"
  | "F64".

Definition _type_ T (ty : #{PPP}#) := ty _ _floatsize
  | "Tvoid"
  | "Tfloat"
  | "Tfunction"

  | "Tnil"
  | "Tcons".
  Definition _type := _type_ _ (@__type).
  Definition _typelist := _type_ _ (@__typelist).

Definition _ident := _positive.

Definition _program {A} #{PPP}# := @__program #{PPP}#
  { "prog_funct" ; "prog_main" }.

Definition _ast := _program _type.
#>

This can hopefully be done under the condition of having defined <!_INDUCTIVE!> and <!_RECORD!> as~:
<#
  Notation "A ** n" := (A ^^ n --> A) (at level 29) : type_scope.

Check _INDUCTIVE : string -> forall n, s ** n.
  Notation "| x" := (_INDUCTIVE x _) (at level 9).

Check _RECORD : forall n, vector string n -> s ** n.
  Notation "{ a ; .. ; b }" :=
    (_RECORD _ (Vcons _ a _ .. (Vcons _ b _ (Vnil _)) ..)).
#>
where the type <!vector!> and ``<!_ ^^  _ --> _!>'' are respectively more deeply explained in the Coq libraries <!Bvector!> and <!NaryFunctions!>.
Note that the function <!_RECORD!> can be implemented using only <!_INDUCTIVE!>. Hence, our combinators of pretty-printing are all simulated by only one, here <!_INDUCTIVE!>~
{let module S = Sfoot in footnote () "In the same spirit, a {texttt "Record"} construction is basically seen as a notation for an {texttt "Inductive"} construction~{cite ["Coq:manual"]}." (* FIXME lien vers la théorie des inductifs plus précis *)
}.

The last ambiguity to resolve is the meaning of <!__floatsize!>, <!__type!>, <!__typelist!> and <!__program!>. In fact, it suffices us to rename them as the function <!floatsize_rect!>, ..., <!program_rect!>, where all the ``<!..._rect!>'' functions are already furnished by Coq everytime a type is defined. However, for mutually recursive defined type such as <!type!> and <!typelist!>, we can use a function which fold completely the structure and also perform the mutually call explicitly (see the <!Scheme!> command for this purpose). This justifies why we have regrouped above the constructors of <!type!> and <!typelist!> together in a unique folding function <!_type_!>.

Finally, as <!_INDUCTIVE!> and <!_RECORD!> return the function type ``<!_ ** _!>'', we need to enhance the type of our recursors <!__floatsize!>, ..., <!__program!> by a more powerful one, and at least {emph "convertible"} to its initial type (the ``convertibility'' relation as defined in~{cite ["Coq:manual"]}). It means that we need to write explicitly their type~:
<#
  Notation "A [ a ; .. ; b ] -> C" :=
    (A ** a -> .. (A ** b -> C) ..) (at level 90).

Definition __floatsize {A} : A [ 0   (* F32       :           _ *)
                               ; 0 ] (* F64       :           _ *)
                             -> _ := #{PPP}#.
Definition _type_ A B (f : _ -> Type) := f (
                             A [ 0   (* Tvoid     :           _ *)
                               ; 1   (* Tfloat    : _ ->      _ *)
                               ; 2   (* Tfunction : _ -> _ -> _ *)

                               ; 0   (* Tnil      :           _ *)
                               ; 2 ] (* Tcons     : _ -> _ -> _ *)
                             -> B).
Definition __type {A} : _type_ A _ (fun ty => _ -> ty) := #{PPP}#.
Definition __typelist {A} : _type_ A _ (fun ty => _ -> ty) := #{PPP}#.
#>
Each number corresponds exactly to the arity attended by the respective constructor.
All the informations present until now are sufficient for the type-checker to automatically deduce the remaining type. With only these declarations, our Coq library of pretty-printing considered as a functor is finished, its instantiation by a module and the definition of <!_INDUCTIVE!> being a secondary task.
"
; (let module SL_p = S.SL_gen (struct let sl = "PROGRAM" end) in
Label.fact "
The pretty-printer defined can be used to parse an arbitrary {SL_p.C.compcert} to a Coq representation. For the rest, this associated deep embedded Coq program will be named as : {SL_p.Coq.Deep.compcert}.
")
; subsubsection "Programming discussions"
; "
"
; paragraph "Explicit annotations"
; "
The counterpart of using this kind of simplified way for our printer (i.e. using the dependently form as {forceline}
<!| "Tvoid" | "Tfloat" | "Tfunction" | "Tnil" | "Tcons"!>) is highlighted by the necessity to explicitly mention the arity of each constructor. This can constitute a notable inconvenient, but in the absence of annotations, the type reconstruction becomes undecidable.
The other alternative would be to give the constructor as a more normal form ``<!_ -> ... -> _!>''. However, the real <!AST.program fundef type!> contains approximatively a hundred of constructors and specifying the type with a single number has rather been a good compromise.
"
; paragraph "Monadic embedding"
; "
Above, we have described <!s!> as a type similar to $string$. In fact, <!s!> is considered abstractly during the folding of each recursors, except for <!_INDUCTIVE!> which needs to manipulate it. Therefore, we can define it as $"<!s!>" := t~{alpha}$ where $t$ is a monadic type and ${alpha}$ the usual value a monadic type carry with. <!_INDUCTIVE!> being instantiated in a module separated from the pretty-printing functor, the integration remains easy.

Note that as a first consequence, because <!_INDUCTIVE!> is the pretty-printing foundation of every constructor, we can embed inside the monadic type some extra informations, like the current indentation position depending on the depth. Moreover, the basic datatype <!s!>, initially considered as a $string$ can now for example be replaced by a monadic buffer for efficiency.

"
; paragraph "Automation versus maintainability"
; "
The process of creating a raw printing function given a type may be automated and integrated in the Coq sources. However, in the case the value we wish to import is defined with the help of the Gallina language, there may be some difficulty to print exactly the tactics used, as well as the Coq comments <!(* ... *)!>. Hopefully, for our task of importing a {S.C.gcc} data, this is not a problem because the value is rawly taken from {English.outworld}.

The {S.C.compcert} AST contains a lot of type information at each node (for example, every constructor of <!Csyntax.expr!> carries a field of type <!Csyntax.type!>), so the representation of the value printed is rather expressive. Our actual pretty-printer includes a sharing algorithm where types are first collected and declared separately with the Coq ``<!Definition!>''.
Before this optimization, the {S.Manual.Arm.Coq.Deep.compcert} size were about {Version.Size.Slv6_iss.Old.with_indent}M. Now, it is approximatively {Version.Size.Slv6_iss.New.facto_coq}M, and the type-checking time is thus clearly improved.

Because natural numbers are also frequently used, it can be more readable to print them using 0-9 digits, instead of their raw inductive constructor definition. Remark that if the number we print is too big, like some position of memory location, OCaml raises a <!Stack_overflow!> with byte-code or <!Segmentation fault!> in native version{let module S = Sfoot in footnote () "see ``{emph "11.5 Compatibility with the bytecode compiler"}'' in {cite ["OCaml"]}. In the same spirit, when we try to display a large number with the {Version.coq} pretty-printer, like {texttt "Check 5001."}, if it can print the number, it prints with a warning before."}. This problem can hopefully be solved by abstracting the printing process for every failing type with a tail recursive function, the whole being instantiated with the extracted code at OCaml side.

With the extraction procedure, we have also encountered an extra {eta}-reduction automatically performed, which gives a not well-typed OCaml code. This is resolved by manually {eta}-expanding the offending term or using a more recent version of Coq where this problem has just been fixed (appendix {ref_ Label.appendix_eta}).

"
; subsection "The creation of {S.SL.C.asm}"
; "
We have mentioned at the beginning that one motivation of choosing {S.C.compcert}, as type for our work in Coq, is precisely the {P.compcert} certification proof leading to assembler.

Thus, we are attentive at the result of every compilation steps of {S.SL.C.gcc} with {P.compcert}, this includes the success to obtain a {S.C.compcert} code as well as, at the end, an assembly file.

{Label.notation_ "
For the following, let us introduce
{itemize
    [ "{S.C.asm} for programs for which a generated assembly file can be successfully dumped (tested at least one time), in general at the end of the compilation. For a demonstration with {Version.compcert}, see the option <!-dasm!>." ]
}
"}
{let module SL_p = S.SL_gen (struct let sl = "PROGRAM" end) in
Label.fact "Because the internal processing {texttt "f"} going from a {S.C.compcert} representation to a {S.C.asm} representation in {P.compcert} is written in Coq, we can name {SL_p.Coq.Deep.asm} the mapping of {texttt "f"} to the {SL_p.Coq.Deep.compcert} associated to an initial {S.C.asm}.

Therefore, we now have a way to parse an arbitrary {S.C.asm} file to Coq.
"}
"
; subsubsection ~label:Label.ctx_compil "The context of compilation : 32 vs 64 bits"
; "
By definition {S.SL.C.gcc} is GCC well-compiled, but until now, we have not precised the type of the machine used during compilation, e.g. a 32 or 64 bits processor, as well as the type of the processor that the binary will be run on, e.g. again a 32 or 64 bits (this last option can be chosen at the invocation of GCC). Hopefully, after at least four attempts, we found the same success of compilation of {S.SL.C.gcc} on any combination of 32 or 64 bits machine, and, 32 or 64 bits processor for the target{let module S = Sfoot in footnote () "Among the bits processor, there are of course others important characteristics describing a computer, for example, are we compiling on/for a PowerPC, IA32 or ARM machine~? Without giving complex details, we just precise that the success of these four attempts has been found on a particular same processor."}.

Like GCC, {P.compcert} also allows us to customize the target of processor for the executable. Unlike GCC, no default choice is provided in the case the target is manually not specified, this choice becomes mandatory in {P.compcert}. By looking at the architecture of our own 32 and 64 bits computers, among the possibilities available by {P.compcert}, we opt to set {texttt Version.compcert_arch} first, with the hope to extend to other processors after. But since here, cares must be taken because this simple choice can have a non-trivial influence on proofs. In particular, this {S.C.gcc} program~:
<@@{let open English in H_comment [ yes ; yes ; no ; no ]}@
#include <inttypes.h>

void main() {
  int64_t x = 0;
}
@>
is rejected by {P.compcert} (if we still target the {texttt Version.compcert_arch} processor), because 64 bits data-structures are not fully supported yet{let module S = Sfoot in footnote () "with the current {Version.compcert}"}, and this behavior does not depend on the 32 or 64 bits machine {P.compcert} is running.

Hence, a new problem is coming : we realize that the {S.Manual.Arm.C.gcc} uses frequently 64 bits data-structures (as shown in the {texttt "A4.1.81 SMLSLD"} instruction). Then so does the whole {S.SL.C.gcc}, which is, precisely at this moment, rejected by {P.compcert}.

"
; subsubsection "Is {S.SL.C.gcc} well-typed ?"

; paragraph "Going to {S.C.compcert} to obtain {S.SL.C.compcert}"
; "
We remarked curiously that the previous {S.C.gcc} program is {P.compcert} well accepted when the processor target is fixed to <!arm-linux!>, and such, on a 64 bits computer only, not on a 32 bits. It means that the good support for 64 bits data-structures depends on the computer used and the processor target fixed. However, it may be surprising because the big part of the compiler is issued from Coq, a deterministic environment. Where is the error~? In fact, there is not : starting from a {S.C.gcc} code, the heuristic performed to retrieve a {S.C.compcert} code includes an external call to a preprocessor, namely ``<!gcc -E!>''. As long as it terminates, this call is correct because by definition, the heuristic uses its proper algorithm to transform the most {S.C.human} to a {S.C.compcert} program. Like the GCC compiler, the behavior of <!gcc -E!> depends on the options <!-m32!> and <!-m64!>, which targets the processor for the executable. If absent, <!gcc -E!> considers by default the processor currently settled. In fact, in the case of {texttt Version.compcert_arch}, the extra option <!-m32!> is present everytime.
Note that for <!arm-linux!>, no options are specified, hence the previously success exhibited earlier on 64 bits is now explained.

"
; paragraph "Going to {S.C.asm} to obtain {S.SL.C.asm}"
; "
Once a {S.C.compcert} value is obtained, it is sure that the compiling process leading to assembler will terminate (this part being from Coq). However in this case, termination is not always a good result. In particular, the chain leading to assembler is written in monadic style. Types checking{let module S = Sfoot in footnote () "We can roughly approximate the Coq conversion from {S.C.compcert} AST to the assembler as a typing process. In particular, it may seem feasible to embed this translation in a dependent type."} are done on the fly during the conversion. When the checks fail, an exception is then returned (as terminating value).

Hopefully, in case of errors, we are clearly informed, i.e. events occur sequentially in this order : a monadic error is returned, no assembly outputs are produced, the {S.C.compcert} value is not useful (if existing and dumped) as well as the Coq value pretty-printed from it, and {S.SL.C.gcc} needs to be corrected.
"
; paragraph "Conclusion"
; "
"
; subparagraph "Type-checking"
; "
To force the compilation of {S.SL.C.gcc} by {P.compcert} to have a deterministic behavior, one which is independent of the machine used and to obtain an assembly file, which is here a synonym of well consistency checking, we present two possible solutions.
{itemize
[ "``Keep the program {S.SL.C.gcc}, Modify the environment {P.compcert}''{newline}
For the {texttt Version.compcert_arch} target, inspired from <!arm-linux!>, we tried randomly to change the heuristic from <!-m32!> to the explicit <!-m64!> in the preprocessing stage. Then the compilation successfully terminates ! On 32 and 64 bits computer, we can generate a 32 bits assembly file.
"
; "``Modify the program {S.SL.C.gcc}, Keep the environment {P.compcert}''{newline}
Here we replace, in the generated {S.Manual.Arm.C.gcc} as well as the whole {S.SL.C.gcc}, every 64 bits type by a record containing two 32 bits field. Usual arithmetical operations on 64 bits are then simulated in 32 bits.

Because 32 bits data-structures are supported, the compilation process terminates.
However, contrarily to the previous solution, it requires here to activate in {P.compcert} the emulation of assignment between structs or unions (see the option <!-fstruct-assign!>){let module S = Sfoot in footnote () "Because the activation of this option affects the {P.compcert} heuristic, we can finally wonder if the environment has really been kept !"}." ]
}

Remark that on both solutions, the option <!-fno-longlong!> can be set because ``<!long long!>'' types are not used.

Recall that {S.SL.Arm.C.gcc} includes initially the generated {S.Manual.Arm.C.gcc}. Because now it compiles correctly with {P.compcert}, it also means that the generated {S.Manual.Arm.C.gcc} is in fact a {S.C.asm} source.
"
; subparagraph "Validation tests"
; "
After the modification performed above, we now have a single program, called {S.SL.C.gcc} or {S.SL.C.asm} (depending on the speaking context), compiling with target fixed at 32 and 64 bits. However to be able to think about {S.SL.C.gcc} as a more close synonym for {S.SL.C.asm}, we need at least to study their behavior at runtime.

We observed unfortunately that unlike {ref_ Label.ctx_compil}, there exist some tests which fail now. In fact, even if the problem of 64 bits data-structures is resolved at compilation time, some arithmetical operations using 32 bits data-structures can have a not expected behavior at execution time.
More precisely, by examining closely the situation, we remarked for instance that this {S.C.human} program~:
<@@{let open English in H_comment (BatList.init 4 (fun _ -> yes))}@
#include <stdio.h>

void main() {
  int i = 32;
  printf("line 1 %lx\n", 1lu <<  i);
  printf("line 2 %lx\n", 1lu << 32);
}
@>
which in fact belongs to the {S.C.gcc} and {S.C.asm} class of programs, has two surprising different behaviors by considering the executables respectively produced by GCC and {P.compcert} (both compiling on/for {texttt Version.compcert_arch}). Indeed, we have the following results, depending on the compiler used~:{ newline ^^

texttt (tabular (interl 4 `L)
    [ Data [ "" ; "gcc -m64" ; "gcc -m32 -O0{textrm ","}" ; "gcc -m32 -O{textrm "$n$ ($n {in_} [ {texttt "1"} ; {texttt "2"} ; {texttt "3"} ]$)"}" ]
    ; Data [ "" ; "" ; "{textrm P.compcert}" ; "" (* FIXME can we delete this element ? *) ]
    ; Hline
    ; Data [ "line 1" ; "100000000" ; "1" ; "0" ]
    ; Data [ "line 2" ; "100000000" ; "0" ; "0" ] ]) ^^ newline }

(in OCaml, <!Int32.shift_left 1_l 32!> evaluates to <!1_l!>).

Remark that initially, starting from this {S.C.human} code included in {P.simsoc} and {P.simcert}~:
<@@{let open English in H_comment (BatList.init 4 (fun _ -> yes))}@
#include <stdio.h>

void main() {
  int i = 32;
  printf("line 1 %lx\n", (1lu <<  i) - 1);
  printf("line 2 %lx\n", (1lu << 32) - 1);
}
@>
we wanted to obtain <!ffffffff!> everywhere (this was the previous behavior we had in {ref_ Label.ctx_compil} leading to success on validation tests) and is clearly not the results expected~:{newline ^^

texttt (tabular (interl 3 `L)
    [ Data [ "" ; "gcc -m64{textrm ","}" ; "gcc -m32 -O0{textrm ","}" ]
    ; Data [ "" ; "gcc -m32 -O{textrm "$n$ ($n {in_} [ {texttt "1"} ; {texttt "2"} ; {texttt "3"} ]$)"}" ; "{textrm P.compcert}" ]
    ; Hline
    ; Data [ "line 1" ; "ffffffff" ; "0" ]
    ; Data [ "line 2" ; "ffffffff" ; "ffffffff" ] ]) ^^ newline}

Finally, we have fixed this error to get <!ffffffff!> everywhere : this problem using 32 bits data-structures can be easily avoided by using explicitly the deterministic aforementioned operations on 64 bits data-structures, instead of 32.

Now, validation tests succeed on both {S.SL.C.gcc} and {S.SL.C.asm}." (* Except this 64 bits data-structures not supported in {P.compcert}, we have not encountered others difficult problems during the compilation.*)
; th_same_source [ S.SL.C.gcc ; S.SL.C.compcert ; S.SL.C.asm ]

; subsection "The behavior of {S.SL.C.asm}, towards {S.SL.C.infty}"
; "
"
; subsubsection ~label:Label.th_init_state "Does {S.SL.C.asm} terminate ?"
; "
We are now one step closer to invocate the main {P.compcert} theorem, which predicts completely the behavior of the assembly file produced from {S.SL.C.asm}.
{itemize
[ "Because {S.SL.C.asm} has in fact been existentially produced, it means the compilation process has lead to a successful monadic value. This is a condition needed first by the main {P.compcert} theorem. Due to the success to get an assembly file, we conjecture this condition is easy to prove in Coq, in particular using some ``<!vm_compute!>''."
; "Besides that hypothesis, the main {P.compcert} theorem takes precisely as parameter the behavior we estimate for the source. Indeed, it is our task to give a bet on a behavior $b$ the initial {S.SL.C.asm} has (at {S.C.compcert} AST production time), and to show that its execution really follows $b$. Then, if $b$ is not classified in particular as a {emph "wrong"} behavior, {P.compcert} will answer us that the assembly executable has surely the behavior $b$." ]
}

Remark that some reorganizations have been done in the last version of {P.compcert}. Before {Version.compcert}, the main theorem of correction needs to take exactly these two hypothesis before proceeding further. In {Version.compcert}, the idea remains the same but more lemmas are provided to help proving that the {S.C.compcert} program will {emph "progress safely"} at each steps. In particular, if we suppose the evaluation being deterministic, analyzing the {S.C.compcert} program within the big-step semantic suffices to deduce the behavior of the assembly. To simply outline our reasoning, we will approximate in the following the ``{emph "progress safety"}'' notion by ``{emph "not wrong"}'' and will not emphasize too much on the evaluation strategy.

{let s = "S" in
let module SL_p = S.SL_gen (struct let sl = s end) in
Label.definition_ "
We define :
{itemize
[ "{S.C.lambda_l} for {S.C.asm} sources {texttt s} equipped with these proofs in Coq~:" ^^
  enumerate [ "the associated {SL_p.Coq.Deep.asm} has been obtained successfully,"
          ; "the behavior of the {SL_p.Coq.Deep.compcert} is {emph "not wrong"}." ]
(*" can be successfully transformed to an assembly file with a certified compiler preserving its original semantic (and preserving at least from the {S.C.compcert} big-step semantic). Moreover, the behavior of the initial source is required to be proved {emph "not wrong"}."*)
; "{SL_p.Coq.Deep.lambda_l} will naturally stand for the {SL_p.Coq.Deep.asm} (if {SL_p.C.asm} {in_} {S.C.lambda_l})." ]
}
"}
Now enriched by this definition, can we apply the main CompCert theorem to {S.SL.C.asm} ? Is the {S.SL.C.asm} a {S.C.lambda_l} program ? After careful considerations, we think to answer negatively here. Indeed, a {S.C.lambda_l} program is in particular close with regard to its initial environment : it can not receive arguments from the outside world.

{Label.example "
This {S.C.asm} code is not a {S.C.lambda_l} program :
<@@{let open English in H_comment (BatList.flatten [ BatList.init 4 (fun _ -> yes) ; [ no ] ])}@
int main(int _) {
  return 0;
}
@>
because the type of the main function (called from {English.outworld}) is not of the form {texttt "unit {rightarrow} int"}. Thus it initially goes wrong by definition.
"}
We conclude straightforwardly~:
{Label.fact "
The {S.SL.C.asm} is not a {S.C.lambda_l} program because as a simulator, its task is to simulate an arbitrary assembly file given in input.

More clearly~:
<#
Lemma no_initial_state :
  forall s, ~ Csem.initial_state asmC_simlight s.

  Proof.
    intuition. inversion H.
    vm_compute in H1. inversion H1. subst b.
    vm_compute in H2. inversion H2. subst f.
    vm_compute in H3. inversion H3.
  Qed.

Proposition wrong_behavior :
  program_behaves (Csem.semantics asmC_simlight) (Goes_wrong E0).

  Proof.
    apply program_goes_initially_wrong ;
    apply no_initial_state.
  Qed.
#>
(Note that, except ``<!asmC_simlight!>'' which designates {S.SL.C.asm} and has been produced by our pretty-printer, every others free variables are more explained in the original source of CompCert.)
(* see in CompCert /common/Behaviors.v *)
"}
This result appears unsatisfactory. In particular, the classification made by the {S.C.lambda_l} group restricts a bit the general notion of correctness one could have for an arbitrary {S.C.human} program.
{
let module SL_p = S.SL_gen (struct let sl = "PROGRAMS" end) in
let module SL_a = S.SL_gen (struct let sl = "ASM" end) in
let i_sqcup x = index sqcup (tiny x) in
"Assume the pretty-printer <!parse!> (just defined in {ref_ Label.pretty_print}) being a morphism preserving the applicative operator ``${sqcup}$'' : {align_ "$({SL_p.C.asm}, {i_sqcup "apply"}) {overset "<!parse!>" longrightarrow} ({SL_p.Coq.Deep.asm}, {i_sqcup "APPLY"})$"}
Then, instead of searching if {S.SL.C.asm} {in_} {S.C.lambda_l}, one could determine if
{align_ "{forall} {SL_a.C.asm}, ({S.SL.C.asm} {i_sqcup "apply"} {SL_a.C.asm}) {in_} {S.C.lambda_l} "}"
}

{let module SL_a = S.SL_gen (struct let sl = "FUN" end) in
let i_sqcup x = index sqcup (tiny x) in
Label.definition_ "
We present here
{itemize
[ "{S.C.infty} being the smallest set satisfying these properties~:" ^^
enumerate [ "${S.C.lambda_l} {subseteq} {S.C.infty}$"
          ; "{forall} {SL_a.C.asm}, {S.P.C.infty}, {newline}
               ({SL_a.C.asm} {i_sqcup "apply"} {S.P.C.infty}) {in_} {S.C.infty}
               {longrightarrow_ }
               {SL_a.C.asm} {in_} {S.C.infty}"
           ]
; "{S.P.Coq.Deep.infty} is introduced as a synonym of {S.P.Coq.Deep.asm} (if {S.P.C.asm} {in_} {S.C.infty})." ]
}
"}

"
; paragraph "Is {S.C.infty} empty ?"
; "
Because the membership of {S.SL.C.asm} to {S.C.infty} depends at least on the shape of {S.C.lambda_l}, it is interesting to check if we can first exhibit an element from {S.C.lambda_l}, as this element will belong to {S.C.infty}. So, we tried to prove that it contains at least this simple {S.C.asm} program, called {S.P.C.asm}~:
<@@{let open English in H_comment (BatList.flatten [ BatList.init 4 (fun _ -> yes) ; [ maybe ] ])}@
int main() {
  return 0;
}
@>"
; subparagraph "Towards the membership of {S.P.C.asm} in {S.C.lambda_l}"
; "Here is the proof that {S.P.C.compcert} terminates, with a specific input-output traces and exit number~:
<#
Proposition source_terminates :
  { trace : _ & { result : _ &
  Cstrategy.bigstep_program_terminates program_c trace result } }.
    Proof. #{PPP}# Qed.
Definition trace := projT1 source_terminates.
Definition result := projT1 (projT2 source_terminates).
    Require Import #{PPP}#.
#>"
; "As soon as the assembly file can be obtained successfully, we can predict that it behaves as the original {S.P.C.compcert}~:
<#
Proposition asm_of_c :
  { program_asm : _ | transf_c_program program_c = OK program_asm } ->
  { program_asm : _ & { trace : _ & { result : _ &
  transf_c_program program_c = OK program_asm /\
  Cstrategy.bigstep_program_terminates program_c trace result /\
  program_behaves (Asm.semantics program_asm) (Terminates trace result) } } }.
    Proof. #{PPP}# Qed.
#>"
; "However, the proof of the successful production takes a lot of time, we are unfortunately forced to admit it~:
<#
Proposition production_successful :
  { program_asm : _ | transf_c_program program_c = OK program_asm }.
    Proof. admit. Qed.
#>"
; "Note that, once smaller lemmas are completed, the main result appears immediately~:<#
Theorem certifying_production :
  { program_asm : _ & { trace : _ & { result : _ &
  transf_c_program program_c = OK program_asm /\
  Cstrategy.bigstep_program_terminates program_c trace result /\
  program_behaves (Asm.semantics program_asm) (Terminates trace result) } } }.
    Proof. apply asm_of_c. apply production_successful. Qed.
#>"
; "
The proof of {texttt "production_successful"} failed, because it takes extensive computation time. During that time, we were thinking instead to an other way to solve the potential membership of {S.P.C.asm} to {S.C.infty}. This leads to the part explaining the shortcut found.
(*
<@@{let open English in H_comment (BatList.init 4 (fun _ -> yes), Some maybe)}@
main() {
  int x = 2;
  int y = 3;
  return x + y;
}
@>
in particular by using mainly a sequence of <!eapply!> tactics. However, as long as the proof grows up, each <!eapply!> takes abnormally a too long time to succeed, in particular in front of a huge term. *)
"
; paragraph "{S.SL.C.infty} obtained with a meta consideration on Coq"
; (let module SL_p = S.SL_gen (struct let sl = "P" end) in "
The example above shows us that (*proving the behavior, of an apparently simple program {SL_p.C.asm}*)computing rawly in Coq is susceptible to take a long time. We also remark that this task is finally not a priority, especially in case it is easier to prove first if we have some kind of equivalent behavior between {S.SL.C.asm} and a Coq program.

Finally, we wonder if we can begin first with the equivalence between {S.SL.Coq.Deep.compcert} and {S.SL.coq}, than trying to obtain a {SL_p.C.lambda_l} by running extensive computations(*betting on a behavior $b$*). In fact, with the former proceeding, we will have implicitly the proof of the non wrong-behavior of {S.SL.Coq.Deep.compcert}, due to the supposed non wrong-behavior of the real Coq system.
(*
Then we will know at the meta level that {S.SL.C.asm} can not have a {emph "wrong"} behavior, even if a constructive $b$ has not yet been exhibited in Coq at that time (a condition needed for the creation of {S.SL.C.lambda_l}).

In conclusion, if we choose first to prove a kind of equivalence between {S.SL.Coq.Deep.compcert} and {S.SL.coq}, and succeed, we will have meta proved the non wrong behavior for {S.SL.C.asm}.
 By extending the reasoning further, we would be sure that the semantic of the initial source has been preserved to assembler. Moreover, we conjecture all this reasoning can yet be formally proved in a type system not different than at Coq level, by starting to exhibit a particular behavior $b$ from one side of the equivalence~: from the {S.SL.Coq.Deep.compcert}, or maybe the {S.SL.coq} side...*)

This solution has been explored and is further detailed in {cite ["shi2011"]}.
" (* On both cases, during the establisment of the two proofs, we are informed if we encounter some not well-formed part in {S.SL.C.asm} (part that need to be changed).*)
)
; subsubsection "Future work : the complete proof of equivalence"
; "
"
; paragraph "Coq {longrightarrow_} {S.C.infty}~?"
; "
Initially, before the creation of any simulator in {P.simsoc}, remark that to get at the end at least one {S.C.infty} simulator, we could initially take another approach, that is to start with a complete simulator in Coq (a similar one to {P.simsoc}, not only {S.SL.C.asm}), then to modify and equip Coq with a constructive extraction procedure into {S.C.infty} (like OCaml). This solution is feasible, because {S.C.infty} has a formal semantic (since the {S.C.compcert} AST), and rather general as the extraction process can be applied to any Coq program. However, as the project {P.simsoc} has historically been established before {P.simcert}, the organization of the {S.C.gcc} code behind {P.simsoc} is currently rather detailed and complex now, compared to the existing one at Coq side. Moreover, in term of efficiency, it seems not trivial how to perform in Coq the optimization necessary for setting good performances in SimSoC. Hence, the extraction from Coq is interesting, but we are also interested to know which large part of this {S.C.gcc} simulator can automatically be translated in Coq and which can not.
"
; paragraph "Coq {longleftarrow_} {S.C.infty}~?"
; "
The problem we are focusing is more open than only oriented from Coq to {S.C.infty}. For example, even if the {S.Manual.ArmSh.coq} is usually considered as the model of reference, for validation, tests are usually performed in {S.SL.C.asm} due to performance issue (appendix {ref_ Label.appendix_speed}). Indeed, we are interested in a semantical preservative way to report back modifications from {S.C.infty} side to Coq.

More generally, it may be difficult to prove the semantical preservation from a Turing-complete language to Coq. Nevertheless, we conjecture the {S.Manual.ArmSh.C.asm} is only formed with recursive functions. If we omit this semantical preservative requirement, the question remains important for proving the correction of an arbitrary {S.C.infty} code. Given a {S.C.human} code, under which conditions can one retrieve a ``similar'' Coq code, attesting its good termination ?

"
; paragraph "OCaml {longrightarrow_} (Coq {longleftrightarrow_} {S.C.infty}) ?"
; "

In~{ref_ Label.simgendef} and the SH part, we have explained the automatic importation of the {S.Manual.ArmSh.C.human} by {S.simgen} into {S.C.asm}. By following our reasoning of translating {S.C.infty} into some form of Coq code, it is legitimate to ask if we can also translate the {S.Manual.C.asm} in Coq directly. However, the generation of the {S.Manual.C.asm} being an automatic process, we had found convenient to use the existing code to produce the {S.Manual.ArmSh.coq} in the same OCaml environment. Then, {S.simgen} generates both the {S.Manual.C.asm} and the {S.Manual.ArmSh.coq}. By catching the reasoning in this context, the intention to prove the equivalence between these {emph "two"} outputs of a {emph "single"} program (here {S.simgen}) from a {emph "single"} input is less astonishing.

For this particular case, the generation of {S.C.asm} being automatic, instead of proving directly the output's equivalence, we can think about proving the good generation starting from the {S.simgen_ast}. Indeed, by approximating the raw Coq source into its AST (the Coq AST), as well as approximating the {S.C.asm} source into the {S.C.compcert} AST,
{itemize
[ "on one hand we have an OCaml function translating {forceline}from {S.simgen_ast} to Coq AST,"
; "on the other hand, we have another OCaml function {forceline}from {S.simgen_ast} to {S.C.compcert} AST." ]
}
As we think they can easily be translated in Coq, the problem of good equivalence between the {S.Manual.ArmSh.coq} and the {S.Manual.C.asm} can be simplified to the problem of building a couple given a particular {S.simgen_ast}, i.e writing a Coq function of type : {newline}
<!  (!>{S.Simgen.coq}<!, !>{S.Simgen.coq_deep_ocaml}<!) ->
    { (!>{S.Manual.ArmSh.coq}<!  ,  !>{S.Manual.Coq.Deep.infty}<!)
    |  !>{S.Manual.ArmSh.coq}<! <~> !>{S.Manual.Coq.Deep.infty}<!  }!>{newline}
(*or this one (which seems harder) : {newline}
<!  !>{S.Simgen.coq_deep_ocaml} <!->
    { (!>{S.Manual.ArmSh.coq}<!, !>{S.Manual.Coq.Deep.infty}<!)
    | !>{S.Manual.ArmSh.coq}<! <~> !>{S.Manual.Coq.Deep.infty}<! }!>{newline}*)
Of course, the equivalence function ``<!<~>!>'' still remains to be defined, but constructing this function-proof may be easier than working directly on the output. Hence, this solution can figure as a potential candidate to explore.

(*****************************************************************************)
"
; section ~label:Label.concl "Conclusion"
; "

We have obtained a SH4 formal model using the same algorithm as was employed for the ARMv6, by automatic extraction of pseudo-formal descriptions and automatic conversion to Coq syntax. The merge into {P.simcert} was performed modularly with functors in order to facilitate the integration of future processors.

The importation of a {S.C.compcert} value via our Coq pretty-printer being ready, the next step is to prove the equivalence between the existing Coq model and the {S.C.asm} model. After this, we will extend our proof to the complete part of the processor and system simulator : code specialization, dynamic recompilation on the host machine...

Finally, we hope to plug our certified simulator after the {P.compcert} chain leading to assembler source. In contrast with the bootstrapping process which aim to create a higher language than existing, this would terminate the certifying chain preserving the semantics of high-level software to safe execution on hardware.
"
(*****************************************************************************)
; bibliographystyle "alpha"
; bibliography "t"

(*****************************************************************************)
; newpage
; appendix

; section "Appendix"

; subsection ~label:Label.appendix_speed "Simulation speed"
; subsubsection "Between {S.SL.C.asm} and {S.SL.C.gcc}"
; "The {P.simsoc} team has released an archive including a simulator of ARMv6 instructions and also a cross-compiler targeting the ARMv6 : {https \"gforge.inria.fr/projects/simsoc\"}. In fact, the simulator present in {P.simsoc} version {Version.Cpp11.simsoc} {let module S = Sfoot in footnote () (https \"gforge.inria.fr/frs/download.php/28048/simsoc-0.7.1.tar.gz\")} (in {texttt "{P.simsoc}_{Version.Cpp11.simsoc}/libsimsoc/processors/arm_v6/simlight"}) is an optimized form of {S.SL.C.gcc} : for example, as new feature, it simulates the Thumb instruction set~{cite ["arm"]}. The not-optimized version can be found here~: {http \"formes.asia/media/simsoc-cert/simsoc-cert.tar.gz\"} (in {texttt "arm6/simlight"}){let module S = Sfoot in footnote () "Note that the support of the SH4 processor is not present in this archive..."}.

We now summarize all the results in a table. Remark that most of the following {S.C.gcc} examples could be found in {texttt "{P.simsoc}_{Version.Cpp11.simsoc}/examples/Demo/soft"}."
; Performance.draw_performance1 (fun x y -> small (longtable x y))

; newpage
; subsubsection "Comparison with {S.SL.coq}"
; "The {S.SL.coq} code can successfully be extracted to OCaml, we will call it {S.SL.Ocaml.coq}. However, at the time of writing, it does not support the Thumb instructions. Here are the results."
; Performance.draw_performance2 (fun x y -> small (longtable x y))
; "Remark that {S.SL.Coq.Deep.compcert} can also be extracted to OCaml. However, this deep embedded program can not be rawly tested without the use of the {let i_sqcup x = index sqcup (tiny x) in i_sqcup "APPLY"} operator : since {Version.compcert}, even if the option <!-interp!> permits to interpret an arbitrary {S.C.compcert} code, this code also needs to be in {S.C.lambda_l}. But we have proved in {ref_ Label.th_init_state} that {S.SL.C.asm} does not belong to {S.C.lambda_l} (moreover, we have at runtime : ``{texttt "ERROR: Initial state undefined"}'')."

; subsection "Errors in the OCaml extracted code"
; "
For the following, starting from a well-typed Coq program, the extraction gives wrong {Version.ocaml} files. This behaviour has been seen with at least the version trunk {Version.coqsvn}~and the {Version.coq} (in the examples, we will present code written in style {Version.coqsvn}).
" (* we are going to submit a bug report after further tests if needed, in particular with the current ``trunk'' development version. *)
; subsubsection ~label:Label.appendix_eta "The optimized {eta}-simplification"
; "
{https \"coq.inria.fr/bugs/show_bug.cgi?id=2570\" ^^ newline}
This extracted implementation from a Coq code~:
<~
open Datatypes
open List
open Vector

type __ = Obj.t

(** val to_list0 : nat -> 'a1 t -> 'a1 list **)

let to_list0 n = function
| Coq_nil -> Datatypes.Coq_nil
| Coq_cons (x, n0, t0) -> Datatypes.Coq_cons (x, Datatypes.Coq_nil)

(** val u1 :
    nat -> (__ -> nat -> __ t -> __ list) -> 'a1 t -> 'a3 list -> 'a2 t ->
    'a1 -> 'a1 **)

let u1 n to_list1 sep xs surr =
  let v_to_list = to_list0 n in
  let c = fun s l -> combine (v_to_list s) l in
  List.fold_left (fun x x0 -> x) (c sep (c surr xs))
~>
is accepted by the OCaml type-checker but it does not match its extracted interface, because <!u1!> has a too general type. This can be resolved by manually replacing {forceline}``<!let v_to_list   = to_list0 n  !>'' by {forceline}``<!let v_to_list v = to_list0 n v!>''.

"
; paragraph "Others possible solutions"
; "
{itemize
[
"Note that in the original Coq code~:
<#
Require Import Vector.
Require Import List.

Definition to_list0 : forall A n,
  Vector.t A n -> { _ : list A | True } :=
  fun A _ v => @exist (list A) (fun _ => True)
    match v with
    | Vector.cons x _ _ => List.cons x List.nil
    | Vector.nil => List.nil
    end I.

Definition u1 : forall M N L n
  (to_list1 : forall A n,
  Vector.t A n -> { _ : list A | True })
  (sep : Vector.t M n)
  (xs : list L)
  (surr : Vector.t N n),
  M -> M.
  intros until 4;
  pose (v_to_list A v := let (l, _) := to_list0 A n v in l);
  pose (c := fun B n s l => @List.combine _ B (v_to_list n s) l);
  refine (List.fold_left _ (c _ _ sep (c _ _ surr xs)));
  auto with *.
Defined.
#>
if we replace, <!to_list0!> by <!to_list1!> in <!u1!>, the extracted implementation matches correctly its interface~:
<~
let u1 n to_list0 sep xs surr =
  let v_to_list = to_list0 __ n in
  let c = fun s l -> combine (v_to_list s) l in
  List.fold_left (fun x x0 -> x)
    (Obj.magic (fun _ _ s l -> c s l) __ __ sep
      (Obj.magic (fun _ _ s l -> c s l) __ __ surr xs))
~>
"
; "
Instead of doing a manual {eta}-expansion, we can disable in Coq the optimization leading to this error with~:
<#
Set Extraction Flag 494.
#>
(see the file {texttt "coq_svn_{Version.coqsvn}/plugins/extraction/table.ml"} for others possibilities with numbers).
Then, we have this well-accepted code, which does not use <!Obj.magic!>~:
<~
let u1 n to_list1 sep xs surr =
  let v_to_list = fun _ v -> to_list0 n v in
  let c = fun _ _ s l -> combine (v_to_list __ s) l in
  List.fold_left (fun x x0 -> x) (c __ __ sep (c __ __ surr xs))
~>
" ]}

"
; paragraph "Localization in the source"
; "
In {texttt "coq_svn_{Version.coqsvn ^^ Code.latex_of_ppp PPP}mlutil.ml"}, the {eta}-reduction performed in the function <!kill_dummy!> does not perform an {eta}-reduction test to avoid a not generalizable <!'_a!>. At the time of writing, it is not sure that this test may or not be done in a similar way as {forceline ^^ texttt "coq_svn_{Version.coqsvn ^^ Code.latex_of_ppp PPP}extraction.ml"}.
Remark also, even if it has not been tested, that we may factorize the part under the case <!MLletin!> in <!kill_dummy!> with the <!MLletin!> part in <!kill_dummy_hd!> by abstracting what is necessary.
"
; subsubsection ~label:Label.appendix_singl "The optimized simplification of proofs on modules"
; "
{https \"coq.inria.fr/bugs/show_bug.cgi?id=843\" ^^ newline}
According to the {index le (English.bdiz)} convertibility rule, an object of type <!Prop!> can be identified as an object of type <!Set!> or <!Type!>. In particular, this is a program well-compiled by Coq :
<#
Inductive F := .

Module Type MT.
  Parameter t : Type.
End MT.

Module M : MT.
  Definition t := F.
End M.
#>
However, the extracted OCaml program is not well-typed because the extracted module <!M!> is empty whereas its module type <!MT!> is not : ``The field <!t!> is required but not provided''.

"
; paragraph "Localization in the source"
; "
{itemize
[
"In {texttt "coq_svn_{Version.coqsvn ^^ Code.latex_of_ppp PPP}extract_env.ml"}, the function <!extract_seb!> calls <!Modops.add_signature!> before <!extract_sfb!> to update the current environment from <!env!> to <!env'!>. However, at the same time, there is also no call to <!Visit.add_ref!> in case we encounter an association $["<!t!>" {leadsto} T] {in_} "<!env!>"$ and an association $["<!t!>" {leadsto} "<!Prop!>"] {in_} "<!env'!>"$, if <!Prop!> ${index le English.bdiz} T$ and the {English.bdiz}-normal form of $T {ne}$ <!Prop!>. Then, <!extract_sfb!> considers that the extraction of <!t!> can be ignored. This extracting omission can generally be performed for a proposition, but our term <!t!> has been recategorized as <!Type!> due to the module type constraint."
;
"Similarly, for the functor case, <!extract_seb!> calls <!Modops.add_module!> without doing the above checking. Indeed, by replacing ``<!Module M : MT.!>'' with ``<!Module M (M : MT) : MT.!>'', we have an empty functor in the extracted source." ]
}

"
; paragraph "Others possible solutions"
; "
{itemize
[
"We can explicitly strengthened the type of <!F!> by writing {forceline}``<!Inductive F : Set := .!>''."
;
"At the end, if we mention that the definition inside the module will be used : ``<!Definition t := M.t.!>'', the extraction works correctly because <!M.t!> has been classified as a value that the extraction can not avoid. Hence, <!Visit.add_ref!> will be called before we enter in <!extract_sfb!>." ]
}
"
; paragraph "Notes"
; "
The proposed solution is enough to get a correct extracted OCaml files for our SH4 project, but is not completely a general form for an arbitrary Coq program. For example, we would have the same problem for a module where the typing of its contents is consulted several times after its definition~:
<#
Module M.
  Definition t := F.
End M.

Module Make (M : MT).
End Make.

Module MM := Make M.
#>
"
])
