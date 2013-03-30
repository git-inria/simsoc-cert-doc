Require Import
  BinPos
  Bvector
  NaryFunctions
  String
.

Open Scope string_scope.

Inductive floatsize : Type :=
  | F32: floatsize
  | F64: floatsize.

Inductive type : Type :=
  | Tvoid : type
  | Tfloat: floatsize -> type
  | Tfunction: typelist -> type -> type
with typelist : Type :=
  | Tnil : typelist
  | Tcons : type -> typelist -> typelist.

Definition ident := positive.

Record program (F : Type) : Type := mkprogram {
  prog_funct : list (ident * F);
  prog_main : ident
}.

Definition ast := program type.



Module Rec.
  Definition _floatsize {A} := floatsize_rect (fun _ => A).

  Scheme _type_rect := Induction for type Sort Type
  with _typelist_rect := Induction for typelist Sort Type.

  Definition _ty_ AA P P0
    (f_ty : forall (P : type -> Type) (P0 : typelist -> Type),
         P Tvoid ->
         (forall f1 : floatsize, P (Tfloat f1)) ->
         (forall t : typelist,
          P0 t -> forall t0 : type, P t0 -> P (Tfunction t t0)) ->
         P0 Tnil ->
         (forall t : type,
          P t -> forall t0 : typelist, P0 t0 -> P0 (Tcons t t0)) ->
         AA P P0)
    {F}
    (_floatsize : _ -> F)
    f_void f_float f_function f_tnil f_tcons
    := f_ty (fun _ => P) (fun _ => P0)
      f_void
      (fun f => f_float (_floatsize f))
      (fun _ aux0 _ aux1 => f_function aux0 aux1)

      f_tnil
      (fun _ aux1 _ aux2 => f_tcons aux1 aux2)
  .

  Definition _type {P P0} := @_ty_ _ P P0 _type_rect.
  Definition _typelist {P P0} := @_ty_ _ P P0 _typelist_rect.

  Definition _program {A B C D E F : Type}
    (_prod : forall A B, (A -> _) -> (B -> _) -> prod A B -> C)
    (_list : forall A, (A -> _) -> list A -> _)
    (f_f : F -> E)
    _ident
    (f_mk : A -> B -> D)
    (x : _ F)
    :=
    f_mk
      (_list _ (_prod _ _ _ident f_f) (prog_funct _ x))
      (_ident (prog_main _ x)).
End Rec.



Module Rec_weak0.
  Import Rec.

  Notation "A ** n" := (A ^^ n --> A) (at level 29) : type_scope.

  Definition _floatsize {A} :
    A ** 0 ->
    A ** 0 ->
    _ := @_floatsize _.

  Definition _type_ A B (f : _ -> Type) := f (
    A ** 0 ->
    A ** 1 ->
    A ** 2 ->
    A ** 0 ->
    A ** 2 ->
    B).
  Definition _type {A} : _type_ A _ (fun ty => _ -> ty) := @_type _ _ _.
  Definition _typelist {A} : _type_ A _ (fun ty => _ -> ty) := @_typelist _ _ _.

  Definition _program {A B} : _ -> _ -> _ -> _ ->
    A ** 2 ->
    _ := @_program _ _ A _ A B.
End Rec_weak0.

Module Rec_weak.
  Import Rec.

  Notation "A ** n" := (A ^^ n --> A) (at level 29) : type_scope.
  Notation "A [ a ; .. ; b ] -> C" := (A ** a -> .. (A ** b -> C) ..) (at level 90).

  Definition _floatsize {A} : A
    [ 0
    ; 0 ]
    -> _ := @_floatsize _.

  Definition _type_ A B (f : _ -> Type) := f (A
    [ 0
    ; 1
    ; 2
    ; 0
    ; 2 ] -> B).
  Definition _type {A} : _type_ A _ (fun ty => _ -> ty) := @_type _ _ _.
  Definition _typelist {A} : _type_ A _ (fun ty => _ -> ty) := @_typelist _ _ _.

  Definition _program {A B} : _ -> _ -> _ -> _ -> A
    [ 2 ] ->
    _ := @_program _ _ A _ A B.
End Rec_weak.



Module Type CONSTRUCTOR.
  Import Rec_weak.
  Parameter t : Type -> Type.
  Parameter u : Type.

  Parameter _RECORD : forall n : nat, vector string n -> t u ** n.
  Parameter _INDUCTIVE : string -> forall n : nat, t u ** n.

  Parameter _prod : forall A B, (A -> t u) -> (B -> t u) -> prod A B -> t u.
  Parameter _list : forall A, (A -> t u) -> list A -> t u.
  Parameter _positive : positive -> t u.
End CONSTRUCTOR.



Module Constructor (Import C : CONSTRUCTOR).
  Import Rec_weak.

  Notation "{ a ; .. ; b }" := (_RECORD _ (Vcons _ a _ .. (Vcons _ b _ (Vnil _)) ..)).
  Notation "| x" := (_INDUCTIVE x _) (at level 9).

  Definition _floatsize := _floatsize
    | "F32"
    | "F64".

  Definition _type_ T (ty : forall A : Type,
         _type_ A (T -> A)
           (fun ty : Type =>
            (floatsize -> A) -> ty) ) := ty _ _floatsize
    | "Tvoid"
    | "Tfloat"
    | "Tfunction"
    | "Tnil"
    | "Tcons".

  Definition _type := _type_ _ (@_type).
  Definition _typelist := _type_ _ (@_typelist).

  Definition _ident := _positive.

  Definition _program {A} f_a := @_program _ A _prod _list f_a _ident
    { "prog_funct" ; "prog_main" }.

  Definition _ast := _program _type.
End Constructor.



(* ********************************************************************** *)



Module Vector.
  (* Convert a [vector] into a [list] with an extra property on its length. *)
  Definition to_list : forall A n, vector A n -> { l : list A | n = List.length l }.
    induction n ; intros.
    exists List.nil.
    trivial.
    inversion X.
    destruct (IHn X0).
    exists (cons a x).
    simpl.
    auto.
  Defined.
End Vector.

Module String_C <: CONSTRUCTOR.
  Import Rec_weak.

  Record t_ := mk_t_
    { s : string }.

  Definition t (u : Type) := t_.
  Definition u := unit.

  Definition _RECORD : forall n : nat, vector string n -> t u ** n.
  intros.
  case_eq n ; intros.
  apply ncurry.
  intros. exact (mk_t_ "(* assert false *)").

  subst n ; rename n0 into n.
  apply ncurry.
  intro cpl.

  generalize (nprod_to_list _ _ (snd cpl)) (Vector.to_list _ _ H) ; intros l1 (l2, p).
  apply mk_t_.
  case_eq l2; intros. rewrite H0 in p. simpl in p. assert False. Require Import Omega. omega. tauto.
  refine ("{| " ++ s0 ++ " := " ++ s (fst cpl) ++ _ ++ " |}").

  refine (List.fold_left _ (List.combine l1 l) "").
  intros acc (s, name). inversion s.
  exact (acc ++ " ; " ++ name ++ " := " ++ s1).
  Defined.

  Definition _INDUCTIVE : string -> forall n : nat, t u ** n.
  intros.
  case_eq n ; intros.
  apply ncurry.
  intros. exact (mk_t_ H).

  apply ncurry.
  intro cpl.
  generalize (nprod_to_list _ _ cpl) ; intros l.

  cut string.
  intros s. exact (mk_t_ (H ++ " " ++ s)).
  refine (List.fold_left _ l "").
  intros acc s. inversion s.
  exact (acc ++ "(" ++ s0 ++ ")").
  Defined.

  Definition _prod : forall A B, (A -> t u) -> (B -> t u) -> prod A B -> t u.
  intros. inversion X1.
  destruct (X X2). destruct (X0 X3).
  apply mk_t_. exact ("(" ++ s0 ++ ", " ++ s1 ++ ")").
  Defined.

  Definition _list : forall A, (A -> t u) -> list A -> t u.
  intros.
  apply mk_t_.
  case_eq X0 ; intros. exact "[]".
  refine ("[" ++ s (X a) ++ _ ++ "]").
  refine (List.fold_left _ l "").
  intros acc x. destruct (X x).
  exact (acc ++ "; " ++ s0).
  Defined.

  Require Import Euclid.
  Require Import Recdef.
  Require Import Ascii.

  Definition ascii_to_string a := String a "".
  Coercion ascii_to_string : ascii >-> string.
  Definition ascii_of_number n := ascii_of_nat (n + 48).
  Coercion ascii_of_number : nat >-> ascii.

  Open Scope nat_scope.

  Require Import Euclid.
  Require Import Omega.
  Remark gt_10_0 : 10 > 0.
    omega.
  Qed.

  Function string_of_nat_aux (acc : string) (n : nat) {wf lt n} : string :=
    match n with
    | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 => n ++ acc
    | _ =>
      string_of_nat_aux
        (let (r, _) := modulo 10 gt_10_0 n in r ++ acc)
        (let (q, _) := quotient 10 gt_10_0 n in q)
    end.
    intros.
    match goal with |- context [ ?Q ?a ?b ?c ] => destruct (Q a b c) as (q, pr_q) end.
    inversion pr_q.
    omega.
    auto with *.
  Qed.

  Definition string_of_nat_ := string_of_nat_aux "".

  Definition string_of_positive_ p :=
    string_of_nat_ (nat_of_P p).

  Definition _positive : positive -> t u.
  intros. apply mk_t_. refine (_ ++ "%positive").
  apply string_of_positive_. trivial.
  Defined.

End String_C.

Module C := Constructor String_C.



(* ********************************************************************** *)



Require Import List.
Open Scope list_scope.
Notation "[ ]" := nil : list_scope.
Notation "[ a ; .. ; b ]" := (a :: .. (b :: []) ..) : list_scope.

(*Check {| prog_funct := [(1%positive, Tvoid); (1%positive, Tfloat (F32))] ; prog_main := 1%positive |}.*)