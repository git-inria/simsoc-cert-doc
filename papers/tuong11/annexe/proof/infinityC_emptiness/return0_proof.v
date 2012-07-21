(**
SimSoC-Cert, a toolkit for generating certified processor simulators
See the COPYRIGHTS and LICENSE files.

Proof that return0 returns 0.
*)

Set Implicit Arguments.

Require Import Csem Cstrategy Smallstep Events Integers Globalenvs AST Memory
  Csyntax Coqlib Maps.
Require Import Util return0.

Ltac norm e := let x := fresh in set (x := e); vm_compute in x; subst x.

Ltac comp :=
  match goal with
    | |- ?l = _ => norm l
    | |- Csem.alloc_variables _ _ ?l _ _ => norm l
    | |- Csem.bind_parameters _ _ ?l _ _ => norm l
  end.

Ltac hnorm e := let x := fresh in set (x := e); hnf in x; subst x.

Ltac hcomp :=
  match goal with
    | |- ?l = _ => hnorm l
    | |- Csem.alloc_variables _ _ ?l _ _ => hnorm l
    | |- Csem.bind_parameters _ _ ?l _ _ => hnorm l
  end.

Ltac alloc :=
  match goal with
    | |- Csem.alloc_variables _ _ nil _ _ => simple apply alloc_variables_nil
    | |- Csem.alloc_variables _ _ (cons _ _) _ _ =>
      simple eapply alloc_variables_cons; [comp; refl | alloc]
  end.

Ltac bind :=
  match goal with
    | |- Csem.bind_parameters _ _ nil _ _ => simple apply bind_parameters_nil
    | |- Csem.bind_parameters _ _ (cons _ _) _ _ =>
      simple eapply bind_parameters_cons; [hcomp; refl | bind]
  end.

Ltac s := simple eapply star_step with (t:=E0)(t1:=E0)(t2:=E0);
  [right|idtac|refl].

Ltac e := simple eapply star_step with (t:=E0)(t1:=E0)(t2:=E0);
  [left |idtac|refl].

Set Printing Depth 1.

Require Import Complements.
Require Import Cstrategy.
Require Import Behaviors.

Lemma source_terminates : exists t, exists k, Cstrategy.bigstep_program_terminates p t k.

  Proof.
    simple eapply ex_intro. simple eapply ex_intro.
    eapply bigstep_program_terminates_intro.
    comp;refl.
    comp;refl.
    comp;refl.
    comp;refl.
    eapply eval_funcall_internal.
    list_norepet beq_positive_ok.
    comp; alloc.
    comp; bind.

    eapply exec_Sreturn_some.

    eapply eval_expression_intro.
    eapply eval_val.

    eapply esr_val.
    compute.
    intuition.
    simpl.
    trivial.
  Qed.

Module V0.

Definition c_program := p.

Lemma source_terminates : { trace : _ & { result : _ & Cstrategy.bigstep_program_terminates c_program trace result  } }.

  Proof.
    eexists. eexists.
    eapply bigstep_program_terminates_intro.
    comp;refl.
    comp;refl.
    comp;refl.
    comp;refl.
    eapply eval_funcall_internal.
    list_norepet beq_positive_ok.
    comp; alloc.
    comp; bind.

    eapply exec_Sreturn_some.

    eapply eval_expression_intro.
    eapply eval_val.

    eapply esr_val.
    compute.
    intuition.
    simpl.
    trivial.
  Qed.

Definition trace := projT1 source_terminates.
Definition result := projT1 (projT2 source_terminates).

Require Import Axioms.
Require Import Coqlib.
Require Import Maps.
Require Import Errors.
Require Import AST.
Require Import Values.
Require Import Smallstep.

Require Import Asm.
Require Import Compiler.
Require Import Complements.

Theorem asm_of_c : 
  { asm : _ | transf_c_program c_program = OK asm } ->
  { asm : _ & { trace : _ & { result : _ & 
  transf_c_program c_program = OK asm /\
  Cstrategy.bigstep_program_terminates c_program trace result /\
  program_behaves (Asm.semantics asm) (Terminates trace result) } } }.

  Proof.
    intros (asm, p_asm).
    exists asm. exists trace. exists result.
    assert (bigstep_program_terminates c_program trace result).
    unfold trace, result. destruct source_terminates. destruct s. trivial.
    intuition.
    generalize (bigstep_cstrategy_preservation c_program asm); intro.
    intuition.
  Qed.


Theorem production_successful :
  { asm : _ | transf_c_program c_program = OK asm }.

  Proof.
(*
  (* unsuccessful draft *)
    unfold transf_c_program.
    simpl.
    unfold transf_clight_program.
    simpl.
    unfold Cshmgen.transl_program.
    simpl.

    unfold transform_partial_program2. simpl.

      generalize typ_eq; intro. 
    remember (Cshmgen.list_typ_eq (Tfloat :: nil) (Tfloat :: nil) &&
            opt_typ_eq (Some Tfloat) (Some Tfloat)) as b.
    assert (b = true).
    destruct (Cshmgen.list_typ_eq (Tfloat :: nil) (Tfloat :: nil)). simpl in *.
    destruct (opt_typ_eq (Some Tfloat) (Some Tfloat)). simpl in *.
    trivial.

    auto.
    auto.
    rewrite H0.
    assert (forall x y, Cshmgen.list_typ_eq x x && opt_typ_eq y y = true).
    clear.
    intuition.
    destruct (Cshmgen.list_typ_eq x x).
    ring_simplify. destruct (opt_typ_eq y y).
    trivial.
    auto.
    auto.
    repeat rewrite H1; simpl.
    clear.

    unfold transf_cminor_program. simpl.
    unfold transform_partial_program.
    unfold Selection.sel_program.
    match goal with | |- context [ Selection.sel_fundef ?a ] => remember (Selection.sel_fundef a) as b end.
    simpl in b.
    unfold Selection.sel_fundef in b.
    compute in b. rename b into f.
    unfold print. 
    unfold transform_program.
    unfold prog_funct.
    unfold transf_program.
    match goal with | |- context [ map ?a ?x ] => remember (map a x) as bb end.
    simpl in Heqbb.
    subst f. unfold print in *.
    simpl.
    (*Check map_partial.
    Print Errors.res.
    Check List.In.*)
    cut (forall x, List.In x bb -> match transf_cminorsel_fundef (snd x) with OK _ => true | _ => false end = true).
    clear.
    intros. 
    cut (match map_partial prefix_name transf_cminorsel_fundef bb with OK _ => true | _ => false end = true).
    destruct (map_partial prefix_name transf_cminorsel_fundef bb). simpl. intros.
    eexists {| prog_funct := l; prog_main := main; prog_vars := nil |}.
    trivial.
    auto with *.

    case_eq (bb).
    trivial.
    intros. revert H. revert p l H0.
    induction bb. auto with *.
    intros. injection H0. intros. rewrite H1 in *. rewrite H2 in *. clear H0 H1 H2.
    simpl.
    generalize (H p (List.in_eq _ _)).
    destruct p. simpl. destruct (transf_cminorsel_fundef f ).
    cut (match map_partial prefix_name transf_cminorsel_fundef l with OK _ => true | _ => false end = true).
    destruct (map_partial prefix_name transf_cminorsel_fundef l).
    trivial.
    trivial.
    case_eq l. trivial. intros. 
    generalize (IHbb _ _ H0); intros.
    apply H1. intros.  apply H.
    auto with *.
    trivial.

    case_eq bb. 
    auto. intros p l H. rewrite Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    case_eq bb. 
    auto. intros p l H. rewrite <- Heqbb in H. injection H ; clear H.
    intros H H0 x H1. destruct H1. subst x. subst p. simpl. trivial.
    clear bb Heqbb . rename H into Heqbb. rename l into bb.
    clear p H0. revert x H1.

    subst bb.
    intuition.
    subst x.
    unfold transf_cminorsel_fundef. simpl.
    unfold transf_rtl_fundef. simpl.
    match goal with |- context [ Csharpminor.fn_sig ?a ] => remember (Csharpminor.fn_sig a) as b end.
    match goal with |- context [ Tailcall.transf_function ?a ] => remember (Tailcall.transf_function a) as bb end.
    revert Heqbb.
    unfold Tailcall.transf_function. simpl.
    match goal with |- context [ Tailcall.transf_instr ?a ] => remember (Tailcall.transf_instr a) as bbb end.
    unfold Tailcall.transf_instr in Heqbbb.
    unfold RTL.transf_function.
    intros. subst bb. simpl.
    match goal with |- context [ CastOptim.transf_function ?a ] => remember a as bb end.
    subst bbb.
    unfold PTree.map in Heqbb. simpl in Heqbb.
    repeat rewrite compose_print_identity.
    subst b.
    revert Heqbb.
    unfold RTLgen.ret_reg. simpl.
    intros. 
    (* *)
    subst bb. simpl.
    match goal with |- context [ PTree.Node ?a ?b ?c ] => remember (PTree.Node a b c) as bb end.
    match goal with |- context [ Csharpminor.fn_sig ?a ] => remember a as bbb end.
    revert Heqbbb.
    unfold Cshmgen.make_cast. simpl. unfold Cshmgen.make_intconst. simpl.
    intros. subst bbb.
    unfold Csharpminor.fn_sig. simpl.
    unfold CastOptim.transf_function. simpl.
    match goal with |- context [ CastOptim.transf_code ?a _ ] => remember a as bbb end.
    revert Heqbbb.
    unfold CastOptim.analyze.
    simpl.
    unfold CastOptim.transfer. simpl.
    unfold RTL.successors. simpl.
    intros. 
    subst bb. simpl in * .
    (*Print Module CastOptim.
    Print Module CastOptim.DS.*)
    revert Heqbbb.
    match goal with |- context [ CastOptim.DS.fixpoint ?a ?b ?c ] => remember (CastOptim.DS.fixpoint a b c) as oo end.
*)
    admit.
  Qed.

Theorem certifying_production : 
  { asm : _ & { trace : _ & { result : _ & 
  transf_c_program c_program = OK asm /\
  Cstrategy.bigstep_program_terminates c_program trace result /\
  program_behaves (Asm.semantics asm) (Terminates trace result) } } }.

  Proof.
    apply asm_of_c. apply production_successful.
  Qed.

End V0.
