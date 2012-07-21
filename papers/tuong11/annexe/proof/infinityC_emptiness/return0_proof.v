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
