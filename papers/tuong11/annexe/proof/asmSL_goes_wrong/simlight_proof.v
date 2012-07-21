(**
SimSoC-Cert, a toolkit for generating certified processor simulators
See the COPYRIGHTS and LICENSE files.

Proof that asmC-simlight goes wrong.
*)

Set Implicit Arguments.

Require Import Csem Cstrategy Smallstep Events Integers Globalenvs AST Memory
  Csyntax Coqlib Maps.
Require Import Util simlight.

Definition asmC_simlight := p. 

Lemma no_initial_state : forall s, ~ Csem.initial_state asmC_simlight s.

  Proof.
    intuition. inversion H.
    vm_compute in H1. inversion H1. subst b.
    vm_compute in H2. inversion H2. subst f.
    vm_compute in H3. inversion H3.
  Qed.

Require Import Behaviors.

Proposition wrong_behavior :
  program_behaves (Csem.semantics asmC_simlight) (Goes_wrong E0).

  Proof.
    apply program_goes_initially_wrong ; 
    apply no_initial_state.
  Qed.
