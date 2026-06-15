-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- =====================================================================
-- KitchenSpeak — No-Curdle Proof (Agda)   [v2.0 flagship]
-- =====================================================================
--
-- Lowers `examples/v2/scald-milk.ks` and discharges the v2.0 keystone:
-- the MATERIAL safety envelope. Where PoachedEgg.agda proved a Tropical
-- *appliance/boil* envelope (`tropical-safe`), this proves a Tropical
-- *material* envelope — milk's curdle point — is never reached. It is the
-- machine-checked form of the checker's CURDLE rule (check.ml /
-- docs/spec/0003-type-system.adoc §physical-safety).
--
--     recipe ScaldMilk {
--         use Milk as m;
--         sync(Hob) {
--             Hob.Heat(m, target: 70C, slope: GENTLE)
--                 until ~ temp >= 68C
--                 max_duration 5min
--                 on_fail ABORT;
--         }
--     }
--
-- Milk (stdlib_ks.ml): soft scald 70C, HARD curdle 82C. The recipe's
-- GENTLE setpoint is 70C, strictly below the curdle envelope. The headline
-- theorem `no-curdle` shows the controlled trajectory never reaches 82C,
-- and `stays-fresh` shows the milk's phase typestate never becomes CURDLED.
--
-- Obligations discharged:
--   1. Termination  — `warm` is structurally recursive on a fuel parameter.
--   2. Safety       — every WarmResult is Warmed or Aborted (on_fail ABORT).
--   3. Echo witness — a Warmed result carries an existential reach witness.
--   4. Material     — NEW: `no-curdle` + `stays-fresh`. The milk never
--                     crosses its hard curdle limit; its phase stays FRESH.
--
-- The soft scald threshold (70C) MAY be reached — that is a quality warning
-- in the checker, not a ruin — and the proof deliberately does NOT forbid it.
--
-- ---------------------------------------------------------------------
-- Agda 2.6.4+ / stdlib 2.x. As with PoachedEgg.agda we do NOT use
-- {-# OPTIONS --safe #-}: the sensor stream and the GENTLE controller
-- contract are physical-world postulates a production lowering replaces
-- with HAL bindings.
-- ---------------------------------------------------------------------

module NoCurdle where

open import Data.Nat
  using (ℕ; zero; suc; _≥_; _≥?_; _≤_; _<_; _<?_)
open import Data.Nat.Properties
  using (<-transʳ; <-transˡ; <-irrefl)
open import Data.Sum
  using (_⊎_; inj₁; inj₂)
open import Data.Product
  using (_×_; _,_; Σ-syntax)
open import Data.Empty
  using (⊥-elim)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl)
open import Relation.Nullary
  using (yes; no)
open import Relation.Nullary.Decidable
  using (True; toWitness)


-- =====================================================================
-- § 1. Physical primitives and the milk envelope.
-- =====================================================================

Celsius : Set
Celsius = ℕ

Minutes : Set
Minutes = ℕ

-- The recipe's `until ~ temp >= 68C` guard.
reach-threshold : Celsius
reach-threshold = 68

-- The GENTLE setpoint cap (`target: 70C`).
target-cap : Celsius
target-cap = 70

-- Milk's SOFT scald threshold (quality; reaching it only warns).
scald-threshold : Celsius
scald-threshold = 70

-- Milk's HARD curdle threshold (ruin; reaching it is the CURDLE error).
curdle-threshold : Celsius
curdle-threshold = 82

-- The setpoint sits strictly below the curdle envelope — a configuration
-- fact about the chosen recipe, computed by the decision procedure.
cap<curdle : target-cap < curdle-threshold
cap<curdle = toWitness {a? = target-cap <? curdle-threshold} _


-- =====================================================================
-- § 2. Milk typestate.
-- =====================================================================
--
-- Milk's phase is a typestate. CURDLED is the absorbing ruin phase the
-- type system forbids entering (constitution §2.2).

data MilkState : Set where
  FRESH   : MilkState
  CURDLED : MilkState

record Milk (s : MilkState) : Set where
  constructor mkMilk


-- =====================================================================
-- § 3. Sensor oracle and the GENTLE controller contract.
-- =====================================================================
--
-- AXIOM: `temp-at` is the postulated hob thermometer (the physical world,
-- not a derivable term). `gentle-bounded` is the GENTLE controller's
-- contract — under GENTLE the trajectory never exceeds the setpoint cap —
-- supplied by the HAL and trusted here, exactly as PoachedEgg.agda trusts
-- its `gentle-bounded`. A production lowering discharges it from an
-- integrator bound on the slope. Trusted base: 1 sensor + 1 controller.
postulate
  temp-at        : Minutes → Celsius
  gentle-bounded : ∀ (t : Minutes) → temp-at t ≤ target-cap


-- =====================================================================
-- § 4. The Warm step (Tropical + Echo, on_fail ABORT).
-- =====================================================================

ReachWitness : Set
ReachWitness = Σ[ t ∈ Minutes ] temp-at t ≥ reach-threshold

data WarmResult : Set where
  Warmed  : Milk FRESH → ReachWitness → WarmResult
  Aborted : WarmResult

warm : Milk FRESH → (fuel : ℕ) → (elapsed : Minutes) → WarmResult
warm _ zero    _ = Aborted
warm m (suc n) t with temp-at t ≥? reach-threshold
... | yes pf = Warmed mkMilk (t , pf)
... | no  _  = warm m n (suc t)

-- max_duration 5min budget.
warm-budget : Minutes
warm-budget = 5

scald-milk : Milk FRESH → WarmResult
scald-milk m = warm m warm-budget 0


-- =====================================================================
-- § 5. Obligations, discharged.
-- =====================================================================

-- ── Obligation 1 — Termination ───────────────────────────────────────
-- Structural recursion of `warm` on `fuel`. If this file type-checks,
-- the loop terminates; the measure is the `max_duration` budget.

-- ── Obligation 2 — Safety (on_fail totality) ─────────────────────────
warm-exhaustive : ∀ (r : WarmResult) →
  (Σ[ m ∈ Milk FRESH ] Σ[ w ∈ ReachWitness ] r ≡ Warmed m w)
  ⊎ (r ≡ Aborted)
warm-exhaustive (Warmed m w) = inj₁ (m , w , refl)
warm-exhaustive Aborted      = inj₂ refl

-- ── Obligation 3 — Echo witness extraction ───────────────────────────
warmed-gives-witness :
  ∀ (m : Milk FRESH) {m′ : Milk FRESH} {w : ReachWitness} →
  scald-milk m ≡ Warmed m′ w →
  Σ[ t ∈ Minutes ] temp-at t ≥ reach-threshold
warmed-gives-witness _ {w = w} _ = w

-- ── Obligation 4 — MATERIAL safety (the v2.0 keystone) ────────────────
--
-- (a) The headline: at every sampled minute the milk is strictly below
--     its curdle envelope. Composing the controller contract (§3) with
--     the configuration fact (§1): temp-at t ≤ 70 < 82.
no-curdle : ∀ (t : Minutes) → temp-at t < curdle-threshold
no-curdle t = <-transʳ (gentle-bounded t) cap<curdle

-- (b) The milk's phase typestate, read off the sensor at each minute.
milk-state-at : Minutes → MilkState
milk-state-at t with temp-at t ≥? curdle-threshold
... | yes _ = CURDLED
... | no  _ = FRESH

-- (c) Therefore the milk never enters the CURDLED ruin phase. The `yes`
--     branch would need temp-at t ≥ curdle, contradicting `no-curdle`.
stays-fresh : ∀ (t : Minutes) → milk-state-at t ≡ FRESH
stays-fresh t with temp-at t ≥? curdle-threshold
... | yes ge = ⊥-elim (<-irrefl refl (<-transˡ (no-curdle t) ge))
... | no  _  = refl

-- (d) Specialised to the trajectory the recipe actually produces: a
--     warmed milk reached its guard temperature inside the safe band
--     [68, 82).
warmed-is-safe :
  ∀ (m : Milk FRESH) {m′ : Milk FRESH} {w : ReachWitness} →
  scald-milk m ≡ Warmed m′ w →
  Σ[ t ∈ Minutes ] (temp-at t ≥ reach-threshold × temp-at t < curdle-threshold)
warmed-is-safe _ {w = (t , ge)} _ = t , ge , no-curdle t


-- =====================================================================
-- § 6. What is *not* proved here (the trust boundary).
-- =====================================================================
--
--   (a) That `temp-at` is a sound classifier — the postulated-sensor
--       boundary (COMMENTARY.adoc §Echo).
--   (b) That `gentle-bounded` holds of a real controller — v2.0
--       postulates the GENTLE contract; a production port discharges it
--       from an integrator bound on the slope.
--   (c) Linearity (Milk used once) — holds by inspection of signatures,
--       machine-checked under a QTT-Agda port (cf. Dough.agda §3).
--
-- What it DOES prove: `scald-milk` is total, every success carries a
-- reach witness, and — the keystone — the milk never reaches its curdle
-- envelope and never enters the CURDLED phase. This is the proof-side
-- mirror of the checker's CURDLE rule: do the stuff AND prove the stuff.
