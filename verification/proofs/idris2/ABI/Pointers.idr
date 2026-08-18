-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- ABI Proof: Non-null pointer safety
-- Template proof — customise for your project's pointer types.
-- All proofs MUST be constructive (no believe_me, no assert_total).

module ABI.Pointers

import Data.So

%default total

||| A pointer value that has been proven non-null.
||| The `So` constraint carries a compile-time witness that `ptr /= 0`.
public export
record SafePtr where
  constructor MkSafePtr
  ptr : Bits64
  {auto 0 nonNull : So (ptr /= 0)}

||| Proof that SafePtr can never hold a null (zero) value.
||| This is enforced by the `So` constraint in the record.
||| Erased (0 quantity): the witness it extracts is itself erased, so
||| this proof exists only at the type level — which is all a proof needs.
export
0 safePtrNeverNull : (sp : SafePtr) -> So (sp.ptr /= 0)
safePtrNeverNull sp = sp.nonNull

||| Wrap a raw pointer with a runtime null check.
||| Returns Nothing if the pointer is null.
export
checkPtr : (raw : Bits64) -> Maybe SafePtr
checkPtr 0 = Nothing
checkPtr raw = case choose (raw /= 0) of
  Left prf => Just (MkSafePtr raw)
  Right _ => Nothing

||| Proof that checkPtr 0 always returns Nothing.
export
checkPtrZeroIsNothing : checkPtr 0 = Nothing
checkPtrZeroIsNothing = Refl

||| An opaque handle backed by a non-null pointer.
||| Use this for FFI resource handles (file descriptors, sockets, etc.).
public export
record Handle (tag : String) where
  constructor MkHandle
  safePtr : SafePtr

||| `So b` has at most one inhabitant (proof irrelevance for `So`).
0 soUnique : {b : Bool} -> (x, y : So b) -> x = y
soUnique Oh Oh = Refl

||| Proof that two handles with equal pointers are equal.
||| Erased (0 quantity) so the erased `nonNull` witnesses may be
||| inspected; their equality is discharged by `soUnique`.
export
0 handlePtrEq : (h1, h2 : Handle tag) -> h1.safePtr.ptr = h2.safePtr.ptr -> h1 = h2
handlePtrEq (MkHandle (MkSafePtr p {nonNull = n1})) (MkHandle (MkSafePtr q {nonNull = n2})) eq =
  case eq of
    Refl => rewrite soUnique n1 n2 in Refl
