
variable (α : Type) (p q : α → Prop) (r : Prop)
variable (r : Prop)

-- Izjave napišite na list papirja, nato pa jih dokažite v datoteki.

theorem eq1 : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) :=
  by
    apply Iff.intro
    intro h1
    intro x
    intro px
    apply h1
    exact ⟨ x, px ⟩
    intro h hp
    let ⟨x, hp⟩ := hp
    exact h x hp

theorem eq2 : (r → ∀ x, p x) ↔ (∀ x, r → p x) :=
  by
    apply Iff.intro
    · intro h x r
      exact h r x
    · intro h x r
      exact h r x

theorem eq3 : r ∧ (∃ x, p x) ↔ (∃ x, r ∧ p x) :=
  by
    apply Iff.intro
    · intro ⟨ hr, ⟨ x, hp ⟩ ⟩
      apply Exists.intro x ⟨ hr, hp ⟩
    · intro ⟨ hr, ⟨ x, hp ⟩ ⟩
      constructor
      exact x
      exists hr

theorem eq4 : r ∨ (∀ x, p x) → (∀ x, r ∨ p x) :=
  by
    intro h x
    cases h with
    | inl r =>
      apply Or.inl
      exact r
    | inr r =>
      specialize r x
      apply Or.inr
      exact r

-- Tu pa nam bo v pomoč klasična logika
-- namig: `Classical.byContradiction` in `Classical.em` sta lahko v pomoč
open Classical
#check Classical.byContradiction
#check Classical.em

theorem eq5 : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
 by
  apply Iff.intro
  · intro h
    apply Classical.byContradiction
    intro npx
    apply h
    intro x
    apply Classical.byContradiction
    intro px
    apply npx
    exact ⟨x, px⟩
  · intro h1 h2
    obtain ⟨ x, notPx ⟩ := h1
    have h3 := h2 x
    contradiction

theorem eq6 : r ∨ (∀ x, p x) ↔ (∀ x, r ∨ p x) :=
  by
    apply Iff.intro
    · intro h x
      cases h with
      | inl r =>
        apply Or.inl
        exact r
      | inr h1 =>
        specialize h1 x
        apply Or.inr
        exact h1
    · intro h
      have h2 := Classical.em r
      cases h2 with
      | inl r =>
        apply Or.inl
        exact r
      | inr ner =>
        right
        intro x
        have xx := h x
        cases xx with
        | inl r => contradiction
        | inr px => exact px
