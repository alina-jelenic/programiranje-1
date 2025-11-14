-- Strukture:

-- (A x B) ^ C <=> A ^ C x B ^ C
def eksponent (A B C : Type) (f : C → Prod A B) : Prod (C → A) (C → B) :=
  ⟨
    (fun c => (f c).1),
    (fun c => (f c).2)
  ⟩
def eksponent_prop (A B C : Prop) (f : C → A ∧ B) : (C → A) ∧ (C → B) :=
  ⟨
    fun c => (f c).1,
    fun c => (f c).2
  ⟩
def eksponent_prop_s_taktikami (A B C : Prop) (f : C → A ∧ B) : (C → A) ∧ (C → B) :=
  by
    apply And.intro
    · intro hc
      exact (f hc).1
    · intro hc
      have h1 : A ∧ B := f hc
      exact h1.2



-- ------------------------------
-- Logika

theorem eq1 {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  by
    apply Iff.intro
    · intro h
      apply And.intro
      exact h.2
      exact h.1
    · intro h
      apply And.intro
      exact h.2
      exact h.1

theorem eq2 {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
  by
    apply Iff.intro
    · intro h
      cases h with
      | inl ha =>
          apply Or.inr
          exact ha
      | inr hb =>
          apply Or.inl
          exact hb
    · intro h
      cases h with
      | inl ha =>
          apply Or.inr
          exact ha
      | inr hb =>
          apply Or.inl
          exact hb

theorem eq3 {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) :=
  by
    apply Iff.intro
    . intro h
      apply And.intro
      . exact h.right.left
      . apply And.intro
        . exact h.left
        . exact h.right.right
    . intro h
      apply And.intro
      . exact h.right.left
      . apply And.intro
        . exact h.left
        . exact h.right.right


theorem eq4 {A B C : Prop} : (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C)) :=
 by
  apply Iff.intro
  · intro h
    cases h with
    | inl ha => exact Or.inr (Or.inl ha)
    | inr bc =>
        cases bc with
        | inl b => exact Or.inl b
        | inr c => exact Or.inr (Or.inr c)
  · intro h
    cases h with
    | inl b => exact Or.inr (Or.inl b)
    | inr ac =>
        cases ac with
        | inl a => exact Or.inl a
        | inr b => exact Or.inr (Or.inr b)

theorem eq5 {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) :=
  by
    apply Iff.intro
    · intro h
      cases h.right with
      | inl hb => exact Or.inl (And.intro h.left hb)
      | inr hc => exact Or.inr (And.intro h.left hc)
    · intro h
      cases h with
      | inl ab => exact And.intro ab.left (Or.inl ab.right)
      | inr ac => exact And.intro ac.left (Or.inr ac.right)


theorem eq6 {A B C : Prop} : (B ∨ C) → A ↔ (B → A) ∧ (C → A) :=
  by
    apply Iff.intro
    · intro h
      constructor
      · intro hb
        apply h
        left
        exact hb
      · intro hc
        apply h
        right
        exact hc
    · intro h hbc
      cases hbc with
      | inr hc => exact h.right hc
      | inl hb => exact h.left hb

theorem eq7 {A B C : Prop} : C → (A ∧ B) ↔ (C → A) ∧ (C → B) :=
  by
    apply Iff.intro
    · intro h
      constructor
      intro c
      exact (h c).1
      intro c
      exact (h c).2
    · intro h c
      constructor
      exact h.1 c
      exact h.2 c

-- ------------------------------
-- Enakosti naravnih števil (z uporabo `calc`)

theorem kvadrat_dvoclenika {a b : Nat} : (a + b)^2 = a^2 + 2 * a * b + b^2 :=
  by
    calc
      (a + b)^2
      _ = (a + b) * (a + b) := by rw [Nat.pow_two]
      _ = a * (a + b) + b * (a + b) := by rw [Nat.add_mul]
      _ = a * a + a * b + b * (a + b) := by rw [Nat.mul_add]
      _ = a * a + a * b + (b * a + b * b) := by rw [Nat.mul_add]
      _ = a * a + a * b + b * a + b * b := by repeat rw [Nat.add_assoc]
      _ = a * a + a * b + a * b + b * b := by rw [Nat.mul_comm b a]
      _ = a^2 + a * b + a * b + b^2 := by repeat rw [Nat.pow_two]
      _ = (a^2 + (a * b + a * b)) + b^2 := by rw [Nat.add_assoc (a^2)]
      _ = a^2 + 2 * (a * b) + b^2 := by rw [Nat.two_mul]
      _ = a^2 + 2 * a * b + b^2 := by repeat rw[Nat.mul_assoc]


theorem vsota_eksponent_produkta {a b c d : Nat} : (a * b)^(c + d) = (a^c)*(a^d)*(b^c)*(b^d) :=
  by
    calc
      (a * b)^(c + d)
      _ = (a * b)^(c) * (a * b)^d := by rw[Nat.pow_add]
      _ = a^c * b^c * (a^d  * b^d) := by repeat rw [Nat.mul_pow]
      _ = a^c * (b^c * a^d)  * b^d := by repeat rw [Nat.mul_assoc]
      _ = (a^c * (a^d * b^c)) * b^d := by rw [Nat.mul_comm (b^c) (a^d)]
      _ = ((a^c * a^d) * b^c) * b^d := by rw [← Nat.mul_assoc]
      _ = a^c * a^d * b^c * b^d := by rfl
