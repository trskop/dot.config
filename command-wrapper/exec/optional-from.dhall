  λ(a : Type)
→ λ(default : a)
→ λ(optional : Optional a)
→ Optional/fold a optional a (λ(_ : a) → _) default
