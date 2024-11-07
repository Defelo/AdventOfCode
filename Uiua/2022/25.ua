&fras ⊢⇌ &args

Digits    ← "=-012"
ToSnafu   ← ⊏⊙Digits [◌⍢(⌊⊃÷◿5+2|>0)]
FromSnafu ← ∧(+×5:)⊙0 -2⊗⊙Digits

&p ToSnafu /+ ⊜FromSnafu≠@\n.
