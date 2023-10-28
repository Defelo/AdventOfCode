dots folds ← {⍵⊆⍨0≠≢¨⍵}⊃⎕NGET '../.cache/2021/13' 1
dots ← {⍎¨','(≠⊆⊢)⍵}¨dots
folds ← {c n←'='(≠⊆⊢)⊃⌽' '(≠⊆⊢)⍵⋄(⊃c)(⍎n)}¨folds
fold ← {c n←2⊃⍵⋄∪{x y←⍵⋄c≡'y':x(y⌊n+n-y)⋄(x⌊n+n-x)y}¨⊃⍵}
⎕ ← ≢fold dots (⊃folds)
dots ← ⊃{fold ⍵ ⍺}/(⌽folds),⊂dots
w h ← ⊃⌈/dots
⎕ ← ↑↑{,/⍵}¨↓{(⊂⌽⍵)∊dots:'##'⋄'  '}¨1-⍨⍳(h+1)(w+1)
