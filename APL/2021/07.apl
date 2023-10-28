input ← ⍎¨','(≠⊆⊢)⊃⊃⎕NGET '../.cache/2021/7' 1
⎕ ← ⌊/{+/|¨⍵-input}¨⍳≢input
⎕ ← ⌊/{+/(+/⍳)¨|¨⍵-input}¨⍳≢input
