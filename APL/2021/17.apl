(x1 x2) (y1 y2) ← {⍎¨'.'(≠⊆⊢)⊃','(≠⊆⊢)⍵}¨1↓'='(≠⊆⊢)⊃⊃⎕NGET '../.cache/2021/17' 1
s ← {w←{x y vx vy←⍵⋄(x>x2)∨(y<y1):⍬⋄(⊂x y),∇(x+vx)(y+vy)(vx-×vx)(vy-1)}(0 0),⍵⋄(⊃⌈⌿⌽↑w)(⊃∧/{((x2 y2)≥⍵)∧(x1 y1)≤⍵}⊃⌽w)}
⎕ ← ⌈/∊{vy←⍵⋄{vx←⍵⋄y k←s vx vy⋄k×y}¨⍳x2}¨1-⍨y1+⍳500+1-y1
⎕ ← +/∊{vy←⍵⋄{vx←⍵⋄y k←s vx vy⋄k}¨⍳x2}¨1-⍨y1+⍳500+1-y1