input ← ⍎¨¨⊃⎕NGET '../.cache/2021/11' 1
up←{⍉0,⍨⍉1↓⍵}
down←{⍉0,⍉¯1↓⍵}
left←{0,⍨⍉1↓⍉⍵}
right←{0,⍉¯1↓⍉⍵}
ff←{10≠⍵}×{10⌊⍵+(⍵>0)∧(⊢+left+right)(⊢+up+down)10=⍵}
step←{{a b←⍵⋄a≡b:a⋄∇(ff a)a}1+⍵ 0}
⎕ ← {d n←⍵⋄n>100:0⋄(⊂+/0=∊d)+∇(step d)(n+1)}(↑input) 0
⎕ ← {d n←⍵⋄~∨/∊d:n⋄∇(step d)(n+1)}(↑input) 0
