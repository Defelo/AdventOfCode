input ← ⊃⎕NGET '../.cache/2021/3' 1
⎕ ← ×/⊃{⍺+2×⍵}/⌽(⊢,~)∘{(≢⍵)≤2×+/⍎¨⍵}¨↓⍉↑ input
⎕ ← ×/{2⊥⍎¨⍵}¨{x←⍵⋄⊃⊃{i←⍺⋄m←{(x+1)⊃⍵,~⍵}{(≢⍵)≤2×+/⍎¨⍵}i⊃↓⍉↑⍵⋄o←({m=⍎i⊃⍵}¨⍵)/⍵⋄(1+0=≢o)⊃o ⍵}/⌽(⊂input),⍳≢⊃input}¨0 1
