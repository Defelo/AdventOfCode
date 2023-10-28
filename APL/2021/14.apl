input ← ⊃⎕NGET '../.cache/2021/14' 1
⎕PP ← 32
template ← ⊃input
rules ← {(⍵⌷⍨⊢)¨1 2 7}¨2↓input
gr ← {k←⍵⋄{⊃⌽⊃⍵/⍨{k≡2↑⍵}¨⍵}rules}
next ← {{w←⍵⋄∪{k←⍵⋄⍵ (+/{(2⊃⍵)×k≡⊃⍵}¨w)}¨{⊃⍵}¨w}⊃,/{(a b) c←⍵⋄x←gr ⊃⍵⋄((a x)c)((x b)c)}¨⍵}
solve ← {{(⌈/⍵)-(⌊/⍵)}⊃⌽↓⍉{a b←⍵⋄({⊃⌽⍵}¨a) {⍺,+/⍵}⌸ b}↓⍉↑ (⊂((1↑template) 1)),(next⍣⍵) ↓{(⊃⍺)(≢⍵)}⌸2,/template}
⎕ ← solve 10
⎕ ← solve 40
