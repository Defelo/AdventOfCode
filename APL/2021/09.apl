input ← ↑⍎¨¨⊃⎕NGET '../.cache/2021/9' 1
low←{w←⍵⋄⊃∧/{w<⍵}¨(⍉9,⍉¯1↓⍵)(⍉9,⍨⍉1↓⍵)(9,⍉¯1↓⍉⍵)(9,⍨⍉1↓⍉⍵)}input
⎕ ← +/∊low×input+1
dfs ← {x v←⍵⋄(⊂x)∊v:v⋄⊃{~(⊂⍺)∊⍳⍴input:⍵⋄⍺⌷input≥9:⍵⋄dfs ⍺ ⍵}/(((x+⊢)¨(¯1 0)(1 0)(0 1)(0 ¯1)),⊂(v∪⊂x))}
⎕ ← ×/3↑{⍵[⍒⍵]}{≢dfs ⍵ (⍳0)}¨⍸low
