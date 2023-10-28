input ← ↑⍎¨¨⊃⎕NGET '../.cache/2021/15' 1
solve←{d←⍵⋄h w←⍴d⋄max←+/∊d⋄⊃⌽∊({⊃⌊/⍵ (d+{⍉max,⍉¯1↓⍵}⍵)(d+{⍉max,⍨⍉1↓⍵}⍵)(d+{max,⍉¯1↓⍉⍵}⍵)(d+{max,⍨⍉1↓⍉⍵}⍵)}⍣(2×w+h))h w⍴0,(1-⍨w×h)⍴max}
⎕ ← solve input
⎕ ← solve ⍉⊃,/⍉¨,⌿⍉{1+⍨9|1-⍨input++/⍵}¨1-⍨⍳5 5
