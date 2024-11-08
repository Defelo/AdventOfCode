&fras ⊢⇌ &args

ParseInst ← ⊃↘↙2 ↘1⊢regex"^([RDLU]) (\\d+) \\(#(.{5})([0123])\\)$"
ParseHex  ← ∧(+×16:)⊙0⊗⊙"0123456789abcdef"
ParseA    ← ≡⍜°⊟⊓(⊗⊙"RDLU"◇⊢|⋕)
ParseB    ← ≡(⍜°⊟⊓(⋕|ParseHex°□)⇌)
Solve ← (
  ⊙(0 0 0)
  ◌∧(⊃(⨬(+|◌|-|◌)|⨬(◌|+|◌|-)⊙×|+⋅⊙⋅⋅∘)°⊟)
  +1+÷2:
)

⊜ParseInst≠@\n.
∩Solve ⊓ParseB ParseA
