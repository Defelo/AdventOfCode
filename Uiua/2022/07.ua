&fras ⊣ &args

⊜□≠@\n.

CdRoot   ← ↙0
CdParent ← ↘¯1
CdChild  ← ⊂:□
File     ← ⊃⋅⋅∘(insert:⊙(□⊂:□))⋕

⊙({}map.[])
◌∧◇⍣(
  CdRoot °$"$ cd /"
| CdParent °$"$ cd .."
| CdChild°$"$ cd _"
| °$"$ ls"
| ◌°$"dir _"
| File °$"_ _")

°map⊙(map.[])
∧◇(∧(insert⊙+:◡(⬚0get⊙◌))⍚↙⇡⊃⧻¤)

A ← /+×≤100000.
B ← /↧▽⊸≥-:30000000-:70000000⊸get□{}

⊃B A
