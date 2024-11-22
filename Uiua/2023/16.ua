&fras ⊣ &args

⊜∘≠@\n.

Dirs ← ↻1[∩¯,,⟜⇌⇡2]

GetBit ← ⬚0⊏⊙⋯
SetBit ← °⋯/↥⬚0⊟⊓°⊚⋯

HasFlag ← GetBit⊙⊡°⊂
SetFlag ← ⍜(⊙⊡)SetBit°⊂

Tiles ← "./\\|-"
Next ← ⨬(
  ¤
| ¤⨬(1|0|3|2)
| ¤⨬(3|2|1|0)
| ⨬(¤0|[0 2]|¤2|[0 2])
| ⨬([1 3]|¤1|[1 3]|¤3))

IsValid ← /××∩≥[.0].⊙(-1)↘1
Step    ← ▽:⟜≡IsValid⊙(¤△) ≡⊂⟜(+⊏⊙Dirs)⊙¤ Next⊗⊙Tiles :⊙:⊙⟜⊸⊡°⊂
Iter    ← ⨬(⊃(Step⊙◌)(⊙∘SetFlag)|↯0)◡HasFlag

Energize ← /+♭≠0 ⋅⊙◌⍢(⊂⊙Iter:°⊂|≠0⧻) ⊙⊸≡≡⋅0¤

Edge ← ⊂∩(⊂∩⍉) ∩⍜°⊂⊓(◿4-1)⇌ ∩(∩⊂2⊙0∩⊟0,⊣)◡: ∩⇡°⊟△

⊃(/↥≡Energize⊃Edge¤|Energize1_0_0)
