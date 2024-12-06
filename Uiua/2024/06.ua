&fras ⊣ &args

⊜∘≠@\n.

Init  ← ¯1_0⊢⊚=@^
Step  ← ⨬(⟜+|⇌⍜⊢¯)=@#◡(⬚@.⊡+)
Valid ← /××∩>¯1.⊙△

map.[] ⊸Init
&p⊸⧻ ⊙⋅◌⍢(⊃(insert⊙0:⊙◌|⋅Step)|⋅⋅Valid)

SkipForward ← ⍢(⟜+|×⊃(⋅Valid|≠@#⬚@.⊡+))
CheckCycle ← (
  map.[] ⊸Init
  ⍢(⊃(insert⊙0:⊙⊂|⇌⍜⊢¯SkipForward◌)|¬has□:⊙⊂)
  Valid⋅◌
)

▽=@.◡⊡⊙◌°map
&p /+≡wait≡pool(CheckCycle⍜⊡⋅@#)⊙¤
