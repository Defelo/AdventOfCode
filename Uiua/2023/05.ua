&i "../lib.ua" "Load"

ParseSeeds ← ≡parse↘1⊜□≠@ .⊔⊢
ParseRangeMap ← ⍜⍘[⊙⊙∘]⊃(-:|⋅∘|⋅+)⊜parse≠@ .
ParseMaps ← ≡(□≡⊐ParseRangeMap↘1⊜□≠@\n.⊔) ↘1
Contains ← ≍1_0≤
FindRangeMap ← ⊢⊚⊂⊙1 ≡Contains ⊙⍜⍉(↘1)
MapNum ← +⊡⊃FindRangeMap(⊂⊙0⊢⍉:)
SeedRanges ← ⍜(⍘⊟⍉)⊃∘+ ↯¯1_2
Intersect ← ⊟⊓/↥/↧⍜⊟⍉
IsEmpty ← /≤
SplitAt ← ⊃(↧⊂∞)(↥⊂:¯∞)
MapSeedRange ← (
  ¤+⊙(⊙⊟ :⊙Intersect ∩SplitAt ⊙,) ⍘[⊙⊙∘]:
  ∩(▽¬≡IsEmpty.)
)
FlatMapSeedRanges ← ;∧(⊃⋅∘(∩⊂⊙::MapSeedRange)) ⊙⊙(.↯0_2[])
MapSeedRanges ← ⊂∧(⊙⊂FlatMapSeedRanges:) :⊙⊙(↯0_2[])
A ← /↧≡(∧(MapNum:⊔):) ⊙¤
B ← /↧⊢⍉ ∧(MapSeedRanges:⊔) :SeedRanges

⊃B A ⊃ParseSeeds ParseMaps ⊜□¬⌕"\n\n".
