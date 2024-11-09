&fras ⊢⇌ &args

⊜⋕∈+@0⇡10.

BinOp! ← +4⟜(⍜⊏◌⊸⊏+3⊙::◡(/^0⊏⊸⊏++1⇡2))
OpAdd  ← BinOp!+
OpMul  ← BinOp!×

Step   ← ⨬(OpAdd|OpMul)-1◡⊏
IsDone ← =99⊏

Run ← ◌ ⍢(Step|¬IsDone) 0

RunWithInput ← ⊢Run⍜⊏◌1_2:

⊃(⊗19690720≡RunWithInput☇1⇡100_100¤|RunWithInput 12_2)
