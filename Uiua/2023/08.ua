&fras ⊣ &args

ParseInstructions ← =@R°□⊢
ParseNode         ← ↘1⊢regex"([^ ,]+) = \\(([^ ,]+), ([^ ,]+)\\)"
ParseNetwork      ← ≡ParseNode ↘1
ReplaceNames      ← ⍜⍉(⊗⊃(↘1)⊢)
FindStartAndGoal  ← ∩(⊗□) "AAA","ZZZ"⊢⍉
StartAndGoalMask  ← ∩=@A,@Z≡◇(⊣)⊢⍉
Step              ← ⊃(⊏⊃(⊏◿⧻,⋅⊙⋅⋅∘|⊏⊙⋅⋅∘)|+1⋅∘|⋅⋅⊙⊙∘)
Run!              ← ⋅⊙⋅⋅◌ ⍢Step(¬^0) ⊙0
LCM               ← ÷◌⊃⍢⊃◿∘±×
A                 ← Run!(=⊙⋅∘) FindStartAndGoal
B                 ← /LCM ≡(Run!(⊏⊙⋅∘)) ⊓(⊚|¤|¤|¤) StartAndGoalMask

⊜□≠@\n.
⊃ParseNetwork ParseInstructions
⊃B A ⟜ReplaceNames
