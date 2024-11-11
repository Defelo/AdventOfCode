Parse ← ⊜⋕∈⊂@-+@0⇡10.

# Op <inst>
Op ↚ ⍤.≥0.◿100

# Param <arg> <inst>
Param ↚ ◿10⌊÷ⁿ⊙10+2

# Read <arg> <ip> <mem>
Read ↚ ⨬(⊏|∘) ⊸⊃(Param⊙⊏|⊏++1)

# Write <val> <arg> <ip> <mem>
Write ↚ ⍜⊏◌⊃(⊏++1◌|:⊙⋅⋅∘|⍤.=0Param⊙⊏◌)

# ...Op <ip> <mem>
BinOp! ↚ +4⊃⋅⋅∘Write⊙2◡(^0⊃(Read1|Read0))
OpAdd  ↚ BinOp!+
OpMul  ↚ BinOp!×

Input!  ↚ +2⊃⋅⋅∘Write⊙0:⊙:⊙⊙^0
Output! ↚ +2⊙⊙^0 ⊙:: ◡(Read0)

Jmp!     ↚ ⨬(+3◌|⊙◌)^0◡⊃(Read0|Read1)
JmpTrue  ↚ Jmp!(≠0)
JmpFalse ↚ Jmp!(=0)

Lt ↚ BinOp!<
Eq ↚ BinOp!(=)

Step‼  ↚ ⨬(⊙⊙∘|OpAdd|OpMul|Input!^0|Output!^1|JmpTrue|JmpFalse|Lt|Eq)Op◡⊏
IsDone ↚ ≍99⊏

Run‼ ← ◌⍢(Step‼^0^1|¬IsDone)0
