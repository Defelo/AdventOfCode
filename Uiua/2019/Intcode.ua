Parse ← map⇡⊸⧻ ⊜⋕∊⊂@-+@0⇡10.

# Op <inst>
Op ↚ ⍤.≥0.◿100

# Mode <arg> <inst>
Mode ↚ ◿10⌊÷ⁿ⊙10+2

# Read <arg> <ip> <base> <mem>
Read ↚ ⬚0⨬(get⊙◌|∘|get+) ⊃(Mode⊙(get|◌)|get++1⊙⊙◌|⋅⋅⊙∘)

# Write <val> <arg> <ip> <base> <mem>
Write ↚ insert:⊙(⨬(⊙◌|⍤.0|+) ⊃(Mode⊙(get|◌)|get++1⊙⊙◌|⋅⋅⊙∘))

# ...Op <ip> <base> <mem>
BinOp! ↚ +4⊃⋅⋅⊙∘Write⊙2◡(^0⊃(Read1|Read0))
OpAdd  ↚ BinOp!+
OpMul  ↚ BinOp!×

Input!  ↚ +2⊃⋅⋅⊙∘Write⊃(⋅⋅⋅∘|0|⊙⊙∘) ⊙⊙⊙^0
Output! ↚ +2 ⊙⊙⊙^0 ⊃(⋅⊙⊙∘|⊙⋅⋅◌) ◡(Read0)

Jmp!     ↚ ⨬(+3◌|⊙◌)^0◡⊃(Read0|Read1)
JmpTrue  ↚ Jmp!(≠0)
JmpFalse ↚ Jmp!(=0)

Lt ↚ BinOp!<
Eq ↚ BinOp!(=)

Base ↚ +2⊙+:◡(Read0)

Step‼  ↚ ⨬(⊙⊙⊙∘|OpAdd|OpMul|Input!^0|Output!^1|JmpTrue|JmpFalse|Lt|Eq|Base)Op◡(get⊙◌)
IsDone ↚ ≍99get⊙◌

Run‼ ← ◌◌⍢(Step‼^0^1|¬IsDone)0 0
