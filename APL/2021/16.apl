input ← ⊃,/{⍵⊤⍨4⍴2}¨1-⍨('0123456789ABCDEF'⍸⊢)¨⊃⊃⎕NGET '../.cache/2021/16' 1
⎕PP ← 32
read ← {a b←⍵⋄2⊥input[1-⍨a+⍳b]}
rl ← {i x←⍵⋄c←read i 1⋄x←(x×16)+read(i+1)4⋄~c:(i+5)x⋄∇(i+5)x}
gv0 ← {{i c←⍵⋄i≥c:i 0 ⍬⋄i v x←solve i⋄i vs xs←∇i c⋄i(v+vs)(x,xs)}(⍵+15)(⍵+15+read ⍵ 15)}
gv1 ← {{i c←⍵⋄c≤0:i 0 ⍬⋄i v x←solve i⋄i vs xs←∇i(c-1)⋄i(v+vs)(x,xs)}(⍵+11)(read ⍵ 11)}
gv ← {0=read ⍵ 1:gv0 (⍵+1)⋄gv1 (⍵+1)}
op ← {t x←⍵⋄t=0:⊃+/x⋄t=1:⊃×/x⋄t=2:⊃⌊/x⋄t=3:⊃⌈/x⋄t=5:(⊃x)>2⊃x⋄t=6:(⊃x)<2⊃x⋄t=7:(⊃x)=2⊃x}
solve ← {i←⍵⋄v←read i 3⋄i←i+3⋄t←read i 3⋄i←i+3⋄t=4:{i x←⍵⋄i v x}rl i 0⋄i vs xs←gv i⋄i(v+vs)(op t xs)}
_ part1 part2 ← solve 1
⎕ ← part1
⎕ ← part2
