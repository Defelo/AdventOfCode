input ← '-'(≠⊆⊢)¨⊃⎕NGET '../.cache/2021/12' 1
next ← {w←⍵⋄⊃{a b←⍺⋄a≡w:⍵,⊂b⋄b≡w:⍵,⊂a⋄⍵}/input,⊂⍬}
search ← {p v s←⍵⋄p≡'end':1⋄t←(∧/97≤⎕UCS p)∧∨/(p≡⊢)¨v⋄t∧s∨p≡'start':0⋄+/{search ⍵ (v∪⊂p) (t∨s)}¨next p}
⎕ ← search 'start' ⍬ 1
⎕ ← search 'start' ⍬ 0
