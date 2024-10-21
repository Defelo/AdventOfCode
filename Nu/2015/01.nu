use std

def main [input] {
  open --raw $input
  | split chars
  | each { match $in { '(' => 1, ')' => -1 } }
  | std iter scan 0 {|it, acc| $it + $acc }
  | tee { last | print }
  | std iter find-index {|x| $x == -1 }
}
