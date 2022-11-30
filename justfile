alias r := run
alias c := clean
alias gs := get_session

_default:
    @just --list

run:
    python live.py

clean:
    git restore live.py
    rm -f input.txt

get_session:
    python get_session.py
