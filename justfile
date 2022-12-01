alias b := backup
alias r := run
alias c := clean
alias cc := clean_all
alias gs := get_session
alias d := day

_default:
    @just --list

backup:
    cp live.py $(date +"%Y%m%d_%H%M%S_live.py.bak")

run: backup
    python live.py

clean:
    git restore live.py
    rm -f input.txt

clean_all: clean
    rm -f *_live.py.bak

get_session:
    python get_session.py

day year day:
    PYTHONPATH=. python {{year}}/{{day}}.py
