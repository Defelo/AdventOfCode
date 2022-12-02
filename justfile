alias b := backup
alias r := run
alias c := clean
alias cc := clean_all
alias gs := get_session

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

py year day:
    PYTHONPATH=. python {{year}}/{{day}}.py

pyh year day *args:
    PYTHONPATH=. hyperfine {{args}} 'python {{year}}/{{day}}.py'

rs year day:
    cargo run --bin {{year}}_{{day}}

rsr year day:
    cargo run --release --bin {{year}}_{{day}}

rsb year day *args:
    cargo bench --bench {{year}}_{{day}} {{args}}

rsh year day *args:
    cargo build --release --bin {{year}}_{{day}}
    hyperfine {{args}} 'target/release/{{year}}_{{day}}'
