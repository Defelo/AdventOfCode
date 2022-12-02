alias b := backup
alias r := run
alias c := clean
alias cc := clean_all
alias gs := get_session

_default:
    @just --list

# create backup of live.py
backup:
    cp live.py $(date +"%Y%m%d_%H%M%S_live.py.bak")

# run live.py after creating a backup
run: backup
    python live.py

# restore live.py and remove input
clean:
    git restore live.py
    rm -f input.txt

# restore live.py, remove all backups of it and remove input
clean_all: clean
    rm -f *_live.py.bak

# extract session cookie from brave
get_session:
    python get_session.py

# run python solution
py year day:
    PYTHONPATH=. python {{year}}/{{day}}.py

# benchmark python solution using hyperfine
pyh year day *args:
    PYTHONPATH=. hyperfine {{args}} 'python {{year}}/{{day}}.py'

# run rust solution
rs year day:
    cargo run --bin {{year}}_{{day}}

# run rust solution in release mode
rsr year day:
    cargo run --release --bin {{year}}_{{day}}

# benchmark rust solution using criterion
rsb year day *args:
    cargo bench --bench {{year}}_{{day}} {{args}}

# benchmark rust solution using hyperfine
rsh year day *args:
    cargo build --release --bin {{year}}_{{day}}
    hyperfine {{args}} 'target/release/{{year}}_{{day}}'
