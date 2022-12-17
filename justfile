alias b := backup
alias r := run
alias t := test
alias c := clean
alias cc := clean_all
alias gs := get_session

_default:
    @just --list

# create backup of live.py
backup:
    @echo -e "\033[1mCreating backup of live.py\033[0m"; cp -v live.py $(date +"%Y%m%d_%H%M%S_live.py.bak")

# run live.py on examples and input.txt after creating a backup
live: backup test run

# run live.py on input.txt
run:
    @echo -e "\033[1mRunning on input.txt\033[0m"; python live.py input.txt

# run live.py on example.txt
test:
    @set -e; for file in example*.txt; do if [[ -s $file ]]; then echo -e "\033[1mRunning on $file\033[0m"; python live.py $file; fi; done

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
    PYTHONPATH=. hyperfine --shell sh {{args}} 'python {{year}}/{{day}}.py'

# benchmark python solutions using hyperfine
ypyh year *args:
    PYTHONPATH=. hyperfine --shell sh {{args}} "$(for f in {{year}}/*.py; do printf 'python '$f'; '; done)"

# run rust solution
rs year day *args:
    cargo run --bin {{year}}_{{day}} -- {{args}}

# run rust solution in release mode
rsr year day *args:
    cargo run --release --bin {{year}}_{{day}} -- {{args}}

# test rust solution
rst year day:
    cargo test --bin {{year}}_{{day}}

# benchmark rust solution using criterion
rsb year day *args:
    cargo bench --bench {{year}}_{{day}} {{args}}

# benchmark rust solution using hyperfine
rsh year day *args:
    cargo build --release --bin {{year}}_{{day}}
    hyperfine --shell sh {{args}} 'target/release/{{year}}_{{day}}'

# benchmark rust solutions using hyperfine
yrsh year *args:
    cargo build --release
    hyperfine --shell sh {{args}} "$(for f in target/release/{{year}}_*; do if [[ -x $f ]]; then printf ./$f'; '; fi; done)"
