#!/usr/bin/env bash

set -ex

target=$(cargo metadata --format-version=1 | jq -r .target_directory)
cargo build -r
cp "$target/release/libutils.so" .
