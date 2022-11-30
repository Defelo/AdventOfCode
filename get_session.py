#!/usr/bin/python

from Crypto.Cipher import AES
from Crypto.Protocol.KDF import PBKDF2
from pathlib import Path
import requests
import sqlite3
import re

key = PBKDF2("peanuts", b"saltysalt", 16, 1)


def chrome_decrypt(encrypted_value):
    dec = AES.new(key, AES.MODE_CBC, IV=b" " * 16).decrypt(encrypted_value[3:])
    decrypted = dec[: -dec[-1]].decode()
    return decrypted


conn = sqlite3.connect(str(Path("~/.config/BraveSoftware/Brave-Browser/Default/Cookies").expanduser().absolute()))
session = chrome_decrypt(
    conn.execute(
        "select encrypted_value from cookies where host_key=? and name=?;", [".adventofcode.com", "session"]
    ).fetchone()[0]
)
print(session)
with open(".session.txt", "w") as file:
    file.write(session)

if match := re.search(
    r'<div class="user">([^<]+)', requests.get("https://adventofcode.com", cookies={"session": session}).text
):
    print("logged in as", match[1].strip())
else:
    print("invalid token")
