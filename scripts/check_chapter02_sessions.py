#!/usr/bin/env python3
"""Run the canonical book REPL inputs; the OCaml toplevel can exit zero on errors."""
import json
import re
import subprocess
from pathlib import Path

root=Path(__file__).resolve().parents[1]
chapter=root/'code/chap02_maths'
program='#use "topfind";;\nTopfind.log := (fun _ -> ());;\n#require "owl";;\nopen Owl;;\n'
for name in ('book_algorithms.ml','book_gaussian.ml'):
    program+='#mod_use '+json.dumps(str(chapter/name))+';;\n'
program+=(chapter/'sessions/chapter02.ml').read_text()
result=subprocess.run(['ocaml','-noinit','-noprompt','-nopromptcont'],input=program,text=True,capture_output=True)
if result.returncode or re.search(r'\bError:|\bException:',result.stdout+result.stderr):
    raise SystemExit(result.stdout+result.stderr)
print('PASS: canonical Chapter 2 REPL inputs parsed and executed')
