#!/usr/bin/env python3
"""Compile the same canonical book program; omit toplevel-only introspection."""
import re
import sys
from pathlib import Path
text=Path(sys.argv[1]).read_text()
# These directives affect package loading, printers and introspection only.
allowed=re.compile(r'^#(?:use "topfind"|require "(?:owl|owl-plplot)"|install_printer book_matrix_printer|show_type [\w.]+);;$')
lines=[]
for line in text.splitlines():
    if line.startswith('#'):
        if not allowed.fullmatch(line):raise ValueError('Unexpected directive: '+line)
        continue
    if line.startswith('Topfind.log :='):continue
    lines.append(line)
print('\n'.join(lines))
