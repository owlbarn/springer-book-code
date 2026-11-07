"""Check the documented Docker/utop command through a real terminal."""
import errno
import fcntl
import os
import pty
import re
import select
import subprocess
import struct
import termios
import time


def check_repl(image, timeout=30):
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 160, 0, 0))
    command = ['docker', 'run', '--rm', '-it', '--network', 'none',
               '-e', 'TERM=xterm', image, 'utop']
    process = subprocess.Popen(command, stdin=slave, stdout=slave, stderr=slave)
    os.close(slave)
    output = b''
    stage = 0
    plain = b''
    deadline = time.monotonic() + timeout
    try:
        while True:
            if time.monotonic() >= deadline:
                raise TimeoutError('Docker/utop terminal check timed out:\n' + output.decode(errors='replace'))
            if not select.select([master], [], [], 0.2)[0]:
                continue
            try:
                chunk = os.read(master, 65536)
            except OSError as error:
                if error.errno == errno.EIO:
                    break
                raise
            if not chunk:
                break
            output += chunk
            plain = re.sub(rb'\x1b\[[0-?]*[ -/]*[@-~]', b'', output)
            if stage == 0 and b'utop # ' in plain:
                # Split the marker so echoed input cannot impersonate success.
                program = ('let () = if Mat.sum\' (Mat.ones 1 2) <> 2. then '
                           'failwith "array startup"; '
                           'ignore (Plot.create "/tmp/book-startup.png"); '
                           'print_endline ("BOOK_" ^ "STARTUP_OK");;\n')
                os.write(master, program.encode())
                stage = 1
            if stage == 1 and plain.rfind(b'utop # ') > plain.find(b'\nBOOK_STARTUP_OK\r\n') >= 0:
                os.write(master, b'#quit;;\n')
                stage = 2
        returncode = process.wait(timeout=5)
        text = plain.decode(errors='replace')
        if returncode or 'Error:' in text or 'Exception:' in text or '\nBOOK_STARTUP_OK\r\n' not in text:
            raise ValueError('Docker/utop terminal check failed:\n' + text)
        return {'command': command, 'terminal': 'pty', 'status': 'passed',
                'checks': ['default init', 'Owl matrix arithmetic', 'Owl_plplot handle']}
    finally:
        if process.poll() is None:
            process.terminate()
            process.wait(timeout=5)
        os.close(master)
