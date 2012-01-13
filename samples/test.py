#!/usr/bin/python

# Testing framework for DietLISP, based on llvm-lit.

import os
import string
import subprocess
import sys

def combine_strings(strs):
    if len(strs) == 0:
        return ''
    
    s = strs[0]
    add_newline = True

    if s.endswith('\\') and not s.endswith('\\\\'):
        s = s[:-1].rstrip()
        add_newline = False

    combined = s

    for s in strs[1:]:
        prev_add_newline = add_newline
        if s.endswith('\\') and not s.endswith('\\\\'):
            s = s[:-1].rstrip()
            add_newline = False
        else:
            add_newline = True

        if prev_add_newline:
            combined = combined + '\n' + s
        else:
            combined = combined + s
    return combined

def process_output(str):
    return str.replace('\\b', ' ').replace('\\n', '\n')

def parse(file_obj):
    command = None
    outputs = []
    command_prefix = ';; RUN:'
    output_prefix = ';; EXPECT:'

    for line in file_obj:
        s_line = line.strip()
        if s_line.startswith(command_prefix):
            if command is None:
                command = s_line[len(command_prefix):].lstrip()
            else:
                print 'Multiple `RUN` strings.'
                return None
        elif s_line.startswith(output_prefix):
            op = s_line[len(output_prefix):].lstrip()
            outputs.append(process_output(op))

    if command is None:
        print 'Could not find `RUN` string.'
        return None
    return (command, combine_strings(outputs))

def test(command, output):
    try:
        pipe = subprocess.Popen([command], shell=True, stdout=subprocess.PIPE)
        pipe.wait()
        to_test = pipe.stdout.read().strip()
        if to_test == output:
            return True
        print 'DietLISP FAIL!'
        print '**************\n'
        print 'Expected output: \n'
        print output
        print '\nOutput received: \n'
        print to_test
        print ''
    except OSError:
        print 'Failed to execute command `' + command + '`'
        return False

def process_file(file_name):
    if file_name[:3].lower () == 'dnt':
        print 'Skipping ' + file_name
        return
    print 'Testing `' + file_name + '`'
    try:
        f = open(file_name, 'r')
    except IOError:
        print 'Could not open file: `' + file_name + '`'
        return
    
    p = parse(f)
    f.close()

    if p is None:
        print 'Error parsing file `' + file_name + '`'
        return

    (cmd, output) = p
    cmd = cmd.replace('<file>', file_name)

    result = test(cmd, output)
    if not result:
        print 'Failed test `' + file_name + '`'

def usage():
    print 'Usage: ' + sys.argv[0] + ' [--all] [file_name]'

def main():
    if len(sys.argv) < 2:
        usage()
        return
    if sys.argv[1] == '--all':
        for filename in os.listdir(os.getcwd()):
            if filename.endswith('.dlisp'):
                process_file(filename)
    else:
        process_file(sys.argv[1])

if __name__ == "__main__":
    main()
