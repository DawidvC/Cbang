#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import os, os.path
import sys

import shutil
import tempfile
import subprocess

config = {
    'BASIC_TESTS': 'tests/',
    'ADVANCED_TESTS': 'AdvancedTest/',

    'CBC': 'Compiler/cbc',
    'CBCFLAGS': ('-stdpath ' + os.getcwd() + '/stdlib').split(),

    'CC': 'gcc',
    'CFLAGS': ('-Wall -Wextra -std=gnu99 -O2 -Wno-pointer-sign -I ' + os.getcwd() + '/stdlib').split(),

    'LDFLAGS': [],

    'MAKE': 'make',
}

flags = {
        'print-ok': 'Display successful tests',
        'fail-details': 'Display output on errors',
        'cb-only': 'Test C! compilation, but not C compilation',
        'no-colors': 'Use colors for a nice display',
        'force-colors': 'Force the use of colors, even if stdout is not a tty',
        'try-fails': 'Try the C compilation of C! succeeded fails'
}
parameters = dict((flag, False) for (flag, doc) in flags.items())
for arg in sys.argv[1:]:
    if arg.startswith('-'):
        flag = arg[1:]
        if flag in flags:
            parameters[flag] = True
            continue
    print(
        '%s: invalid argument \'%s\'' % (
            sys.argv[0], arg
        ),
        file=sys.stderr
    )
    print(
        'Usage: %s %s' % (
            sys.argv[0], ' '.join('[-%s]' % f
                for (f, _) in sorted(flags.items()))
        ), file=sys.stderr
    )
    for (flag, doc) in sorted(flags.items()):
        print(
            '  %s: %s' % (flag.rjust(12), doc)
        )
    sys.exit(1)

# Override default configuration using environment
for name in os.environ:
    if not os.environ[name].strip():
        continue
    if name in config:
        config[name] = (
            os.environ[name]
            if not isinstance(config[name], list)
            else os.environ[name].split()
        )

# Move to the script directory
os.chdir(os.path.abspath(os.path.dirname(sys.argv[0])))
config['CBC'] = os.path.abspath(config['CBC'])

BOLD = '01'
RED = '31'
GREEN = '32'
YELLOW = '33'
GRAY = '37'
def use_color(text, *colors):
    '''Decorate the text with a color using xterm-color format.'''
    if not parameters['force-colors'] and \
        (parameters['no-colors'] or not os.isatty(sys.stdout.fileno())):
        return text
    return '\x1b[%sm%s\x1b[0m' % (';'.join(colors), text)

def flatten_environ(environ):
    result = {}
    for name, value in environ.items():
        if isinstance(value, list):
            value = ' '.join(value)
        result[name] = str(value)
    return result

class Env:
    def __init__(self):
        self.compile_dir = tempfile.mkdtemp(prefix='cbc-test-')
        self.produced_files = set()

        self.cb_tests = []
        self.c_tests = []

        self.current_source = None
        self.title_printed = False
        # Stores the error logs temporarily
        self.logs = None
        self.notes = []

        self.tests_passed = 0
        self.tests_failed = 0
        self.returncode = 0

    def process_module(self, dirpath, cb_tests):
        for filename in self.produced_files:
            try:
                os.remove(filename)
            except:
                pass
        self.produced_files = set()
        self.cb_tests = cb_tests
        self.c_tests = []
        print(use_color('Testing %s...' % dirpath, BOLD))
        for cb_test in self.cb_tests:
            self.new_source(cb_test.source)
            cb_test.process()
        print('  --')
        for c_test in self.c_tests:
            self.new_source(c_test.source)
            c_test.process()

    def append_cb_test(self, cb_test):
        self.cb_tests.append(cb_test)
    def append_c_test(self, c_test):
        self.c_tests.append(c_test)

    def new_source(self, source):
        self.flush_notes()
        self.current_source = source
        self.title_printed = False
        self.logs = ''
        self.notes = []

    def try_compile(self, args, should_succeed):
        compilation = subprocess.Popen(
            args,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            cwd=self.compile_dir
        )
        (stdout, stderr) = compilation.communicate()
        stdout = stdout.decode(errors='ignore')
        stderr = stderr.decode(errors='ignore')
        indentation = ' ' * 4
        def indent(msg):
            return '\n'.join((
                '%s%s' % (indentation, line) for line in msg.split('\n')
            ))
        if 'obscure internal error' in (stdout + stderr).lower():
            self.add_note('Obscure infernal error detected!')
        if parameters['fail-details'] and \
            ((parameters['print-ok'] or should_succeed) and \
                compilation.returncode != 0) or \
            (parameters['try-fails'] and \
                not should_succeed and compilation.returncode != 0):
            self.logs += '%sstdout:\n%s\n' % (
                indentation,
                use_color(indent(stdout), GRAY))
            self.logs += '%sstderr:\n%s\n' % (
                indentation,
                use_color(indent(stderr), GRAY))
        return should_succeed == (compilation.returncode == 0)

    def print_result(self, source, success, was_scheduled=True):
        if was_scheduled:
            self.tests_passed += 1
            status = use_color('  OK', GREEN, BOLD)
            if not success:
                self.tests_failed += 1
                status = use_color('FAIL', RED, BOLD)
                self.returncode = 2
        else:
            if success:
                status = use_color('CMP-OK', GRAY, BOLD)
            else:
                status = use_color('CMP-FAIL', GRAY, BOLD)
        if not was_scheduled or not success or parameters['print-ok']:
            print('  ' + status, source)
            self.title_printed = True
            self.flush_logs()
        self.flush_notes()

    def print_opt(self, message, option):
        print(use_color('     %s [-%s]' % (message, option), GRAY))

    def flush_logs(self):
        if self.logs and parameters['fail-details']:
            print(self.logs)
        self.logs = ''

    def flush_notes(self):
        if len(self.notes) == 0:
            return
        prefix = use_color('Warning: ', YELLOW, BOLD)
        if not self.title_printed:
            print('  ' + use_color('WARN', YELLOW, BOLD),
                    self.current_source)
            prefix = ''
            self.title_printed = True
        for note in self.notes:
            print(' ' * 7 + prefix + note)
        self.notes = []

    def add_note(self, message):
        self.notes.append(message)

    def clean(self):
        shutil.rmtree(self.compile_dir)

    def summary(self):
        print()
        print(use_color(
            'Results: %d / %d failed tests' % (
                self.tests_failed,
                self.tests_passed
            ), BOLD)
        )
        self.tests_failed = 0
        self.tests_passed = 0

class CBTest:
    def __init__(self, env, cb_source):
        self.env = env
        self.cb_source = cb_source
        self.source = os.path.basename(cb_source)

    def process(self):
        cb_base = os.path.basename(self.cb_source)
        import_dir = os.path.abspath(os.path.dirname(self.cb_source))
        should_fail = cb_base.startswith('fail_')
        should_succeed = cb_base.startswith('ok_')

        env.new_source(cb_base)
        result = self.env.try_compile(
            [config['CBC']] + \
            config['CBCFLAGS'] + ['-I', import_dir] + \
            [self.cb_source],
            not should_fail
        )

        if should_fail or should_succeed:
            self.env.print_result(cb_base, result)

        if parameters['cb-only']:
            return

        c_base = '%s.c' % cb_base.rsplit(os.path.extsep, 1)[0]
        if should_succeed and result:
            self.env.append_c_test(CTest(self.env, c_base))
        if should_succeed and not result:
            self.env.print_result(c_base, False)
        if parameters['try-fails'] and should_fail and not result:
            self.env.append_c_test(
                CTest(self.env, c_base, was_scheduled=False))
            self.env.print_opt('We’ll try the C compilation…', 'try-fails')

class CTest:
    def __init__(self, env, c_source, was_scheduled=True):
        self.env = env
        self.c_source = c_source
        self.was_scheduled = was_scheduled

        self.source = c_source

    def process(self):
        should_fail = self.c_source.startswith('fail_')
        should_succeed = self.c_source.startswith('ok_')

        result = self.env.try_compile(
            [config['CC'], '-c' ] + \
            config['CFLAGS'] + \
            [self.c_source],
            True
        )
        if should_fail or should_succeed:
            self.env.print_result(self.c_source, result, self.was_scheduled)
            if not self.was_scheduled:
                self.env.print_opt('This wasn’t part of the test suite',
                    'try-fails')

class AdvancedTest:
    def __init__(self, env, dirpath):
        self.env = env
        self.dirpath = dirpath

    def process(self):
        args = [config['MAKE'], '-C', self.dirpath]
        args += ['%s=%s' % item for item in flatten_environ(config).items()]
        result = env.try_compile(args, True)
        self.env.try_compile(
            [config['MAKE'], '-C', self.dirpath, 'clean'],
            True
        )
        self.env.print_result(os.path.basename(self.dirpath), result)

def process(env):
    iterator = os.walk(config['BASIC_TESTS'])
    for (dirpath, dirnames, filenames) in iterator:
        filenames.sort()
        cb_tests = []
        for filename in [
            f for f in filenames
            if f.endswith('cb') or f.endswith('cbi')
        ]:
            cb_tests.append(CBTest(
                env,
                os.path.abspath(os.path.join(dirpath, filename))
            ))
        env.process_module(dirpath, cb_tests)
    env.summary()

    print()
    advanced_tests = [
        os.path.abspath(os.path.join(config['ADVANCED_TESTS'], f))
        for f in os.listdir(config['ADVANCED_TESTS'])
    ]
    advanced_tests.sort()
    for dirpath in (dp for dp in advanced_tests if os.path.isdir(dp)):
        AdvancedTest(env, dirpath).process()
    env.summary()

def check_exec_env():
    if not (os.path.isfile(config['CBC'])):
        print("Compiler not present.")
        sys.exit()
    if not (os.path.isfile(os.path.join(os.getcwd(),"stdlib/std.cbi"))):
        print("std.cbi not generated.")
        sys.exit()

if __name__ == '__main__':
    check_exec_env()
    env = Env()
    try:
        process(env)
    finally:
        env.clean()
    sys.exit(env.returncode)
