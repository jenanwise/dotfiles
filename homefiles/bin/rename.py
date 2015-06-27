#!/usr/bin/env python
import os
import sys
import commands

USAGE = 'rename sed_pattern file [file...]'
SED = 'echo "%(name)s" | sed "%(pat)s"'

def main():
    if len(sys.argv) < 2:
        print >> sys.stderr, USAGE
        sys.exit(1)
    pat = sys.argv[1]
    files = sys.argv[2:]

    rename(pat, files, fake=False)

def rename(pat, files, verbose=False, fake=True):
    for name in files:
        status, new_name = commands.getstatusoutput(SED % locals())
        if status:
            print >> sys.stderr, 'sed barfed. Quiting.'
            sys.exit(1)

        if fake or verbose:
            print 'mv "%s" "%s"' % (name, new_name)

        if not fake:
            os.rename(name, new_name)

if __name__ == '__main__':
    main()
