#!/usr/bin/env python3
"""
Install config files from a source-controlled directory.
"""

import os
import shutil
import subprocess
import sys

FILE_MAP = {
    'agignore': '~/.agignore',
    'bashrc': '~/.bashrc',
    'bin': '~/bin',
    'doom.d': '~/.doom.d',
    'gitconfig': '~/.gitconfig',
    'gitignore': '~/.gitignore',
    'inputrc': '~/.inputrc',
    'psqlrc': '~/.psqlrc',
    'snippets': '~/.snippets',
    'tmux.conf.local': '~/.tmux.conf.local',
    'vimrc': '~/.vimrc',
    'vim': '~/.vim',
}


def main():
    if '--help' in sys.argv or '-h' in sys.argv:
        print("Usage: %(cmd)s [--help] [--update]" % {'cmd': sys.argv[0]})

    else:
        update = '--update' in sys.argv or '-u' in sys.argv

        print('Installing config files:\n------------------------')

        basedir = os.path.join(os.path.dirname(sys.argv[0]), 'files')
        for filename, dest in FILE_MAP.items():
            src = os.path.expanduser(
                os.path.expandvars(os.path.join('$PWD', basedir, filename)))
            assert os.path.exists(src), f'"{src}" does not exist'
            dest = os.path.expanduser(dest)
            symlink_with_confirm(src, dest, update)


def symlink_with_confirm(src, dest, update):
    """
    Symlink src to dest. Asks for confirm on overwrite.

    If update, don't ask to replace existing files -- just skip them.
    """
    # Prompt for overwrite if the dest exists.
    if os.path.islink(dest) or os.path.exists(dest):
        if os.path.realpath(dest) == os.path.realpath(src):
            return  # alrady installed

        elif update:
            print('%s exists. Skipping.' % dest)
            return  # don't symlink
        else:
            answer = input('%s exists. Replace [n]? ' % dest).lower()
            if answer in ['y', 'yes']:
                if os.path.isdir(dest):
                    shutil.rmtree(dest)
                else:
                    os.remove(dest)
            else:
                return  # don't symlink

    # Create the dest directory if it does't exist
    elif not os.path.exists(os.path.dirname(dest)):
        os.makedirs(os.path.dirname(dest))

    # everything's okay -- make the symlink
    print("Linking %s to %s" % (src, dest))
    os.symlink(src, dest)


if __name__ == '__main__':
    main()
