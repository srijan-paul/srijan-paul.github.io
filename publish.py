#!/urs/bin/python3
from os import system
from sys import argv

system("bark build")
system("rm -rf docs && cp -r build docs")
system("git add .")
system(f'git commit -m "{argv[1]}"')