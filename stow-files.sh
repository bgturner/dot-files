#!/bin/bash
for dir in emacs vim shell;
do stow $dir
done;
