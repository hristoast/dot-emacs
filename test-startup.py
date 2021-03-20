#!/usr/bin/env python3
#
# THANKS:
# https://github.com/mrkkrp/dot-emacs/blob/ca8198edd845796ff5345df42e7a3367b4f798c5/test-startup.py
#
import subprocess


startup_lisp = """
(let ((debug-on-error t)
      (url-show-status nil)
      (user-emacs-directory default-directory)
      (user-init-file (expand-file-name "init.el"))
      (load-path (delq default-directory load-path)))
  (load-file user-init-file)
  (run-hooks (quote after-init-hook)))"""


def start_emacs(title, eval_str):
    """
    Try to start Emacs in batch mode and without window system. Fail loudly.
    """
    print("Starting", title)
    subprocess.check_call(["emacs", "-nw", "--batch", "--eval", eval_str])
    print("Done:", title, end="\n\n")


try:
    subprocess.check_call(["emacs", "--version"])
    print()
    start_emacs("initial startup", startup_lisp)
except subprocess.CalledProcessError as e:
    exit(e.returncode)
else:
    print("Startup testing finished successfully")
