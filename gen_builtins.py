#!/bin/python3

import os
import glob
import subprocess

DST = "src/Semantics/Builtins.hs"
DIR = "builtins/"
PRE = """
module Semantics.Builtins where

import Syntax.Raw.Abs

builtins :: [Assignment]
builtins = [
"""
POST = """    ]"""

os.system("rm " + DST)
builtins = glob.glob(DIR + "*.ghopfl")

result = PRE

for b in builtins:
    ast = subprocess.run(
        ["stack", "exec", "ast-exe", "--", "-i"] + [b], 
        capture_output=True
    ).stdout.decode("utf-8").replace("\\\"", "\"")[1:-2]
    
    print (ast)
    name = os.path.basename(b).split(".")[0] 
    result += "    Assign (Ident \"" + name + "\") (TSub \"\") $\n        " + ast
    if b != builtins[-1]:
        result += ",\n"
    else:
        result += "\n"

result += POST

hs_file = open(DST, "w")

hs_file.write(result)
