import os

dirname = os.path.dirname
TARGET = dirname(dirname(__file__))

if os.path.exists(os.path.join(TARGET, "py2beam")):
    SRC_DIR = "py2beam"
    CG_DIR = "code_generator"
    FE_DIR = "frontend"
else:
    SRC_DIR = "src"
    CG_DIR = "code_generator"
    FE_DIR = "frontend"

