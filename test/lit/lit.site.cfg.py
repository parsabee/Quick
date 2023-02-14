import sys
import os

config.quick_src_root = r'/Users/parsabagheri/Desktop/quick'
config.quick_obj_root = r'/Users/parsabagheri/Desktop/quick/cmake-build-debug'
config.quick_executable = r'qk'
config.file_check_executable = r'/Users/parsabagheri/Desktop/quick/cmake-build-debug/venv/bin/filecheck'

import lit.llvm
# lit_config is a global instance of LitConfig
lit.llvm.initialize(lit_config, config)

# Let the main config do the real work.
lit_config.load_config(config, os.path.join(config.quick_src_root, "test", "lit", "lit.cfg.py"))
