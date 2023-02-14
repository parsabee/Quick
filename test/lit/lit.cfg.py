import lit.formats

config.name = 'Quick-Compiler'
config.suffixes = ['.qk']
config.test_source_root = os.path.dirname(__file__)
config.test_format = lit.formats.ShTest(True)
config.test_exec_root = os.path.join(config.quick_obj_root, 'test', 'lit')
config.substitutions.append(('%quick', config.quick_executable))
config.substitutions.append(('%file-check', config.file_check_executable))