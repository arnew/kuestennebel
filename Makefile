test: evaluation

ignore:=$(shell make -f Makefile_Bootstrap Makefile_eval)

-include Makefile_local

-include Makefile_eval

include Makefile_rules
