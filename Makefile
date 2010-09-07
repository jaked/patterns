OCAMLMAKEFILE = ./OCamlMakefile

SOURCES := traverse.mli traverse.ml patterns.ml
USE_CAMLP4 := yes

all: traverse.cmi traverse.cmo patterns.cmo

include $(OCAMLMAKEFILE)
