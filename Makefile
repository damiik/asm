.PHONY: all #don't refer to real files
all: ini.bytes ini

TARGET = asm

#executable file
ini: ./parser.ml ./${TARGET}.ml
	ocamlfind ocamlopt -I ./ -o ${TARGET} ./parser.ml ./${TARGET}.ml

#ocamlrun script executable (binary data)
ini.bytes: ./parser.ml ./${TARGET}.ml
	ocamlfind ocamlc -I ./ -o ./${TARGET}.bytes str.cma ./parser.ml ./${TARGET}.ml 



clean:
	-rm -f *.cmi
	-rm -f *.cmx
	-rm -f *.cmo
	-rm -f *.o
	-rm -f *.bytes
	-rm -f $(TARGET)

