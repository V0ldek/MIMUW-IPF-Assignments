# Executable name
PROG=topol

# Required objects
CMOS=topol.cmo mgienieczko.cmo pMap.cmo
CMXS=$(CMOS:.cmo=.cmx)

# Choose target: byte, native, both
all: $(PROG).byte $(PROG).native
native: $(PROG).native
byte: $(PROG).byte

OCAMLCFLAGS=	# options for ocamlc
OCAMLOPTFLAGS=	# options for ocamlopt

$(PROG).byte: $(CMOS)
	ocamlc $(OCAMLCFLAGS) -o $@ $^

${PROG}.native: ${CMXS}
	ocamlopt $(OCAMLOPTFLAGS) -o $@ $^

%.cmo: %.ml
	ocamlc $(OCAMLCFLAGS) -c $<

%.cmi: %.mli
	ocamlc $(OCAMLCFLAGS) -c $<

%.cmx: %.ml
	ocamlopt $(OCAMLOPTFLAGS) -c $<

.depend:
	ocamldep *.mli *.ml > $@

depend:
	rm -f .depend
	$(MAKE) .depend

clean:
	rm -f *.cmi *.cmo *.cmx *.byte *.native *.o .depend

include .depend
