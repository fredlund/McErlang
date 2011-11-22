EMAKE = erl -make

default: tool

all: tool tests doc

src/ltl_parser.erl: src/ltl_parser.yrl
	cd src; erl -noshell -run yecc file ltl_parser -run erlang halt

tool: 	src/ltl_parser.erl
	cd ebin; $(EMAKE)

clean:
	cd ebin; rm -f *.beam ../src/ltl_parser.erl

tests:
	cd test; $(EMAKE)

tests_clean:
	cd ebin; rm -f ltl2buchi_eqc.beam ltl2buchi_wrap.beam wring_wrap.beam

doc: tool 
	echo "Building documentation in ./edoc"
	erl -noshell -eval "edoc:application('ltl2buchi',\".\",[])" -s init stop 
