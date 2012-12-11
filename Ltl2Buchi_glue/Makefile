EMAKE = erl -make

all: tool

tool:   src/mce_ltl_parser.erl
	cd ebin; $(EMAKE)

src/mce_ltl_parser.erl: src/mce_ltl_parser.yrl
	cd src; erl -noshell -run yecc file mce_ltl_parser -run erlang halt

clean:
	cd ebin; rm -f *.beam ../src/mce_ltl_parser.erl
