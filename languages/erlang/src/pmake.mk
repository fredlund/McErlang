sources = $(wildcard *.erl)
beams = $(patsubst %.erl,../ebin/%.beam,$(sources))

all: $(beams) ../ebin/mce_erl_systemCode.beam ../ebin/mce_erl_shell.beam

../ebin/mce_erl_systemCode.beam: pre/mce_erl_systemCode.erl
	(cd pre; ../../../../scripts/mcerl -noshell -s mce_erl_compile start -sources mce_erl_systemCode.erl)
	mv pre/ebin/mce_erl_systemCode.beam ../ebin

../ebin/mce_erl_shell.beam: pre/mce_erl_shell.erl
	(cd pre; ../../../../scripts/mcerl -noshell -s mce_erl_compile start -sources mce_erl_shell.erl)
	mv pre/ebin/mce_erl_shell.beam ../ebin

../ebin/%.beam : %.erl
	cd ../ebin; erl -make

clean:
	rm -f $(beams) 
	rm -f ../ebin/mce_erl_systemCode.beam 
	rm -f ../ebin/mce_erl_shell.beam 
	rm -rf pre/ebin/*




