sources = $(wildcard *.erl)
beams = $(patsubst %.erl,ebin/%.beam,$(sources))

$(beams): $(sources)
	mcerl_compiler -sources $(sources)
#	erl -noshell -s mce_erl_compile start -sources $(sources)

clean:
	rm -f ebin/*

ebin_dir:
	echo Directory ebin

