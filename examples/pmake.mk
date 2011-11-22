sources = $(wildcard *.erl)
beams = $(patsubst %.erl,ebin/%.beam,$(sources))


$(beams): $(sources)
	mcerl_compiler -sources $(sources)

clean:
	rm -f ebin/*



