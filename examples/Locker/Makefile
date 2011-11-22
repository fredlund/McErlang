sources = $(wildcard *.erl) 
beams = $(patsubst %.erl,ebin/%.beam,$(sources))

$(beams): $(sources)
	../../scripts/mcerl_compiler -sources $(sources)

clean:
	rm -f ebin/*

ebin_dir:
	echo Directory ebin




