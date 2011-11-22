sources = $(wildcard *.erl) 
beams = $(patsubst %.erl,ebin/%.beam,$(sources))

$(beams): $(sources)
	../../../scripts/mcerl_compiler -output_dir ../ebin -sources $(sources)

clean:
	rm -f ../ebin/*.beam ../ebin/*.core
