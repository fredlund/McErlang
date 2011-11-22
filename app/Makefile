EMAKE = erl -pa ../../behaviours/ebin -make

all: app doc

app: 
	cd ./ebin; $(EMAKE)

doc: app
	echo "Building documentation in ./doc"
	erl -noshell -eval "edoc:application('mce_app',\".\",[{dir,\"./doc/html\"},{exclude_packages,[]}])" -s init stop 
#	@cd doc; iconv -f LATIN1 -t UTF8 overview-summary.html > tmp; mv tmp overview-summary.html
#	@cd doc; sed -e 's/Ã¼/ü/g' overview-summary.html > tmp; mv tmp overview-summary.html

