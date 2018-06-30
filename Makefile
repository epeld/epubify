
epubify : *.pl
	swipl --quiet -f compile.pl -g "compile:compile_app".
