grammar:
	 antlr4 kod.g4 && javac *.java

run-tests:
	# Due to a bug in grun, the following does not parse all of the passed files:
	# grun kod compilationUnit tests/*.kod

	bash -c 'for file in tests/*.kod; do echo $$file; grun kod compilationUnit $$file; done;'
