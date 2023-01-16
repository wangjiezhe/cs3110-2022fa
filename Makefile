TESTEXE := \
	chap03/test_matching.exe \
	chap03/test_ex03.exe \
	chap04/test_ex04.exe \
	chap05/test_myStack.exe \
	chap06/test_sorts.exe

.PHONY: build
build:
	dune build

.PHONY: coverage-clean
coverage-clean:
	find . -name '*.coverage' | xargs rm -f
	rm -rf _coverage

.PHONY: clean
clean: coverage-clean
	rm -rf _build

.PHONY: coverage
coverage: coverage-clean build
	for t in ${TESTEXE}; do \
		dune exec --instrument-with bisect_ppx --force $$t; \
	done
	bisect-ppx-report html
	bisect-ppx-report summary