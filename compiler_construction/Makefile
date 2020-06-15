.PHONY: all clean
all: 
	stack init
	stack build
	stack install --local-bin-path .
clean:
ifneq ("$(wildcard .stack-work)","")
	stack clean
	rm -rf src/.stack-work
	rm -rf .stack-work
    rm -rf dist/
	rm src/javalette.cabal
	rm stack.yaml
	rm stack.yaml.lock
	rm jlc
endif
