init:
	git submodule update --init
	cd coupledL2 && make init
	cd rocket-chip && git submodule update --init hardfloat cde

compile:
	mill -i CUTE.compile

clean:
	rm -rf ./build
	rm -rf ./out