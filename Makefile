THREADS_BUILD 	?= 1

CMAKE_CXX_COMPILER :=
ifneq ($(origin CXX_COMPILER), undefined)
	CMAKE_CXX_COMPILER := -DCMAKE_CXX_COMPILER=$(CXX_COMPILER)
endif


init:
	git submodule update --init
	cd coupledL2 && make init
	cd rocket-chip && git submodule update --init hardfloat cde

compile:
	mill -i CUTE.compile

TOP = TestTop
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).sv
MEM_GEN = ./scripts/vlsi_mem_gen
MEM_GEN_SEP = ./scripts/gen_sep_mem.sh

gen-test-top:
	mill -i CUTE.test.runMain cute.$(TOP) -td $(BUILD_DIR) --target systemverilog --split-verilog
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$(TOP_V).conf" "$(BUILD_DIR)"

test-top:	
	$(MAKE) gen-test-top

clean:
	rm -rf ./build
	rm -rf ./out