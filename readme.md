# CUTE

---

[CUTE: A scalable CPU-centric and Ultra-utilized Tensor Engine for convolutions](https://www.sciencedirect.com/science/article/pii/S1383762124000432)

now is CUTEv2

## Standalone MXFP build

Run `make init` to materialize the pinned FPE, Utility, and cache dependencies.
Set `NOOP_HOME` to the absolute path of the matching XSAI integration checkout,
with its pinned `difftest` and `fudian` submodules initialized. Run
`NOOP_HOME=/path/to/XSAI make compile` (or `mill -i CUTE.test.compile` with
the same environment). This uses that checkout's dependencies rather than a
local or sibling Difftest checkout.
