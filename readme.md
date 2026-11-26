# CUTE

---

[CUTE: A scalable CPU-centric and Ultra-utilized Tensor Engine for convolutions](https://www.sciencedirect.com/science/article/pii/S1383762124000432)

now is CUTEv2

## Standalone MXFP build

Run `make init` to materialize the pinned FPE, Utility, and cache dependencies.
Use the Difftest and fudian revisions pinned by the matching XSAI integration
checkout. Place Difftest at `../difftest` or `./difftest`, and set `NOOP_HOME`
to that XSAI checkout to use its `fudian` sources (or place fudian at
`../fudian`). Then run `make compile` or `mill -i CUTE.test.compile`.
The parent XSAI build provides these modules directly from its own dependencies.
