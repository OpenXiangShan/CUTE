# CUTE

---

[CUTE: A scalable CPU-centric and Ultra-utilized Tensor Engine for convolutions](https://www.sciencedirect.com/science/article/pii/S1383762124000432)

now is CUTEv2

修改chipyard的build.sbt，支持cute作为子模块编译～

/build.sbt

```
lazy val cute = (project in file("generators/cute"))
  .dependsOn(boom)
  .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(commonSettings)
```

```
lazy val chipyard = (project in file("generators/chipyard"))
  .dependsOn(testchipip, rocketchip, boom, hwacha, sifive_blocks, sifive_cache, iocell,
    sha3, // On separate line to allow for cleaner tutorial-setup patches
    dsptools, `rocket-dsp-utils`,cute,
    gemmini, icenet, tracegen, cva6, nvdla, sodor, ibex, fft_generator,
    constellation, mempress)
  .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(
    libraryDependencies ++= Seq(
      "org.reflections" % "reflections" % "0.10.2"
    )
  )
```

修改chipyard的config，提供cute的配置

/generators/chipyard/src/main/scala/config/BoomConfigs.scala

```
class CUTETestConfig extends Config(
  new boom.common.WithVector(1) ++
  new cute.WithCUTE(Seq(0,2,3)) ++ //在0，2，3号tile上连接cute
  new freechips.rocketchip.subsystem.WithInclusiveCache(capacityKB=512,outerLatencyCycles=40) ++
  new boom.common.WithBoomCommitLogPrintf ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new WithCustomBootROM ++                                       // Use custom BootROM to enable COSIM
  new boom.common.WithNSmallWidenBooms(4) ++                     // small boom config
  new chipyard.config.AbstractConfig)

```
