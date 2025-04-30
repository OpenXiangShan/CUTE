organization := "ict.weichuliqi.yigou"
version := "2.0"
name := "cute"
scalaVersion := "2.13.10"
Compile / packageBin / mappings ~= { mappings =>
  mappings.filter { case (_, path) =>
    !path.startsWith("cutetest/")
  }
}