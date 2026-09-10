/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend, and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * SPDX-License-Identifier: Apache-2.0
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt.internal.inc

import java.nio.file.Files
import scala.jdk.CollectionConverters._
import sbt.io.IO.withTemporaryDirectory
import sbt.util.Logger

class PickleJarSpec extends UnitSpec {
  it should "remove deleted and renamed Java classes and Scala APIs while retaining live products" in withTemporaryDirectory {
    dir =>
      val path = dir.toPath.resolve("early.jar")
      val entries = Set(
        "Live.class",
        "Live$Nested.class",
        "Gone.class",
        "Gone.sig",
        "Gone.tasty",
        "Renamed.class",
        "Singleton.class",
        "Singleton.tasty",
        "META-INF/MANIFEST.MF"
      )
      val root = RootPath(path, writable = true)
      try entries.foreach { entry =>
          val file = root.root.resolve(entry)
          Files.createDirectories(file.getParent)
          Files.write(file, Array[Byte](1))
        }
      finally root.close()
      PickleJar.write(
        path,
        Set("Live.class", "Live$Nested.class", "Singleton$.class").asJava,
        Logger.Null
      )
      val after = RootPath(path, writable = false)
      try {
        entries.filter(e =>
          Set("Live.class", "Live$Nested.class", "Singleton.tasty", "META-INF/MANIFEST.MF")(e)
        )
          .foreach(e => assert(Files.exists(after.root.resolve(e))))
        Set("Gone.class", "Gone.sig", "Gone.tasty", "Renamed.class", "Singleton.class")
          .foreach(e => assert(!Files.exists(after.root.resolve(e))))
      } finally after.close()
      PickleJar.write(path, Set.empty[String].asJava, Logger.Null)
      val empty = RootPath(path, writable = false)
      try {
        entries.filterNot(_ == "META-INF/MANIFEST.MF").foreach(e =>
          assert(!Files.exists(empty.root.resolve(e)))
        )
        assert(Files.exists(empty.root.resolve("META-INF/MANIFEST.MF")))
      } finally empty.close()
  }
}
