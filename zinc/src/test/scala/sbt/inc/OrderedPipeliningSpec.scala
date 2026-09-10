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

package sbt.inc

import sbt.internal.inc.StringVirtualFile
import sbt.io.IO.withTemporaryDirectory
import xsbti.compile.{ CompileOrder, CompileProgress }
import scala.jdk.CollectionConverters._

class OrderedPipeliningSpec extends BaseCompilerSpec {
  private def containsClass(output: java.nio.file.Path, name: String): Boolean = {
    if (!java.nio.file.Files.exists(output)) false
    else if (!output.toString.endsWith(".jar")) java.nio.file.Files.exists(output.resolve(name))
    else {
      val jar = new java.util.zip.ZipFile(output.toFile)
      try jar.getEntry(name) != null
      finally jar.close()
    }
  }

  for {
    version <- List("2.12.x", "2.13.x", "3.x")
    order <- List(CompileOrder.JavaThenScala, CompileOrder.ScalaThenJava)
    jarOutput <- List(false, true)
  } {
    it should s"publish complete ordered Java and Scala APIs before backend completion for $order on $version (jar=$jarOutput)" in withTemporaryDirectory {
      tmp =>
        val sv = switchScalaVersion(Some(version))
        if (sv.startsWith("3.3.")) cancel(
          "Scala 3.3 does not support pipelining; see CONTRIBUTING.md for the Scala 3.8.4 test command"
        )
        val upProject = VirtualSubproject(tmp.toPath.resolve("up"))
        val downProject = VirtualSubproject(tmp.toPath.resolve("down"))
        java.nio.file.Files.createDirectories(upProject.baseDir)
        java.nio.file.Files.createDirectories(downProject.baseDir)
        val up = upProject.setup.copy(outputToJar = jarOutput).createCompiler(sv, incOptions)
        val down = downProject.setup.copy(overrideCp = Some(List(up.earlyOutput)))
          .createCompiler(sv, incOptions)
        val javaFirst = order == CompileOrder.JavaThenScala
        val javaBody = if (javaFirst) "return 41;" else "return new Up().value();"
        val scalaBody = if (javaFirst) "new JavaApi().value()" else "41"
        val sources = Array[xsbti.VirtualFile](
          StringVirtualFile(
            "JavaApi.java",
            s"public class JavaApi { public int value() { $javaBody } }"
          ),
          StringVirtualFile(
            "Up.scala",
            s"class Up { def value: Int = $scalaBody }; object Singleton { def value: Int = 1 }"
          )
        )
        var consumedEarly = false
        var upstreamReturned = false
        val progress = new CompileProgress {
          override def afterEarlyOutput(success: Boolean): Unit = {
            assert(success)
            assert(!upstreamReturned)
            if (!jarOutput) assert(!containsClass(up.output, "Up.class"))
            down.compile(StringVirtualFile(
              "Down.scala",
              "class Down { def value: Int = new JavaApi().value() + new Up().value + Singleton.value }"
            ))
            consumedEarly = true
          }
        }
        try {
          val first = up.doCompile(in =>
            in.withOptions(in.options.withSources(sources).withOrder(order))
              .withSetup(in.setup.withProgress(progress))
          )
          upstreamReturned = true
          assert(consumedEarly)
          assert(containsClass(up.output, "Up.class"))
          if (javaFirst) assert(containsClass(up.output, "JavaApi.class"))
          else assert(!containsClass(up.output, "JavaApi.class"))
          val complete = if (javaFirst) first
          else up.doCompileAllJava(in =>
            in.withOptions(in.options.withSources(sources).withOrder(order))
              .withPreviousResult(up.zinc.previousResult(first))
          )
          assert(containsClass(up.output, "JavaApi.class"))
          complete.analysis.readStamps.getAllProductStamps.asScala.foreach {
            case (product, recorded) =>
              val actual = up.in.options.stamper().get().product(product)
              assert(
                recorded == actual,
                s"Product stamp mismatch: $product recorded=$recorded actual=$actual"
              )
          }
          val unchangedProgress = new CompileProgress {
            override def startUnit(phase: String, source: String): Unit =
              fail(s"Unexpected compilation on unchanged sources: $phase $source")
          }
          val unchanged = up.doCompile(in =>
            in.withOptions(in.options.withSources(sources).withOrder(order))
              .withPreviousResult(up.zinc.previousResult(complete))
              .withSetup(in.setup.withProgress(unchangedProgress))
          )
          assert(!unchanged.hasModified)
          down.compile(StringVirtualFile(
            "Down.scala",
            "class Down { def value: Int = new JavaApi().value() + new Up().value + Singleton.value }"
          ))
          assert(down.lastCompiledUnits.isEmpty)
        } finally {
          up.close()
          down.close()
        }
    }
  }

  for {
    order <- List(CompileOrder.JavaThenScala, CompileOrder.ScalaThenJava)
    cancelled <- List(false, true)
    existing <- List(false, true)
    jarOutput <- List(false, true)
  } {
    it should s"restore early APIs after backend failure without an action cache ($order cancelled=$cancelled existing=$existing jar=$jarOutput)" in withTemporaryDirectory {
      tmp =>
        val upProject = VirtualSubproject(tmp.toPath.resolve("up"))
        val downProject = VirtualSubproject(tmp.toPath.resolve("down"))
        java.nio.file.Files.createDirectories(upProject.baseDir)
        java.nio.file.Files.createDirectories(downProject.baseDir)
        val transactional = incOptions.withClassfileManagerType(
          xsbti.compile.TransactionalManagerType.of(
            new java.io.File(tmp, "backup"),
            sbt.util.Logger.Null
          )
        )
        val sv = switchScalaVersion(Some("2.13.x"))
        val up = upProject.setup.copy(outputToJar = jarOutput).createCompiler(sv, transactional)
        val down = downProject.setup.copy(overrideCp = Some(List(up.earlyOutput))).createCompiler(
          sv,
          incOptions
        )
        val scalaBody =
          if (order == CompileOrder.JavaThenScala) "new JavaApi().value().toInt" else "1"
        val original = Array[xsbti.VirtualFile](
          StringVirtualFile(
            "JavaApi.java",
            "public class JavaApi { public int value() { return 1; } }"
          ),
          StringVirtualFile("Up.scala", s"class Up { def value: Int = $scalaBody }")
        )
        try {
          val previous = if (existing) {
            val first =
              up.doCompile(in => in.withOptions(in.options.withSources(original).withOrder(order)))
            val complete = if (order == CompileOrder.JavaThenScala) first
            else up.doCompileAllJava(in =>
              in.withOptions(in.options.withSources(original).withOrder(order))
                .withPreviousResult(up.zinc.previousResult(first))
            )
            up.zinc.previousResult(complete)
          } else up.zinc.emptyPreviousResult
          val before =
            if (existing) Some(java.nio.file.Files.readAllBytes(up.earlyOutput)) else None
          val store = up.in.setup.earlyAnalysisStore.get()
          val beforeAnalysis = if (existing) Some(store.unsafeGet().getAnalysis) else None
          val changed = original.updated(
            0,
            StringVirtualFile(
              "JavaApi.java",
              "public class JavaApi { public long value() { return 1L; } }"
            )
          )
          val failingSources = changed.updated(
            1,
            StringVirtualFile("Up.scala", s"class Up { def value: Int = $scalaBody + 1 }")
          )
          var earlyPublished = false
          val delegate = up.in.compilers.scalac
          val failing = new xsbti.compile.ScalaCompiler {
            export delegate.{ compile => _, _ }
            def compile(
                sources: Array[xsbti.VirtualFile],
                classpath: Array[xsbti.VirtualFile],
                converter: xsbti.FileConverter,
                changes: xsbti.compile.DependencyChanges,
                options: Array[String],
                output: xsbti.compile.Output,
                callback: xsbti.AnalysisCallback,
                reporter: xsbti.Reporter,
                progress: java.util.Optional[CompileProgress],
                logger: xsbti.Logger
            ): Unit = {
              delegate.compile(
                sources,
                classpath,
                converter,
                changes,
                options,
                output,
                callback,
                reporter,
                progress,
                logger
              )
              if (sources.exists(_.id.endsWith(".scala"))) {
                assert(earlyPublished)
                if (cancelled) throw new xsbti.CompileCancelled {
                  def arguments(): Array[String] = Array.empty
                }
                else throw new IllegalStateException("injected backend failure")
              }
            }
          }
          val progress = new CompileProgress {
            override def afterEarlyOutput(success: Boolean): Unit = {
              assert(success)
              earlyPublished = true
            }
          }
          def attempt() = up.doCompile(in =>
            in.withOptions(in.options.withSources(failingSources).withOrder(order))
              .withPreviousResult(previous)
              .withCompilers(in.compilers.withScalac(failing))
              .withSetup(in.setup.withProgress(progress))
          )
          if (cancelled) assert(!attempt().hasModified)
          else intercept[IllegalStateException](attempt())
          assert(earlyPublished)
          before match {
            case Some(bytes) => assert(java.util.Arrays.equals(
                bytes,
                java.nio.file.Files.readAllBytes(up.earlyOutput)
              ))
            case None => assert(!java.nio.file.Files.exists(up.earlyOutput))
          }
          beforeAnalysis match {
            case Some(analysis) => assert(store.unsafeGet().getAnalysis == analysis)
            case None => assert(store.unsafeGet().getAnalysis.readStamps.getAllSourceStamps.isEmpty)
          }
          val recoveryProgress = new CompileProgress {
            override def startUnit(phase: String, source: String): Unit =
              if (existing && !jarOutput)
                fail(s"Unexpected compilation after rollback: $phase $source")
          }
          val recovered = up.doCompile(in =>
            in.withOptions(in.options.withSources(original).withOrder(order))
              .withPreviousResult(previous).withSetup(in.setup.withProgress(recoveryProgress))
          )
          if (existing && !jarOutput) assert(!recovered.hasModified)
          down.compile(StringVirtualFile(
            "Down.scala",
            "class Down { def value: Int = new JavaApi().value() }"
          ))
        } finally {
          up.close()
          down.close()
        }
    }
  }

  for {
    usePartial <- List(false, true)
    jarOutput <- List(false, true)
  } {
    it should s"regenerate early APIs after failed late javac without an action cache (partial=$usePartial jar=$jarOutput)" in withTemporaryDirectory {
      tmp =>
        val upProject = VirtualSubproject(tmp.toPath.resolve("up"))
        val downProject = VirtualSubproject(tmp.toPath.resolve("down"))
        java.nio.file.Files.createDirectories(upProject.baseDir)
        java.nio.file.Files.createDirectories(downProject.baseDir)
        val sv = switchScalaVersion(Some("2.13.x"))
        val transactional = incOptions.withClassfileManagerType(
          xsbti.compile.TransactionalManagerType.of(
            new java.io.File(tmp, "backup"),
            sbt.util.Logger.Null
          )
        )
        val up = upProject.setup.copy(outputToJar = jarOutput).createCompiler(sv, transactional)
        val down = downProject.setup.copy(overrideCp = Some(List(up.earlyOutput))).createCompiler(
          sv,
          incOptions
        )
        val original = Array[xsbti.VirtualFile](
          StringVirtualFile(
            "JavaApi.java",
            "public class JavaApi { public int value() { return 1; } }"
          ),
          StringVirtualFile("Up.scala", "class Up { def value: Int = 1 }")
        )
        val order = CompileOrder.ScalaThenJava
        try {
          val first =
            up.doCompile(in => in.withOptions(in.options.withSources(original).withOrder(order)))
          val complete = up.doCompileAllJava(in =>
            in.withOptions(in.options.withSources(original).withOrder(order))
              .withPreviousResult(up.zinc.previousResult(first))
          )
          val changed = original.updated(
            0,
            StringVirtualFile(
              "JavaApi.java",
              "public class JavaApi { public long value() { return missing; } }"
            )
          )
          var published = false
          val progress = new CompileProgress {
            override def afterEarlyOutput(success: Boolean): Unit = {
              assert(success); published = true
            }
          }
          val partial = up.doCompile(in =>
            in.withOptions(in.options.withSources(changed).withOrder(order))
              .withPreviousResult(up.zinc.previousResult(complete)).withSetup(in.setup.withProgress(
                progress
              ))
          )
          assert(published)
          intercept[xsbti.CompileFailed] {
            up.doCompileAllJava(in =>
              in.withOptions(in.options.withSources(changed).withOrder(order))
                .withPreviousResult(up.zinc.previousResult(partial))
            )
          }
          up.in.setup.reporter.reset()
          val recoveryPrevious = if (usePartial) partial else complete
          up.doCompile(in =>
            in.withOptions(in.options.withSources(original).withOrder(order))
              .withPreviousResult(up.zinc.previousResult(recoveryPrevious))
          )
          down.compile(StringVirtualFile(
            "Down.scala",
            "class Down { def value: Int = new JavaApi().value() }"
          ))
        } finally {
          up.close()
          down.close()
        }
    }

  }

}
