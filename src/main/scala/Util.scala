package mpstk

import scala.io.Source

import java.io.{BufferedReader, InputStreamReader}
import java.lang.Process

import com.typesafe.scalalogging.LazyLogging

package object util extends LazyLogging {
  /** Convert a list of options into an optional list */
  def optList[A](xs: List[Option[A]]): Option[List[A]] = xs match {
    case hd :: tl => for { x <- hd; l <- optList(tl) } yield x :: l
    case Nil => Some(Nil)
  }

  // FIXME: maybe we can refactor the following
  /** Convert a list of eithers into either a list, or a left projection */
  def eitherList[A,L](xs: List[Either[L,A]]): Either[L, List[A]] = xs match {
    case hd :: tl => for { x <- hd; l <- eitherList(tl) } yield x :: l
    case Nil => Right(Nil)
  }

  /* Return the contents of a resource as a String */
  def resource(name: String): String = {
    val resource = Source.fromResource(name)
    assert(resource != null)
    resource.getLines.mkString("\n")
  }

  /** Used to store the standard output of a process launched via
    * external command, and its execution time */
  protected[mpstk]
  case class StdOutErrAndTime(stdout: String, stderr: String, nanosecs: Long)

  import scala.collection.mutable.MutableList
  /** Run a system command, and return its process handle, and execution
    * time in nsecs.
    * 
    *  @throws java.lang.RuntimeException if command's return status is not 0
    */
  def runCommand(cmd: String, args: Seq[String]): StdOutErrAndTime = {
    import com.zaxxer.nuprocess.NuProcessBuilder

    val startTime = MutableList[Long]() // Will contain starting time (one elem)
    val endTime = MutableList[Long]() // Will contain starting time (one elem)
    val stdout = MutableList[Array[Byte]]()
    val stderr = MutableList[Array[Byte]]()

    logger.debug(s"""Executing: ${cmd} ${args.mkString(" ")}""")
    val builder = new NuProcessBuilder((cmd +: args):_*)
    val handler = new ProcessHandler(startTime, endTime, stdout, stderr)
    builder.setProcessListener(handler)
    val p = builder.start()
    val r = p.waitFor(0, java.util.concurrent.TimeUnit.SECONDS) // Wait forever

    val stdoutStr = stdout.foldLeft("") { (acc, b) => acc + new String(b) }
    val stderrStr = stderr.foldLeft("") { (acc, b) => acc + new String(b) }
    
    if (r != 0) {
      throw new RuntimeException(
        s"""Command failed with code ${r}: ${cmd} ${args.mkString(" ")}"""
          + s"\nStandard error:\n" + stderrStr)
    }

    StdOutErrAndTime(stdoutStr, stderrStr, endTime(0) - startTime(0))
  }

  class ProcessHandler(startTime: MutableList[Long],
                       endTime: MutableList[Long],
                       stdout: MutableList[Array[Byte]],
                       stderr: MutableList[Array[Byte]])
      extends com.zaxxer.nuprocess.NuProcessHandler {
    import com.zaxxer.nuprocess.NuProcess
    import java.nio.ByteBuffer

    private var nuProcess: NuProcess = null

    override def onPreStart(nuProcess: NuProcess): Unit = {
      startTime += System.nanoTime()
    }

    override def onStart(nuProcess: NuProcess): Unit = {
      this.nuProcess = nuProcess;
    }
   
    override def onStdout(buffer: ByteBuffer, closed: Boolean): Unit = {
      if (!closed) {
         val bytes = new Array[Byte](buffer.remaining)
         // You must update buffer.position() before returning (either
         // implicitly, like this, or explicitly) to indicate how many
         // bytes your handler has consumed.
         buffer.get(bytes)
         stdout += bytes
      }
    }

    override def onStderr(buffer: ByteBuffer, closed: Boolean): Unit = {
      if (!closed) {
         val bytes = new Array[Byte](buffer.remaining);
         // You must update buffer.position() before returning (either
         // implicitly, like this, or explicitly) to indicate how many
         // bytes your handler has consumed.
         buffer.get(bytes)
         stderr += bytes
      }
    }

    override def onStdinReady(buf: ByteBuffer): Boolean = false

    override def onExit(ret: Int): Unit = {
      endTime += System.nanoTime()
    }
  }

  /** Slow (but portable) version of runCommand(...) */
  def runCommandSlow(cmd: String, args: Seq[String]): StdOutErrAndTime = {
    logger.debug(s"""Executing: ${cmd} ${args.mkString(" ")}""")
    val builder = new ProcessBuilder((cmd +: args):_*)
    val startTime: Long = System.nanoTime()
    val p = builder.start()

    val outReader = new BufferedReader(new InputStreamReader(p.getInputStream))
    val outStr = {
      Iterator.continually(outReader.readLine()).takeWhile(_ != null).mkString
    }

    val errReader = new BufferedReader(new InputStreamReader(p.getErrorStream))
    val errStr = {
      Iterator.continually(errReader.readLine()).takeWhile(_ != null).mkString
    }

    val r = p.waitFor()
    val endTime: Long = System.nanoTime()

    if (r != 0) {
      throw new RuntimeException(
        s"""Command failed with code ${r}: ${cmd} ${args.mkString(" ")}"""
          + s"\nStandard error:\n" + errStr)
    }
    StdOutErrAndTime(outStr, errStr, endTime - startTime)
  }
}
