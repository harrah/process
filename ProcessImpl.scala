/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah, Vesa Vilhonen
 */
package xsbt

import java.lang.{Process => JProcess, ProcessBuilder => JProcessBuilder}
import java.io.{InputStream, IOException, OutputStream}
import java.io.{PipedInputStream, PipedOutputStream}
import java.io.{File, FileInputStream, FileOutputStream}
import java.net.URL
import OutputStreamBuilder.{fileInput, fileOutput, urlInput}

import scala.concurrent.SyncVar

private abstract class AbstractProcessBuilder extends ProcessBuilder with SinkPartialBuilder with SourcePartialBuilder
{
	def #&&(other: ProcessBuilder): ProcessBuilder = SequentialProcessBuilder.and(this, other)
	def #||(other: ProcessBuilder): ProcessBuilder = SequentialProcessBuilder.or(this, other)
	def #|(other: ProcessBuilder): ProcessBuilder =
	{
		require(other.canPipeTo, "Piping to multiple processes is not supported.")
		SequentialProcessBuilder.piped(this, other, false, false)
	}
	def ##(other: ProcessBuilder): ProcessBuilder = SequentialProcessBuilder.seq(this, other)
	
	protected def toSource = this
	protected def toSink = this
	
	def run(): Process = run(false)
	def run(connectInput: Boolean): Process = run(BasicIO.standard(connectInput))
	
	def ! = run(false).exitValue()
	def !< = run(true).exitValue()
	def !(io: ProcessIO) = run(io).exitValue()

	def canPipeTo = false
}

private[xsbt] class URLBuilder(url: URL) extends URLPartialBuilder with SourcePartialBuilder
{
	protected def toSource = urlInput(url)
}
private[xsbt] class FileBuilder(base: File) extends FilePartialBuilder with SinkPartialBuilder with SourcePartialBuilder
{
	protected def toSource = fileInput(base)
	protected def toSink = fileOutput(base, false)
	def #<<(f: File): ProcessBuilder = #<<(fileInput(f))
	def #<<(u: URL): ProcessBuilder = #<<(urlInput(u))
	def #<<(s: => InputStream): ProcessBuilder = #<<(new InputStreamBuilder(s))
	def #<<(b: ProcessBuilder): ProcessBuilder = SequentialProcessBuilder.piped(b, fileOutput(base, true), false, false)
}

private abstract class BasicBuilder extends AbstractProcessBuilder
{
	protected[this] def checkNotThis(a: ProcessBuilder) = require(a != this, "Compound process '" + a + "' cannot contain itself.")
	final def run(io: ProcessIO): Process =
	{
		val p = createProcess(io)
		p.start()
		p
	}
	protected[this] def createProcess(io: ProcessIO): BasicProcess
}
private abstract class BasicProcess extends Process
{
	def start(): Unit
}

private abstract class CompoundProcess extends BasicProcess
{
	def destroy() { destroyer() }
	def exitValue() = getExitValue().getOrElse(error("No exit code: process destroyed."))

	def start() = getExitValue
	
	protected lazy val (getExitValue, destroyer) =
	{
		val code = new SyncVar[Option[Int]]()
		code.set(None)
		val thread = Spawn(code.set(runAndExitValue()))
		
		(
			Future { thread.join(); code.get },
			() => thread.interrupt()
		)
	}
	
	/** Start and block until the exit value is available and then return it in Some.  Return None if destroyed (use 'run')*/
	protected[this] def runAndExitValue(): Option[Int]

}

private object SequentialProcessBuilder
{
	def and(first: ProcessBuilder, second: ProcessBuilder) =
		new SequentialProcessBuilder(first, second, "#&&")(
			io => new SequentialProcess(first, second, io, _ == 0) )

	def or(first: ProcessBuilder, second: ProcessBuilder) =
		new  SequentialProcessBuilder(first, second, "#||")(
			io => new SequentialProcess(first, second, io, _ != 0) )

	def seq(first: ProcessBuilder, second: ProcessBuilder) =
		new SequentialProcessBuilder(first, second, "##")(
			io => new SequentialProcess(first, second, io, ignore => true) )

	def piped(first: ProcessBuilder, second: ProcessBuilder, toError: Boolean, useFirstCode: Boolean) =
		new SequentialProcessBuilder(first, second, if(toError) "#|!" else "#|")(
			io => new PipedProcesses(first, second, io, toError, useFirstCode) )
}
private final class SequentialProcessBuilder(a: ProcessBuilder, b: ProcessBuilder, operatorString: String)
	(val create: ProcessIO => BasicProcess) extends BasicBuilder
{
	checkNotThis(a)
	checkNotThis(b)
	override def toString = " ( " + a + " " + operatorString + " " + b + " ) "
	def createProcess(io: ProcessIO) = create(io)
}

private final class SequentialProcess(a: ProcessBuilder, b: ProcessBuilder, io: ProcessIO, evaluateSecondProcess: Int => Boolean) extends CompoundProcess
{
	protected[this] override def runAndExitValue() =
	{
		val first = a.run(io)
		runInterruptible(first.exitValue)(first.destroy()) flatMap
		{ codeA =>
			if(evaluateSecondProcess(codeA))
			{
				val second = b.run(io)
				runInterruptible(second.exitValue)(second.destroy())
			}
			else
				Some(codeA)
		}
	}
}

private class PipedProcesses(a: ProcessBuilder, b: ProcessBuilder, defaultIO: ProcessIO, toError: Boolean, useFirstCode: Boolean) extends CompoundProcess
{
	protected[this] override def runAndExitValue(): Option[Int] =
	{
		val pipeOut = new PipedOutputStream
		val pipeIn = new PipedInputStream(pipeOut)
		try { runAndExitValue(pipeOut, pipeIn)   }
		finally
		{
			BasicIO.close(pipeIn)
			BasicIO.close(pipeOut)
		}
	}
	private[this] def runAndExitValue(pipeOut: PipedOutputStream, pipeIn: PipedInputStream): Option[Int] =
	{
		val currentSource = new SyncVar[Option[InputStream]]
		val source = new PipeSource(currentSource, pipeOut, a.toString)
		source.start()
		
		val currentSink = new SyncVar[Option[OutputStream]]
		val sink = new PipeSink(pipeIn, currentSink, b.toString)
		sink.start()
		
		try { runAndExitValue(currentSource, currentSink) }
		finally
		{
			source.interrupt()
			sink.interrupt()
		}
	}
	private[this] def runAndExitValue(currentSource: SyncVar[Option[InputStream]], currentSink: SyncVar[Option[OutputStream]]): Option[Int] =
	{
		def handleOutOrError(fromOutput: InputStream) = currentSource.put(Some(fromOutput))

		val firstIO =
			if(toError)
				defaultIO.withError(handleOutOrError)
			else
				defaultIO.withOutput(handleOutOrError)
		val secondIO = defaultIO.withInput(toInput => currentSink.put(Some(toInput)) )
		
		val second = b.run(secondIO)
		val first = a.run(firstIO)
		runInterruptible {
			val firstResult = first.exitValue
			currentSource.put(None)
			currentSink.put(None)
			val secondResult = second.exitValue
			if(useFirstCode) firstResult else secondResult
		} {
			first.destroy()
			second.destroy()
		}
	}
}
private class PipeSource(currentSource: SyncVar[Option[InputStream]], pipe: PipedOutputStream, label: => String) extends Thread
{
	protected def get = try { currentSource.get } catch { case _: InterruptedException => None }
	final override def run()
	{
		get match
		{
			case Some(source) =>
				try { BasicIO.transferFully(source, pipe) }
				catch { case e: IOException => println("I/O error " + e.getMessage + " for process: " + label); e.printStackTrace() }
				finally
				{
					BasicIO.close(source)
					currentSource.unset()
				}
				run()
			case None =>
				currentSource.unset()
				BasicIO.close(pipe)
		}
	}
}
private class PipeSink(pipe: PipedInputStream, currentSink: SyncVar[Option[OutputStream]], label: => String) extends Thread
{
	protected def get = try { currentSink.get } catch { case _: InterruptedException => None }
	final override def run()
	{
		get match
		{
			case Some(sink) =>
				try { BasicIO.transferFully(pipe, sink) }
				catch { case e: IOException => println("I/O error " + e.getMessage + " for process: " + label); e.printStackTrace() }
				finally
				{
					BasicIO.close(sink)
					currentSink.unset()
				}
				run()
			case None =>
				currentSink.unset()
		}
	}
}

private[xsbt] class DummyProcessBuilder(override val toString: String, exitValue : => Int) extends AbstractProcessBuilder
{
	override def run(io: ProcessIO): Process = new DummyProcess(exitValue)
	override def canPipeTo = true
}
private class DummyProcess(action: => Int) extends Process
{
	private[this] val exitCode = Future(action)
	override def exitValue() = exitCode()
	override def destroy() {}
}
/** Represents a simple command without any redirection or combination. */
private[xsbt] class SimpleProcessBuilder(p: JProcessBuilder) extends AbstractProcessBuilder
{
	override def run(io: ProcessIO): Process =
	{
		val process = p.start() // start the external process
		import io.{writeInput, processOutput, processError}
		// spawn threads that process the input, output, and error streams using the functions defined in `io`
		val inThread = Spawn(writeInput(process.getOutputStream), true)
		val outThread = Spawn(processOutput(process.getInputStream))
		val errorThread =
			if(!p.redirectErrorStream)
				Spawn(processError(process.getErrorStream)) :: Nil
			else
				Nil
		new SimpleProcess(process, inThread, outThread :: errorThread)
	}
	override def toString = p.command.toString
	override def canPipeTo = true
}
/** A thin wrapper around a java.lang.Process.  `outputThreads` are the Threads created to read from the
* output and error streams of the process.  `inputThread` is the Thread created to write to the input stream of
* the process.
* The implementation of `exitValue` interrupts `inputThread` and then waits until all I/O threads die before
* returning. */
private class SimpleProcess(p: JProcess, inputThread: Thread, outputThreads: List[Thread]) extends Process
{
	override def exitValue() =
	{
		try { p.waitFor() }// wait for the process to terminate
		finally { inputThread.interrupt() } // we interrupt the input thread to notify it that it can terminate
		outputThreads.foreach(_.join()) // this ensures that all output is complete before returning (waitFor does not ensure this)
		p.exitValue()
	}
	override def destroy() =
	{
		try { p.destroy() }
		finally { inputThread.interrupt() }
	}
}

private object OutputStreamBuilder
{
	def fileOutput(file: File, append: Boolean) = new OutputStreamBuilder(new FileOutputStream(file, append), file.getAbsolutePath)
	def urlInput(url: URL) = new InputStreamBuilder(url.openStream, url.toString)
	def fileInput(file: File) = new InputStreamBuilder(new FileInputStream(file), file.getAbsolutePath)
}
import Uncloseable.protect
private final class OutputStreamBuilder(stream: => OutputStream, label: String) extends ThreadProcessBuilder(label, _.writeInput(protect(stream)))
{
	def this(stream: => OutputStream) = this(stream, "<output stream>")
}
private final class InputStreamBuilder(stream: => InputStream, label: String) extends ThreadProcessBuilder(label, _.processOutput(protect(stream)))
{
	def this(stream: => InputStream) = this(stream, "<input stream>")
}

private abstract class ThreadProcessBuilder(override val toString: String, runImpl: ProcessIO => Unit) extends AbstractProcessBuilder
{
	override def run(io: ProcessIO): Process =
	{
		val success = new SyncVar[Boolean]
		success.put(false)
		new ThreadProcess(Spawn {runImpl(io); success.set(true) }, success)
	}
}
private final class ThreadProcess(thread: Thread, success: SyncVar[Boolean]) extends Process
{
	override def exitValue() =
	{
		thread.join()
		if(success.get) 0 else 1
	}
	override def destroy() { thread.interrupt() }
}