/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah, Vesa Vilhonen
 */
package xsbt

import java.io.{BufferedReader, Closeable, FilterInputStream, FilterOutputStream, InputStream, InputStreamReader, IOException, OutputStream}

private object BasicIO
{
	def ignoreOut = (i: OutputStream) => ()
	val BufferSize = 8192
	def close(c: java.io.Closeable) = try { c.close() } catch { case _: java.io.IOException => () }
	def processFully(processLine: String => Unit)(i: InputStream)
	{
		val reader = new BufferedReader(new InputStreamReader(i))
		processLinesFully(processLine)(reader.readLine)
	}
	def processLinesFully(processLine: String => Unit)(readLine: () => String)
	{
		def readFully()
		{
			val line = readLine()
			if(line != null)
			{
				processLine(line)
				readFully()
			}
		}
		readFully()
	}
	def connectToIn(o: OutputStream) { transferFully(System.in, o) }
	def input(connect: Boolean): OutputStream => Unit = if(connect) connectToIn else ignoreOut
	def standard(connectInput: Boolean): ProcessIO = standard(input(connectInput))
	def standard(in: OutputStream => Unit): ProcessIO = new ProcessIO(in, transferFully(_, System.out), transferFully(_, System.err))

	def transferFully(in: InputStream, out: OutputStream): Unit =
		try { transferFullyImpl(in, out) }
		catch { case  _: InterruptedException => () }
	
	private[this] def transferFullyImpl(in: InputStream, out: OutputStream)
	{
		val continueCount = 1//if(in.isInstanceOf[PipedInputStream]) 1 else 0
		val buffer = new Array[Byte](BufferSize)
		def read
		{
			val byteCount = in.read(buffer)
			if(byteCount >= continueCount)
			{
				out.write(buffer, 0, byteCount)
				out.flush()
				read
			}
		}
		read
	}
}

object Uncloseable
{
	def apply(in: InputStream): InputStream = new FilterInputStream(in) { override def close() {} }
	def apply(out: OutputStream): OutputStream = new FilterOutputStream(out) { override def close() {} }
	def protect(in: InputStream): InputStream = if(in eq System.in) Uncloseable(in) else in
	def protect(out: OutputStream): OutputStream = if( (out eq System.out) || (out eq System.err)) Uncloseable(out) else out
}

protected[xsbt] object runInterruptible
{
	/** Evaluates 'action' and returns it wrapped in Some.  If this thread is interrupted before 'action' completes,
	* 'destroyImpl' is called and None is returned.*/
	def apply[T](action: => T)(destroyImpl: => Unit): Option[T] =
	{
		try { Some(action) }
		catch { case _: InterruptedException => destroyImpl; None }
	}
}