/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
package xsbt

import java.io.{ByteArrayOutputStream, FileInputStream, File, FileOutputStream, InputStream, OutputStream}
import java.net.{URISyntaxException, URL}

object IO
{
	def getSource[T](implicit mf: scala.reflect.Manifest[T]): String =
		(toFile(mf.erasure.getProtectionDomain.getCodeSource.getLocation)).getAbsolutePath

	def writeBytes(file: File, bytes: Array[Byte]) = output(file)(_.write(bytes))
	def readBytes(file: File): Array[Byte] = input(file)(readBytes)
	def readBytes(in: InputStream): Array[Byte] =
	{
		val out = new ByteArrayOutputStream
		transfer(in, out)
		out.toByteArray
	}
	def output[T](file: File)(f: OutputStream => T): T = io(new FileOutputStream(file))(f)
	def input[T](file: File)(f: InputStream => T): T = io(new FileInputStream(file))(f)
	def transfer(in: InputStream, out: OutputStream)
	{
		val buffer = new Array[Byte](8192)
		def transfer0(): Unit =
		{
			val byteCount = in.read(buffer)
			if(byteCount >= 0)
			{
				out.write(buffer, 0, byteCount)
				transfer0()
			}
		}
		transfer0()
	}
	def io[R <: java.io.Closeable,T](res: => R)(f: R => T): T =
	{
		val r = res
		try { f(r) } finally { try { r.close() } catch { case e: Exception => () } }
	}
	def >>(file: File, s: String)
	{
		import java.io._
		io(new PrintWriter(new OutputStreamWriter(new FileOutputStream(file, true)))) { _.println(s) }
	}
	
	def toFile(url: URL) =
		try { new File(url.toURI) }
		catch { case _: URISyntaxException => new File(url.getPath) }
}