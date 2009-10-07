/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
package xsbt

import java.io.File
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Prop._

import Process._

object BasicPipeSpec extends Properties("Process Pipe")
{
	specify("to output file", checkFileOut _)
	specify("to input file", checkFileIn _)
	specify("to process", checkPipe _)
	
	private def cat(args: String) = "cat " + args
	private def checkFileOut(data: Array[Byte]) =
	{
		withData(data) { (temporaryFile, temporaryFile2) =>
			val catCommand = cat(temporaryFile.getAbsolutePath)
			catCommand #> temporaryFile2
		}
	}
	private def checkFileIn(data: Array[Byte]) =
	{
		withData(data) { (temporaryFile, temporaryFile2) =>
			val catCommand = cat("")
			temporaryFile #> catCommand #> temporaryFile2
		}
	}
	private def checkPipe(data: Array[Byte]) =
	{
		withData(data) { (temporaryFile, temporaryFile2) =>
			val catCommand = cat("")
			temporaryFile #> catCommand #| catCommand #> temporaryFile2
		}
	}
	private def temp() = File.createTempFile("xsbt", "")
	private def withData(data: Array[Byte])(f: (File, File) => ProcessBuilder) =
	{
		val temporaryFile1 = temp()
		val temporaryFile2 = temp()
		try
		{
			IO.writeBytes(temporaryFile1, data)
			val process = f(temporaryFile1, temporaryFile2)
			( process ! ) == 0 &&
				( IO.readBytes(temporaryFile1) deepEquals IO.readBytes(temporaryFile2) )
		}
		finally
		{
			temporaryFile1.delete()
			temporaryFile2.delete()
		}
	}
}
