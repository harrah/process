/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
package xsbt

import java.io.File
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Prop._

import Process._

object ExitSpecification extends Properties("Process Exit Codes")
{
	implicit val arbResult: Arbitrary[Array[Boolean]] = Arbitrary(Gen.choose(0, 10) flatMap { size => Gen.resize(size, Arbitrary.arbArray[Boolean].arbitrary) })

	//specify("Correct exit code", (exitCode: Byte) => checkExit(exitCode))
	specify("#&& correct", (exitCodes: Array[Boolean]) => checkBinary(exitCodes)(_ #&& _)(_ && _))
	specify("#|| correct", (exitCodes: Array[Boolean]) => checkBinary(exitCodes)(_ #|| _)(_ || _))
	specify("## correct", (exitCodes: Array[Boolean]) => checkBinary(exitCodes)(_ ## _)( (x,latest) => latest))

	private def checkBinary(results: Array[Boolean])(reduceProcesses: (ProcessBuilder, ProcessBuilder) => ProcessBuilder)(reduceExit: (Boolean, Boolean) => Boolean) =
	{
		(results.length > 1) ==>
		{
			val exitCode = results.map(result => Process(exitb(result))).reduceLeft(reduceProcesses) !
			val expectedResult = results.reduceLeft(reduceExit)
			toBoolean(exitCode) == expectedResult
		}
	}
	private def toBoolean(exitCode: Int) = exitCode == 0
	private def checkExit(code: Byte) =
	{
		val exitCode = unsigned(code)
		(process("xsbt.exit " + exitCode) !) == exitCode
	}
	private def exitb(success: Boolean) = if(success) "true" else "false"
	private def unsigned(b: Byte): Int = ((b: Int) +256) % 256
	private def process(command: String) =
	{
		val ignore = echo // just for the compile dependency so that this test is rerun when TestedProcess.scala changes, not used otherwise

		val thisClasspath = List(IO.getSource[ScalaObject], IO.getSource[xsbt.IO.type], IO.getSource[xsbt.SourceTag]).mkString(File.pathSeparator)
		"java -cp " + thisClasspath + " " + command
	}
}
private trait SourceTag
