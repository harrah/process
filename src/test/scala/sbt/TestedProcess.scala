/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
package xsbt

import java.io.File

object exit
{
	def main(args: Array[String])
	{
		System.exit(java.lang.Integer.parseInt(args(0)))
	}
}
object cat
{
	def main(args: Array[String])
	{
		if(args.length == 0)
			IO.transfer(System.in, System.out)
		else
			args.foreach(catFile)
	}
	private def catFile(filename: String)
	{
		val file = new File(filename)
		if(file.isDirectory)
			error("Is directory: " + file)
		else if(file.exists)
			IO.input(file) ( in => IO.transfer(in, System.out) )
		else
			error("No such file or directory: " + file)
	}
}
object echo
{
	def main(args: Array[String])
	{
		System.out.println(args.mkString(" "))
	}
}