package xsbt

import java.io.File
import org.specs._
import Process._

object CompoundPipeSpec extends Specification
{
	"output from multiple source processes" should {
		"be piped to sink process" in {
			success("echo first" #&& "echo second" #| "grep first" )
			success("echo first" #&& "echo second" #| "grep second")
			success("echo first" #|| "echo second" #| "grep first")
			failure("echo first" #|| "echo second" #| "grep second")
			
			success("echo first" #&& "false" #&& "echo second" #| "grep first" )
			failure("echo first" #&& "false" #&& "echo second" #| "grep second" )
			success("echo first" #&& "true" #&& "echo second" #| "grep first" )
			success("echo first" #&& "true" #&& "echo second" #| "grep second" )
			
			success("echo first" #&& "false" #|| "echo second" #| "grep first" )
			success("echo first" #&& "false" #|| "echo second" #| "grep second" )
			success("echo first" #&& "true" #|| "echo second" #| "grep first" )
			failure("echo first" #&& "true" #|| "echo second" #| "grep second" )
		}
	}
	def success(p: ProcessBuilder) = runQuiet(p) must be( 0 )
	def failure(p: ProcessBuilder) = runQuiet(p) must not(be(0))
	def runQuiet(p: ProcessBuilder): Int = (p #>> (new File("/dev/null")) !)
}