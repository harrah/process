package xsbt

import java.io.File
import org.specs._
import Process._

object PipeExitCodeSpec extends Specification
{
	def devnull = new File("/dev/null")
	"redirect" should {
		"preserve exit code from left process" in {
			failure("false" #>> devnull)
			success("true" #>> devnull)
			failure("false" #> devnull)
			success("true" #> devnull)
			success(devnull #<< "false")
			success(devnull #<< "true")
			success(devnull #< "false")
			success(devnull #< "true")
			failure("false" #< "false")
			failure("false" #< "true")
			failure("false" #< "false")
			failure("false" #< "true")
		}
	}
	"pipe" should {
		"preserve exit code from right process" in {
			success("false" #| "true")
			failure("false" #| "false")
			failure("true" #| "false")
			success("true" #| "true")
		}
	}
	def success(p: ProcessBuilder) = (p !) must be( 0 )
	def failure(p: ProcessBuilder) = (p !) must not(be(0))
}