/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
package xsbt

import scala.concurrent.SyncVar

/** Runs provided code in a new Thread and returns the Thread instance. */
private object Spawn
{
	def apply(f: => Unit): Thread = apply(f, false)
	def apply(f: => Unit, daemon: Boolean): Thread =
	{
		val thread = new Thread() { override def run() = { f } }
		thread.setDaemon(daemon)
		thread.start()
		thread
	}
}
private object Future
{
	def apply[T](f: => T): () => T =
	{
		val result = new SyncVar[Either[Throwable, T]]
		def run: Unit =
			try { result.set(Right(f)) }
			catch { case e: Exception => result.set(Left(e)) }
		Spawn(run)
		() =>
			result.get match
			{
				case Right(value) => value
				case Left(exception) => throw exception
			}
	}
}
