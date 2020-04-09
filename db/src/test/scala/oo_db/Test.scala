package oo_db

import cli.ScallopUtils._

object Test {
	
	class Conf(arguments: Seq[String]) extends MainConfReqSub(arguments) {
		val speeds = new SubConf("speeds") {
			val mode = choice(Seq("INSERT"), required = true)
			
			override protected def onExec(): Unit = {
				(mode.toOption: @unchecked) match {
					case Some("INSERT") =>
						println("Yay")
				}
			}
		}
		val test = new SubConf("test") {
			val testSize = opt[Int](required = true, short = 's')
			val debug = opt[Boolean](default = Some(false))
			
			validate(testSize)(s => {
				if (s > 0)
					Right(())
				else
					Left("test-size must be > 0")
			})
			
			override protected def onExec(): Unit = ???
		}
		val misc = new SubConf("misc") {
			override protected def onExec(): Unit =
				Misc.test
		}
		addSubcommand(speeds)
		addSubcommand(test)
		addSubcommand(misc)
		requireSubcommand()
	}
	
	def main(args: Array[String]): Unit = {
		val conf = new Conf(args)
		conf.exec
	}
	
}
