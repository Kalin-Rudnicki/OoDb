package cli

import org.rogach.scallop.{ScallopConf, Subcommand}

object ScallopUtils {
	
	trait KConf {
		
		def exec(): Unit
		
		protected def onExec(): Unit
		
	}
	
	// =====| Main |=====
	
	abstract class MainConf(args: Seq[String]) extends ScallopConf(args) with KConf {
		
		override def exec(): Unit = {
			verify()
			val subs = subcommands
			if (subcommands.nonEmpty)
				subs.map(_.asInstanceOf[KConf]).foreach(_.exec())
			else
				onExec()
		}
		
	}
	
	class MainConfReqSub(args: Seq[String]) extends MainConf(args) with KConf {
		requireSubcommand()
		
		override protected def onExec(): Unit = {
			throw new RuntimeException("Something has gone very wrong...")
		}
		
	}
	
	
	// =====| Sub |=====
	
	abstract class SubConf(names: String*) extends Subcommand(names: _*) with KConf {
		
		override def exec(): Unit = {
			verify()
			val subs = subcommands
			if (subcommands.nonEmpty)
				subs.map(_.asInstanceOf[KConf]).foreach(_.exec())
			else
				onExec()
		}
		
	}
	
	class SubConfReqSub(names: String*) extends SubConf(names: _*) with KConf {
		requireSubcommand()
		
		override protected def onExec(): Unit = {
			throw new RuntimeException("Something has gone very wrong...")
		}
		
	}
	
}
