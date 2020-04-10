package oo_db

object Misc {
	
	case class T(x: String)
	
	def test: Unit = {
		
		println(
			T("Hello") == T("Hello")
		)
		
	}
	
}
