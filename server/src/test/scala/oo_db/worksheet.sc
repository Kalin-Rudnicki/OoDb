
import scalaz.std.option.optionSyntax._

def test(l: Long): Option[Long] =
	l.some.flatMap(f => if (f == 0L) None else f.some)

test(-1)
test(0)
test(1)
