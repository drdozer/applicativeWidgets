package applicativeWidgets

import scalaz.{ FreeAp, ~> , ValidationNel }
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.std.option._

import shapeless._

/**
  *
  *
  * @author Matthew Pocock
  */
object TestUi {

  def main(args: Array[String]): Unit = {
    val dsl = new WidgetDsl {}

    val blankText = dsl.text(HNil)
    println(s"Made text $blankText")

    val filledText = dsl.text(Text.Set("Hi Mum") :: HNil)
    println(s"Made text $filledText")

    val checkbox = dsl.checkbox(Checkbox.Set(true) :: HNil)
    println(s"Made checkbox $checkbox")

    val ui = (blankText |@| filledText |@| checkbox).tupled //(_, _, _)
    println(s"Made UI $ui")

    object PrettyPrint extends (WidgetImpl ~> Const[String]#λ) {
      def apply[A](wi: WidgetImpl[A]) = wi match {
        case wi@WidgetImpl(text : Text.Widget) => s"$text at ${wi.label}"
      }
    }
  }

}
