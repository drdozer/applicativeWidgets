package applicativeWidgets

import scalaz._
import shapeless._
import shapeless.ops._
import shapeless.{:+:, Coproduct}

import scala.annotation.implicitNotFound

trait WidgetCommand

object WidgetCommand {
  case class Set[T](newValue: T) extends WidgetCommand
  case class Modify[T](updateWith: T => T) extends WidgetCommand

  case object Enable extends WidgetCommand
  case object Dissable extends WidgetCommand
}

trait WidgetEvent

object WidgetEvent {
  trait UpdatedTo[T] extends WidgetEvent {
    def newValue: T
  }
}

@implicitNotFound("Could not unwrap a ${A} from ${T}")
trait Unwrapper[A, T] {
  def apply(t: T): A
}

object Unwraper {
  implicit def at[A, T](implicit ta: T <:< A): Unwrapper[A, T] = new Unwrapper[A, T] {
    override def apply(t: T) = ta(t)
  }

  implicit def inTag[A, T, Tag](implicit atT: Unwrapper[A, T]): Unwrapper[A, T @@ Tag] = new Unwrapper[A, @@[T, Tag]] {
    override def apply(t: @@[T, Tag]) = atT(Tag.unwrap(t))
  }
}


/** The widget API.
 *
 * At any evaluation cycle, a widget respons to at most one of a selection of commands,
 * and emmits zero or more of a list of events.
 *
 * @tparam CS    a Coproduct of WidgetCommand types that this widget can handle
 * @tparam ES    a HList of WidgetEvent types that this widget may emmit
 */
abstract class WidgetApi[CS <: Coproduct, ES <: HList]
    /*(implicit CS: coproduct.Unifier.Aux[CS, WidgetCommand], ES: LUBConstraint[ES, WidgetEvent])*/
   // todo: implicit witness that CS elements can all be unwrapped to WidgetCommand and ES elements can all be unwrapped to WidgetEvent
{
  widgetApi =>

  /** Expose this as a mature widget with the provided label.
   *
   * @tparam Label  the label for this widget.
   */
  def as[Label](label: Label) = new {
    def apply[CSES, CSL <: Coproduct, ESL <: HList]
      (implicit CSES: CSES <:< (CSL, ESL),
       csl: coproduct.Mapper.Aux[WidgetApi.Labeller.type, CS, CSL],
       esl: hlist.Mapper.Aux[WidgetApi.Labeller.type, ES, ESL]) = WidgetImpl[CSES, CSL, ESL, CS, ES, Label](
          widgetApi, label, csl.apply, esl.apply)
  }
}

object WidgetApi {

  object Labeller extends Poly1 {
    implicit def caseCommand[A](a: A) = at[A] { a => Tag[A, String](a) }
  }

}

/** A labelled widget.
 *
 * A widget is baked from a WidgetApi by adding a label to its commands and events.
 */
sealed trait WidgetImpl[CSES] {
  type LCommands <: Coproduct
  type LEvents <: HList
  type Commands <: Coproduct
  type Events <: HList
  type Label

  def api: WidgetApi[Commands, Events]
  def label: Label
  def labelCommands: Commands => LCommands
  def labelEvents: Events => LEvents
}

object WidgetImpl {

  def apply[CSES, CSL <: Coproduct, ESL <: HList, CS <: Coproduct, ES <: HList, L](fromApi: WidgetApi[CS, ES], withLabel: L, lcs: CS => CSL, les: ES => ESL)
      (implicit CSES: CSES <:< (CSL, ESL)) = new WidgetImpl[CSES] {
    override type LCommands = CSL
    override type LEvents = ESL
    override type Commands = CS
    override type Events = ES
    override type Label = L

    type Labelled[A] = A @@ Label

    override val api: WidgetApi[Commands, Events] = fromApi
    override val label: Label = withLabel
    override val labelCommands: Commands => LCommands = lcs
    override val labelEvents: Events => LEvents = les
  }

  type Aux[CSES, CSL, ESL, CS, ES, L] = WidgetImpl[CSES] {
    type LCommands = CSL
    type LEvents = ESL
    type Commands = CS
    type Events = ES
    type Label = L
  }
}


trait WidgetDsl {

  type Widget[A] = FreeAp[WidgetImpl, A]

  def text(txt: String) = FreeAp.lift(Text(txt).as("x").apply)

}



case class Text(txt: String) extends WidgetApi[Text.Set :+: Text.Modify :+: CNil, Text.UpdatedTo :: HNil]


object Text {
  type Set = WidgetCommand.Set[String] @@ Text
  type Modify = WidgetCommand.Modify[String] @@ Text
  type UpdatedTo = WidgetEvent.UpdatedTo[String] @@ Text
}


case class Checkbox(enabled: Boolean) extends WidgetApi[Checkbox.Set :+: Checkbox.Modify :+: CNil, Checkbox.UpdatedTo :: HNil]

object Checkbox {
  type Set = WidgetCommand.Set[Boolean] @@ Checkbox
  type Modify = WidgetCommand.Modify[Boolean] @@ Checkbox
  type UpdatedTo = WidgetEvent.UpdatedTo[Boolean] @@ Checkbox
}


case class RadioButton(enabled: Boolean) extends WidgetApi[RadioButton.Set :+: RadioButton.Modify :+: CNil, RadioButton.UpdatedTo :: HNil]

object RadioButton {
  type Set = WidgetCommand.Set[Boolean] @@ RadioButton
  type Modify = WidgetCommand.Modify[Boolean] @@ RadioButton
  type UpdatedTo = WidgetEvent.UpdatedTo[Boolean] @@ RadioButton
}
