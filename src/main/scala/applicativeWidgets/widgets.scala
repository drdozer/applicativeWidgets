package applicativeWidgets

import scalaz._
import shapeless._
import shapeless.ops._
import shapeless.{:+:, Coproduct}
import shapeless.PolyDefns.~>
import shapeless.ops.coproduct.Basis
import shapeless.ops.hlist.ToCoproductTraversable

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
  case class UpdatedTo[T](newValue: T) extends WidgetEvent
}

//@implicitNotFound("Could not unwrap a ${A} from ${T}")
//trait Unwrapper[A, T] {
//  def apply(t: T): A
//}
//
//object Unwraper {
//  implicit def at[A, T](implicit ta: T <:< A): Unwrapper[A, T] = new Unwrapper[A, T] {
//    override def apply(t: T) = ta(t)
//  }
//
//  implicit def inTag[A, T, Tag](implicit atT: Unwrapper[A, T]): Unwrapper[A, T @@ Tag] = new Unwrapper[A, @@[T, Tag]] {
//    override def apply(t: @@[T, Tag]) = atT(Tag.unwrap(t))
//  }
//}


/** The widget API.
 *
 * At any evaluation cycle, a widget respons to at most one of a selection of commands,
 * and emmits zero or more of a list of events.
 *
 * @tparam CS    a Coproduct of WidgetCommand types that this widget can handle
 * @tparam ES    a Coproduct of WidgetEvent types that this widget may emmit
 */
abstract class WidgetApi[CS <: Coproduct, ES <: Coproduct]
    /*(implicit CS: coproduct.Unifier.Aux[CS, WidgetCommand], ES: LUBConstraint[ES, WidgetEvent])*/
   // todo: implicit witness that CS elements can all be unwrapped to WidgetCommand and ES elements can all be unwrapped to WidgetEvent
{
  widgetApi =>

  /** Expose this as a mature widget with the provided label.
   *
   * @tparam Label  the label for this widget.
   */
  def as[Label](label: Label)(implicit csl: Labeller[Label, CS], esl: Labeller[Label, ES]) = {
    WidgetImpl[(csl.Out, esl.Out), csl.Out, esl.Out, CS, ES, Label](
          widgetApi, label, csl.apply(_ : CS), esl.apply(_ : ES))
  }
}

sealed trait Labeller[Label, C <: Coproduct] extends DepFn1[C] with Serializable { type Out <: Coproduct }

object Labeller {
  def apply[Label, C <: Coproduct](implicit labeller: Labeller[Label, C]): Labeller.Aux[Label, C, labeller.Out] =
    labeller

  type Aux[Label, C <: Coproduct, Out0 <: Coproduct] = Labeller[Label, C] { type Out <: Out0 }
  type TagOf[Label] = {
    type t[A] = A @@ Label
  }

  implicit def cnilMapper[Label]: Aux[Label, CNil, CNil] = new Labeller[Label, CNil] {
    override type Out = CNil
    override def apply(t: CNil): CNil = t
  }

  implicit def cpMapper[Label, H, T <: Coproduct, OutT <: Coproduct]
  (implicit mt: Labeller.Aux[Label, T, OutT]): Labeller.Aux[Label, H :+: T, TagOf[Label]#t[H] :+: OutT] =
    new Labeller[Label, H :+: T] {
      override type Out = TagOf[Label]#t[H] :+: OutT
      override def apply(c: H :+: T): Out = c match {
        case Inl(h) => Inl(Tag.of[Label](h))
        case Inr(t) => Inr(mt(t))
      }
    }
}

/** A labelled widget.
 *
 * A widget is baked from a WidgetApi by adding a label to its commands and events.
 */
trait WidgetImpl[CSES] {
  type LCommands <: Coproduct
  type LEvents <: Coproduct
  type Commands <: Coproduct
  type Events <: Coproduct
  type Label

  def api: WidgetApi[Commands, Events]
  def label: Label
  def labelCommands: Commands => LCommands
  def labelEvents: Events => LEvents
}

object WidgetImpl {

  def apply[CSES, CSL <: Coproduct, ESL <: Coproduct, CS <: Coproduct, ES <: Coproduct, L](fromApi: WidgetApi[CS, ES], withLabel: L, lcs: CS => CSL, les: ES => ESL)
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

    override def toString: String = s"WidgetImpl($api at $label)"
  }

  def unapply[CSES, CS, ES](wi: WidgetImpl[CSES] { type Commands = CS; type Events = ES}): Option[WidgetApi[CS, ES]] =
    Some(wi.api)

  type Aux[CSES, CSL, ESL, CS, ES, L] = WidgetImpl[CSES] {
    type LCommands = CSL
    type LEvents = ESL
    type Commands = CS
    type Events = ES
    type Label = L
  }
}


trait WidgetDsl {
  import shapeless.syntax.singleton._

  type Widget[A] = FreeAp[WidgetImpl, A]

  def widget[WF <: WidgetFactory](wf: WF)(implicit
                                          Commands : Labeller[WF, wf.Commands],
                                          Events : Labeller[WF, wf.Events]) = new {
    def apply[CS <: HList, CSO <: Coproduct](initWith: CS)(implicit
                                                           toCoproductTraversable: ToCoproductTraversable.Aux[CS, List, CSO],
                                                           basis: Basis[wf.Commands, CSO]) =
        FreeAp.lift(wf.Widget(toCoproductTraversable(initWith) map (cso => basis.inverse(Right(cso)))).as(wf))
  }

  def text = widget(Text)
  def checkbox = widget(Checkbox)
  def radioButton = widget(RadioButton)

}


trait WidgetFactory {
  type Commands <: Coproduct
  type Events <: Coproduct

  case class Widget(initWith: List[Commands]) extends WidgetApi[Commands, Events]
}


object Text extends WidgetFactory {
  override type Commands = Set :+: Modify :+: CNil


  type Set = WidgetCommand.Set[String]
  def Set(value: String): Set = WidgetCommand.Set(value)

  type Modify = WidgetCommand.Modify[String]
  def Modify(updateWith: String => String): Modify = WidgetCommand.Modify(updateWith)


  type Events = UpdatedTo :+: CNil

  type UpdatedTo = WidgetEvent.UpdatedTo[String]
  def UpdatedTo(newValue: String): UpdatedTo = WidgetEvent.UpdatedTo(newValue)
}


object Checkbox extends WidgetFactory {
  override type Commands = Set :+: Modify :+: CNil

  type Set = WidgetCommand.Set[Boolean]
  def Set(value: Boolean): Set = WidgetCommand.Set(value)

  type Modify = WidgetCommand.Modify[Boolean]
  def Modify(updateWith: Boolean => Boolean): Modify = WidgetCommand.Modify(updateWith)


  type Events = UpdatedTo :+: CNil

  type UpdatedTo = WidgetEvent.UpdatedTo[Boolean]
  def UpdatedTo(newValue: Boolean): UpdatedTo = WidgetEvent.UpdatedTo(newValue)
}


object RadioButton extends WidgetFactory {
  override type Commands = Set :+: Modify :+: CNil

  type Set = WidgetCommand.Set[Boolean]
  def Set(value: Boolean): Set = WidgetCommand.Set(value)

  type Modify = WidgetCommand.Modify[Boolean]
  def Modify(updateWith: Boolean => Boolean): Modify = WidgetCommand.Modify(updateWith)


  type Events = UpdatedTo :+: CNil

  type UpdatedTo = WidgetEvent.UpdatedTo[Boolean]
  def UpdatedTo(newValue: Boolean): UpdatedTo = WidgetEvent.UpdatedTo(newValue)
}
