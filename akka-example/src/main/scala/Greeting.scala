import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import akka.actor.ActorSystem

import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._
import scala.concurrent.duration._

case object Happy
case object Angry
case class Greeting(who: String)

class GreetingActor extends Actor {
  val log = Logging(context.system, this)

  def receive = happy

  val happy: Actor.Receive = {
    case Greeting(who) => log.info("Hello " + who)
    case Angry         => context become angry
  }

  val angry: Actor.Receive = {
    case Greeting(_) => log.info("Go away!")
    case Happy       => context become happy
  }
}

class GreetingSupervisor extends Actor {
  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.minute) {
      case _: ArithmeticException  => Resume
      case _: NullPointerException => Restart
      case _: Exception            => Escalate
    }

    def receive = {
      case p: Props => sender() ! context.actorOf(p)
    }
}

object GreetingApplication extends App {
  val system = ActorSystem("GreetingSystem")
  val supervisor = system.actorOf(Props[GreetingSupervisor], "supervisor")
  val greeter = system.actorOf(Props[GreetingActor], "greeter")

  greeter ! Greeting("Charlie Parker")
  greeter ! Angry
  greeter ! Greeting("Amelie Poulin")

  system.terminate
}
