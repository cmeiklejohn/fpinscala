import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import akka.actor.ActorSystem

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

object GreetingApplication extends App {
  val system = ActorSystem("GreetingSystem")
  val greeter = system.actorOf(Props[GreetingActor], "greeter")

  greeter ! Greeting("Charlie Parker")
  greeter ! Angry
  greeter ! Greeting("Amelie Poulin")

  system.shutdown
}
