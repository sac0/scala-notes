package akkaessentials.actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object IntroAkkaConfiguration extends App {


  class SimpleLoggingActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  /**
   * Inline configuration
   */
  val configString =
    """
      |akka {
      | loglevel = "DEBUG"
      |}
      |""".stripMargin
  val config = ConfigFactory.parseString(configString)
  val system = ActorSystem("ConfigurationDemo", ConfigFactory.load(config))
  val actor = system.actorOf(Props[SimpleLoggingActor])

  actor ! "A message to remember"

  /**
   * Config file default in resources - application.conf
   */
  val defaultConfigFileSystem = ActorSystem("DefaultConfigFileDemo")
  val defaultConfigFileActor = defaultConfigFileSystem.actorOf(Props[SimpleLoggingActor])
  defaultConfigFileActor ! "A default message to remember"

  /**
   * Separate config in the same file
   */
  val specialConfig = ConfigFactory.load().getConfig("mySpecialConfig")
  val specialConfigSystem = ActorSystem("SpecialConfigSystem",specialConfig)
  val specialConfigFileActor = specialConfigSystem.actorOf(Props[SimpleLoggingActor])
  specialConfigFileActor ! "A special message to remember"

  /**
   * 4 - separate config in another file
   */

  val separateConfig = ConfigFactory.load("sample-conf/sample.conf")
  println(s"separate config log level: ${separateConfig.getString("akka.loglevel")}")

  /**
   * 5 - different file formats
   * JSON, Properties
   */
  val jsonConfig = ConfigFactory.load("sample-conf/jsonConfig.json")
  println(s"json config: ${jsonConfig.getString("aJsonProperty")}")
  println(s"json config: ${jsonConfig.getString("akka.loglevel")}")

  val propsConfig = ConfigFactory.load("sample-conf/propsConfiguration.properties")
  println(s"properties config: ${propsConfig.getString("my.simpleProperty")}")
  println(s"properties config: ${propsConfig.getString("akka.loglevel")}")

}
