import sbt._
import sbt.Keys._

/**
  * Hold generated sources for extra types.
  */
object GeneratedExtension {
  private val java8TimeSource = {
    val java8Time =
      Seq("LocalDate", "LocalTime", "LocalDateTime", "OffsetTime", "OffsetDateTime", "ZonedDateTime", "Instant")
        .map { tpe =>
          s"""
         implicit val ${tpe}W = W[$tpe](dt => Js.Str(dt.toString))
         implicit val ${tpe}R = R[$tpe]{case Js.Str(s) => $tpe.parse(s)}
         """
        }

    val src=s"""
      package upickle
      import java.time._

      /**
       * Auto-generated picklers and unpicklers, used for java8 time types.
       */
      trait GeneratedJava8Time extends Types{
        import Aliases._
        ${java8Time.mkString("\n")}
      }
    """
    ("GeneratedJava8Time",src)
  }

  val nameAndSource:Seq[(String,String)]=Seq(java8TimeSource)
}