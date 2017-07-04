package java8time

import java.time._

import upickle.default._
import utest._
import utest.framework.{Test, Tree}


object Java8TimeTest extends TestSuite {
  val tests: Tree[Test] = this {
    'LocalDate {
      val date = LocalDate.of(2017, 2, 1)
      val dateSer = write(date)
      println(dateSer)
      val dateDeser = read[LocalDate](dateSer)
      assert(dateDeser.equals(date))
    }
    'LocalTime {
      val time = LocalTime.of(22, 1)
      val timeSer = write(time)
      println(timeSer)
      val timeDeser = read[LocalTime](timeSer)
      assert(time == timeDeser)
    }
    'InstantAndCaseClass {
      val snapshot = Instant.now()
      val data = MyData(snapshot,89843L)
      val dataSer=write(data)
      println(dataSer)
      val dataDeser=read[MyData](dataSer)
      assert(dataDeser.snapshot == snapshot)
    }
    'Instant2ZonedDateTime{
      val snapshot = Instant.now()
      val data=MyData(snapshot,89843L)
      val dataSer=write(data)
      val dataDeser2=read[MyData2](dataSer)
      println(dataDeser2)
      assert(dataDeser2.isInstanceOf[MyData2] && dataDeser2.snapshot.toInstant == data.snapshot)
    }
    'OtherTypes{
      val t1=LocalDateTime.now()
      val ser1=write(t1)
      val deser1=read[LocalDateTime](ser1)
      assert(t1==deser1)

      val t2=OffsetTime.now()
      val ser2=write(t2)
      val deser2=read[OffsetTime](ser2)
      assert(t2==deser2)

      val t3=OffsetDateTime.now()
      val ser3=write(t3)
      val deser3=read[OffsetDateTime](ser3)
      assert(t3==deser3)
    }
  }
}
case class MyData(snapshot: Instant, data: Long)
case class MyData2(snapshot: ZonedDateTime,data:Long)