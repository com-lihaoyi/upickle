package upickle

object Common{
  import ADTs.ADT0
  import Defaults._
  import Generic.ADT
  import Hierarchy._
  import Recursive._
  type Data = ADT[Seq[(Boolean, String, Int, Double)], String, A, LL, ADTc, ADT0]
  val benchmarkSampleData: Seq[Data] = Seq.fill(1000)(ADT(
    Vector(
      (true,  "zero",  0, 0),
      (false, "one",   1, 1.1),
      (true,  "two",   2, 22.22),
      (false, "three", 3, 333.33),
      (true,  "four",  4, 4444.4444),
      (false, "five",  5, 55555.55555),
      (true,  "six",   6, 666666.666666),
      (false, "seven", 7, 7777777.7777777),
      (true,  "eight", 8, 88888888.88888888),
      (false, "nine",  9, 999999999.999999999)
    ),
    """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed
      |do eiusmod tempor incididunt ut labore et dolore magna aliqua.
      |Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
      |nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
      |reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
      |pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
      |culpa qui officia deserunt mollit anim id est laborum.""".stripMargin,
    C("虾饺 烧卖 凤爪 糯米鸡 萝卜糕 小笼包 肠粉", "叉烧包 莲蓉包 流沙包 蛋挞 春卷 锅贴"),
    Node(
      -11111111,
      Node(
        222222222,
        Node(
          -33333333,
          Node(
            44444444,
            Node(
              -55555555,
              Node(
                66666666,
                Node(
                  -77777777,
                  Node(
                    88888888,
                    End
                  )
                )
              )
            )
          )
        )
      )
    ),
    ADTc(
      i = 1234567890,
      s = "I am cow hear me moo I weigh twice as much as you and I look good on the barbecue"
    ),
    ADT0()
  ))
  val benchmarkSampleJson = upickle.default.write(benchmarkSampleData)
  val benchmarkSampleMsgPack = upickle.default.writeBinary(benchmarkSampleData)

  println("benchmarkSampleJson " + benchmarkSampleJson.size + " bytes")
  println("benchmarkSampleMsgPack " + benchmarkSampleMsgPack.size + " bytes")

  def upickleLegacy(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}

    implicit def rw1: RW[Data] = upickle.legacy.macroRW
    implicit def rw2: RW[A] = upickle.legacy.macroRW
    implicit def rw3: RW[B] = upickle.legacy.macroRW
    implicit def rw4: RW[C] = upickle.legacy.macroRW
    implicit def rw5: RW[LL] = upickle.legacy.macroRW
    implicit def rw6: RW[Node] = upickle.legacy.macroRW
    implicit def rw7: RW[End.type] = upickle.legacy.macroRW
    implicit def rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit def rw9: RW[ADT0] = upickle.legacy.macroRW


    bench[String](duration)(
      upickle.legacy.read[Seq[Data]](_),
      upickle.legacy.write(_)
    )


  }
  def upickleDefault(duration: Int) = {

    bench[String](duration)(
      upickle.default.read[Seq[Data]](_),
      upickle.default.write(_)
    )
  }

  def upickleBinaryLegacy(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}

    implicit def rw1: RW[Data] = upickle.legacy.macroRW
    implicit def rw2: RW[A] = upickle.legacy.macroRW
    implicit def rw3: RW[B] = upickle.legacy.macroRW
    implicit def rw4: RW[C] = upickle.legacy.macroRW
    implicit def rw5: RW[LL] = upickle.legacy.macroRW
    implicit def rw6: RW[Node] = upickle.legacy.macroRW
    implicit def rw7: RW[End.type] = upickle.legacy.macroRW
    implicit def rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit def rw9: RW[ADT0] = upickle.legacy.macroRW


    bench[Array[Byte]](duration)(
      upickle.legacy.readBinary[Seq[Data]](_),
      upickle.legacy.writeBinary(_)
    )


  }
  def upickleBinaryDefault(duration: Int) = {

    bench[Array[Byte]](duration)(
      upickle.default.readBinary[Seq[Data]](_),
      upickle.default.writeBinary(_)
    )
  }

//  def genCodec(duration: Int) = {
//    import com.avsystem.commons.serialization._
//
//    implicit def gc1: GenCodec[Data] = GenCodec.materialize
//    implicit def gc2: GenCodec[A] = GenCodec.materialize
//    implicit def gc3: GenCodec[B] = GenCodec.materialize
//    implicit def gc4: GenCodec[C] = GenCodec.materialize
//    implicit def gc5: GenCodec[LL] = GenCodec.materialize
//    implicit def gc6: GenCodec[Node] = GenCodec.materialize
//    implicit def gc7: GenCodec[End.type] = GenCodec.materialize
//    implicit def gc8: GenCodec[ADTc] = GenCodec.materialize
//    implicit def gc9: GenCodec[ADT0] = GenCodec.materialize
//
//    bench[String](duration)(
//      json.JsonStringInput.read[Data](_),
//      json.JsonStringOutput.write[Data](_)
//    )
//  }


  def upickleLegacyCached(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}

    implicit lazy val rw1: RW[Data] = upickle.legacy.macroRW
    implicit lazy val rw2: RW[A] = upickle.legacy.macroRW
    implicit lazy val rw3: RW[B] = upickle.legacy.macroRW
    implicit lazy val rw4: RW[C] = upickle.legacy.macroRW
    implicit lazy val rw5: RW[LL] = upickle.legacy.macroRW
    implicit lazy val rw6: RW[Node] = upickle.legacy.macroRW
    implicit lazy val rw7: RW[End.type] = upickle.legacy.macroRW
    implicit lazy val rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit lazy val rw9: RW[ADT0] = upickle.legacy.macroRW

    bench[String](duration)(
      upickle.legacy.read[Seq[Data]](_),
      upickle.legacy.write(_)
    )
  }

  def upickleDefaultCached(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[String](duration)(
      upickle.default.read[Seq[Data]](_),
      upickle.default.write(_)
    )
  }
  def upickleDefaultByteArray(duration: Int) = {
    bench[Array[Byte]](duration)(
      upickle.default.read[Seq[Data]](_),
      upickle.default.writeToByteArray(_)
    )
  }
  def upickleDefaultCachedByteArray(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[Array[Byte]](duration)(
      upickle.default.read[Seq[Data]](_),
      upickle.default.writeToByteArray(_)
    )
  }
  def upickleDefaultCachedReadable(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[String](duration)(
      x => upickle.default.read[Seq[Data]](x: geny.Readable),
      upickle.default.write(_)
    )
  }

  def upickleDefaultCachedReadablePath(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[java.nio.file.Path](duration)(
      file => upickle.default.read[Seq[Data]](java.nio.file.Files.newInputStream(file): geny.Readable),
      data => java.nio.file.Files.write(
        java.nio.file.Files.createTempFile("temp", ".json"),
        upickle.default.write(data).getBytes
      ),
      checkEqual = false
    )
  }

  def upickleLegacyBinaryCached(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}

    implicit lazy val rw1: RW[Data] = upickle.legacy.macroRW
    implicit lazy val rw2: RW[A] = upickle.legacy.macroRW
    implicit lazy val rw3: RW[B] = upickle.legacy.macroRW
    implicit lazy val rw4: RW[C] = upickle.legacy.macroRW
    implicit lazy val rw5: RW[LL] = upickle.legacy.macroRW
    implicit lazy val rw6: RW[Node] = upickle.legacy.macroRW
    implicit lazy val rw7: RW[End.type] = upickle.legacy.macroRW
    implicit lazy val rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit lazy val rw9: RW[ADT0] = upickle.legacy.macroRW

    bench[Array[Byte]](duration)(
      upickle.legacy.readBinary[Seq[Data]](_),
      upickle.legacy.writeBinary(_)
    )
  }

  def upickleDefaultBinaryCached(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[Array[Byte]](duration)(
      upickle.default.readBinary[Seq[Data]](_),
      upickle.default.writeBinary(_)
    )
  }
  def upickleDefaultBinaryCachedReadable(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[Array[Byte]](duration)(
      x => upickle.default.readBinary[Seq[Data]](x: geny.Readable),
      upickle.default.writeBinary(_)
    )
  }

//  def genCodecCached(duration: Int) = {
//    import com.avsystem.commons.serialization._
//
//    implicit lazy val gc1: GenCodec[Data] = GenCodec.materialize
//    implicit lazy val gc2: GenCodec[A] = GenCodec.materialize
//    implicit lazy val gc3: GenCodec[B] = GenCodec.materialize
//    implicit lazy val gc4: GenCodec[C] = GenCodec.materialize
//    implicit lazy val gc5: GenCodec[LL] = GenCodec.materialize
//    implicit lazy val gc6: GenCodec[Node] = GenCodec.materialize
//    implicit lazy val gc7: GenCodec[End.type] = GenCodec.materialize
//    implicit lazy val gc8: GenCodec[ADTc] = GenCodec.materialize
//    implicit lazy val gc9: GenCodec[ADT0] = GenCodec.materialize
//
//    bench[String](duration)(
//      json.JsonStringInput.read[Data](_),
//      json.JsonStringOutput.write[Data](_)
//    )
//  }

  def bench[T](duration: Int)
              (f1: T => Seq[Data], f2: Seq[Data] => T, checkEqual: Boolean = true)
              (implicit name: sourcecode.Name) = {
    val stringified = f2(benchmarkSampleData)
    val r1 = f1(stringified)
    val equal = benchmarkSampleData == r1

    if (checkEqual) {
      assert(equal)
      val rewritten = f2(f1(stringified))
      (stringified, rewritten) match {
        case (lhs: Array[_], rhs: Array[_]) => assert(lhs.toSeq == rhs.toSeq)
        case _ => assert(stringified == rewritten)
      }
    }
    bench0[T, Seq[Data]](duration, stringified)(f1, f2)
  }

  def bench0[T, V](duration: Int, stringified: T)
                  (f1: T => V, f2: V => T)
                  (implicit name: sourcecode.Name)= {
    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        f1(stringified)
        n += 1
      }
      println(name.value + " Read " + n)
    }

    val parsed = f1(stringified)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        f2(parsed)
        n += 1
      }
      println(name.value + " Write " + n)
    }
  }
}
