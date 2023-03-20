package upickle 
object Micro  {
  
  val longStrings: Array[String] = Array.fill(1000)(
    """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed
      |do eiusmod tempor incididunt ut labore et dolore magna aliqua.
      |Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
      |nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
      |reprehenderit in voluptate velit esse cillum dolore eu fugiat 1a
      |pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
      |culpa qui officia deserunt mollit anim id est laborum.""".stripMargin
  )
  val shortStrings: Array[String] = Array.tabulate(1000)(i => "hello " + i)
  val unicodeStrings: Array[String] = Array.fill(1000)(
    "虾饺 烧卖 凤爪 糯米鸡 萝卜糕 小笼包 肠粉 叉烧包 莲蓉包 流沙包 蛋挞 春卷 锅贴"
  )
  type NestedADT = Generic.ADT[
    Generic.ADT[Boolean, Boolean, Boolean, Boolean, Boolean, Boolean],
    Generic.ADT[Boolean, Boolean, Boolean, Boolean, Boolean, Boolean],
    Generic.ADT[Boolean, Boolean, Boolean, Boolean, Boolean, Boolean],
    Generic.ADT[Boolean, Boolean, Boolean, Boolean, Boolean, Boolean],
    Generic.ADT[Boolean, Boolean, Boolean, Boolean, Boolean, Boolean],
    Generic.ADT[Boolean, Boolean, Boolean, Boolean, Boolean, Boolean]
  ]
  val nestedCaseClasses: Seq[NestedADT] =
    Seq.tabulate(1000) { i =>
      val b = i % 2 == 0
      Generic.ADT(
        Generic.ADT(b, b, b, b, b, b),
        Generic.ADT(b, b, b, b, b, b),
        Generic.ADT(b, b, b, b, b, b),
        Generic.ADT(b, b, b, b, b, b),
        Generic.ADT(b, b, b, b, b, b),
        Generic.ADT(b, b, b, b, b, b)
      )
    }

  val sealedTraits: Seq[DeepHierarchy.A] = Seq.tabulate(1000) { i =>
    i % 3 match{
      case 0 => DeepHierarchy.B(1)
      case 1 => DeepHierarchy.D("")
      case 2 => DeepHierarchy.E(true)
    }
  }

  val integers = Seq.tabulate(1000)(n =>
    n * 123456789
  )
  val doubles = Seq.tabulate(1000)(n =>
    n * 1234.56789
  )
  val sequences = Seq.tabulate(1000)(n =>
    Seq.fill(n)(true)
  )
}
