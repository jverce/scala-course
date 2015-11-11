object week3 {
  val a = new Rational(1, 3)                      //> a  : Rational = 1/3
  val b = new Rational(5, 7)                      //> b  : Rational = 5/7
  val c = new Rational(3, 2)                      //> c  : Rational = 3/2
  a.sub(b).sub(c)                                 //> res0: Rational = -79/42
  c.max(a)                                        //> res1: Rational = 3/2
  val d = new Rational(1)                         //> d  : Rational = 1
}

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be non-zero")
  
  def this(x: Int) = this(x, 1)
  
  private def gcd(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => gcd(b, a % b)
  }
  private val g = gcd(x, y)
  val numer = x / g
  val denom = y / g
    
  def less(that: Rational): Boolean =
    this.numer * that.denom < that.numer * this.denom
  
  def max(that: Rational): Rational = if (this.less(that)) that else this
  
  def add(that: Rational): Rational =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)
  
  def neg: Rational = new Rational(-this.numer, this.denom)
  
  def sub(that: Rational): Rational = this.add(that.neg)
  
  override def toString =
    this.numer +
    (if (this.denom != 1) "/" + this.denom else "")
}