object week3 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(43); 
  val a = new Rational(1, 3);System.out.println("""a  : Rational = """ + $show(a ));$skip(29); 
  val b = new Rational(5, 7);System.out.println("""b  : Rational = """ + $show(b ));$skip(29); 
  val c = new Rational(3, 2);System.out.println("""c  : Rational = """ + $show(c ));$skip(8); val res$0 = 
  a / c;System.out.println("""res0: Rational = """ + $show(res$0))}
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
    
  def < (that: Rational): Boolean =
    this.numer * that.denom < that.numer * this.denom
  
  def max(that: Rational): Rational = if (this < that) that else this
  
  def + (that: Rational): Rational =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)
  
  def unary_- = new Rational(-this.numer, this.denom)
  
  def - (that: Rational): Rational = this + -that
  
  def * (that: Rational): Rational =
    new Rational(
      this.numer * that.numer,
      this.denom * that.denom)
  
  def / (that: Rational): Rational =
    new Rational(
      this.numer * that.denom,
      this.denom * that.numer)
  
  override def toString =
    this.numer +
    (if (this.denom != 1) "/" + this.denom else "")
}
