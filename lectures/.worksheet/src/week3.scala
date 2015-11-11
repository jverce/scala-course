object week3 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(43); 
  val a = new Rational(1, 3);System.out.println("""a  : Rational = """ + $show(a ));$skip(29); 
  val b = new Rational(5, 7);System.out.println("""b  : Rational = """ + $show(b ));$skip(29); 
  val c = new Rational(3, 2);System.out.println("""c  : Rational = """ + $show(c ));$skip(18); val res$0 = 
  a.sub(b).sub(c);System.out.println("""res0: Rational = """ + $show(res$0));$skip(11); val res$1 = 
  c.max(a);System.out.println("""res1: Rational = """ + $show(res$1));$skip(26); 
  val d = new Rational(1);System.out.println("""d  : Rational = """ + $show(d ))}
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
