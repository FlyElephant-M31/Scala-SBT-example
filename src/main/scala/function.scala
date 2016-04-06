object Function {

  def linear(theta0:Float, theta1:Float, mileage:Float) = {
    theta0 + (theta1 * mileage)
  }

  def getSum0(mileage:List[Float], price:List[Float], theta0:Float, theta1:Float) : Float = {
    var res = 0.0f
    for (i <- 0 to (price.length - 1)) {
      res += linear(theta0, theta1, mileage(i)) - price(i)
    }
    res
  }

  def getSum1(mileage:List[Float], price:List[Float], theta0:Float, theta1:Float) : Float = {
    var res = 0.0f
    for (i <- 0 to (price.length - 1)) {
      res += (linear(theta0, theta1, mileage(i)) - price(i)) * mileage(i)
    }
    res
  }

}