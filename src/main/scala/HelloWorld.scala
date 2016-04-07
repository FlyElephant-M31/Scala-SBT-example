import scala.io.Source
import scala.util.control.Breaks._
import math.abs
import sys.process._

import breeze.linalg._
import breeze.plot._
import scalax.chart.api._


object GradientDescent {

  def getData = {
    var mileage:List[Float] = Nil
    var price:List[Float] = Nil

    try {
      for(line <- Source.fromFile("Data.csv").getLines()) {
        val tab = line.split(",")
        if (tab.length == 2 && line != "mileage(in km),price(in euro)") {
          mileage = mileage ::: List(tab(0).toFloat / 1000)
          price = price ::: List(tab(1).toFloat / 1000)
        }
      }
    } catch {
      case t:Throwable => {
        println("Unable to get data, please make sure that you have access to Dataset/Data.csv");
        System.exit(0)
      }
    }
    (mileage, price)
  }

  def gradientDescent(mileage:List[Float], price:List[Float], theta0:Float, theta1:Float) : (Float, Float) = {
    val learningRate = 0.0001f
    val tmpTheta0 = learningRate * (Function.getSum0(mileage, price, theta0, theta1) / (price.length).toFloat)
    val tmpTheta1 = learningRate * (Function.getSum1(mileage, price, theta0, theta1) / (price.length).toFloat)

    if (abs(tmpTheta0) < 0.000001 && abs(tmpTheta1) < 0.000001) (theta0 * 1000, theta1)
    else gradientDescent(mileage, price, (theta0 - tmpTheta0), (theta1 - tmpTheta1))
  }

  def main(av:Array[String]) {
    val (mileage, price) = getData
    val (theta0, theta1) = gradientDescent(mileage, price, 0.0f, 0.0f)

    val ln = 30000
    val res = (theta1 * ln.toFloat) + theta0
    println(s"This car worth ${res.toInt} euro")

    val estimate = mileage.map(x => (theta1 * (x * 1000)) + theta0)

   /* val f = Figure()
    val p = f.subplot(0)
    p += plot(mileage.map(x => x * 1000), price.map(x => x * 1000), '.')
    p += plot(mileage.map(x => x * 1000), estimate)
    p.xlabel = "mileage (in km)"
    p.ylabel = "price (in euro)"

    f.saveas("dataGraph.png")
*/
    val data = for (i <- 1 to 5) yield (5,i)
    val chart = XYLineChart(data)
    chart.saveAsPNG("chart.png")

    }


}