import java.awt.BasicStroke

import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.DefaultXYDataset
import org.jfree.util.ShapeUtilities

import scala.io.Source
import scala.math.abs
import scalax.chart.api._


object GradientDescent {

  def getData = {
    val mileagesAndPrices = for {
      line <- Source.fromFile("Data.csv").getLines()
      tab = line.split(",") if tab.length == 2 && ! line.contains("mileage")
      m = tab(0).toFloat / 1000f
      p = tab(1).toFloat / 1000f
    } yield (m, p)
    mileagesAndPrices.toList.unzip
  }

  def gradientDescent(mileage:List[Float], price:List[Float], theta0:Float, theta1:Float) : (Double, Double) = {
    val learningRate = 0.0001f
    val tmpTheta0 = learningRate * (Function.getSum0(mileage, price, theta0, theta1) / price.length.toFloat)
    val tmpTheta1 = learningRate * (Function.getSum1(mileage, price, theta0, theta1) / price.length.toFloat)

    if (abs(tmpTheta0) < 0.000001 && abs(tmpTheta1) < 0.000001) (theta0 * 1000, theta1)
    else gradientDescent(mileage, price, theta0 - tmpTheta0, theta1 - tmpTheta1)
  }

  def main(av:Array[String]) {
    val (mileage, price) = getData
    val (theta0, theta1) = gradientDescent(mileage, price, 0.0f, 0.0f)

    val ln = 30000
    val res = (theta1 * ln.toFloat) + theta0
    println(s"This car worth ${res.toInt} euro")

    val xs = mileage map { _.toDouble * 1000 }
    val ys = price map { _.toDouble * 1000 }
    val estimates = xs map { theta1 * _ + theta0 }

    val dataSet = new DefaultXYDataset()
    dataSet.addSeries("Prices", Array(xs.toArray, ys.toArray))
    dataSet.addSeries("Estimates", Array(xs.toArray, estimates.toArray))

    val chart = XYLineChart(dataSet)
    val renderer = new XYLineAndShapeRenderer()
    renderer.setSeriesStroke(1, new BasicStroke(3f))
    val shape = ShapeUtilities.createDownTriangle(50f)
    renderer.setBaseShape(shape)
    chart.plot.setRenderer(renderer)
    chart.saveAsPNG("chart.png")
  }


}