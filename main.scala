package name.aloise.algorithms

object Solution {

  case class Point(x:Double,y:Double)

  import scala.io.StdIn


  // https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line

  def lineDistance( line:(Point,Point) ):Point => Double = {

    line match {
      case ( Point(x1,y1), Point(x2,y2) ) =>

        val divider = Math.sqrt( ( y2 - y1 )*( y2 - y1 ) + ( x2-x1 )*(x2-x1) )
        val dy = y2 - y1
        val dx = x2 - x1
        val rest = x2*y1 - y2*x1

        ( point:Point ) => ( dy*point.x - dx*point.y + rest ) / divider
    }
  }

  // barycentric coordinate system
  def isInsideTheTriangle( triangle:(Point,Point,Point) ): Point => Boolean = {

    triangle match {
      case (Point(x1, y1), Point(x2, y2), Point(x3, y3)) =>


        val denominator = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
        val dy2y3 = y2 - y3
        val dx3x2 = x3 - x2
        val dy3y1 = y3 - y1
        val dx1x3 = x1 - x3

        ( point:Point ) => {
          lazy val a = ( dy2y3 * (point.x - x3) + dx3x2 * (point.y - y3)) / denominator
          lazy val b = ( dy3y1 * (point.x - x3) + dx1x3 * (point.y - y3)) / denominator
          lazy val c = 1 - a - b

          ( 0 <= a && a <= 1 ) && ( 0 <= b && b <= 1 ) && ( 0 <= c && c <= 1 )

        }

    }

  }

  def quickHull( points:List[Point] ):List[Point] = {

    def findHull( set:List[(Point,Double)], point1: Point, point2: Point ):List[Point] = {
      if(set.isEmpty )
          Nil
        else {

          val (maxDistancePoint, _) = set.foldLeft( set.head) { case (maxDistanceItem, item) =>
            if (Math.abs(item._2) > Math.abs(maxDistanceItem._2))
              item
            else
              maxDistanceItem
          }

          val belongsFunc = isInsideTheTriangle( ( point1, point2, maxDistancePoint ) )

          val pointsLeft = set.filter(p => (p._1 != maxDistancePoint) && !belongsFunc(p._1)).map(_._1)

          val distanceSet1Func = lineDistance((point1, maxDistancePoint))
          val set1 = pointsLeft.map( p => (p, distanceSet1Func(p) ) ).filter( _._2 < 0 ) // to the right of the oriented line

          val distanceSet2Func = lineDistance((maxDistancePoint, point2))
          val set2 = pointsLeft.map( p => (p, distanceSet2Func(p) ) ).filter( _._2 < 0 ) // to the right of the oriented line

          findHull( set1, point1, maxDistancePoint ) ::: ( maxDistancePoint :: findHull( set2, maxDistancePoint, point2 ) )

        }
    }


    val leftPoint = points.foldLeft(points.head) { case (min, current) => if (current.x < min.x) current else min }
    val rightPoint = points.foldLeft(points.head) { case (max, current) => if (current.x > max.x) current else max }

    val distanceFuncSet1 = lineDistance((leftPoint, rightPoint))
    val pointsWithDistanceSet = points.map(p => (p, distanceFuncSet1(p)))

    val set1 = pointsWithDistanceSet.filter(_._2 < 0) // on the top/right of the line

    val set2 = pointsWithDistanceSet.filter(_._2 > 0) // bottom/left of the line

    ( leftPoint :: findHull( set1, leftPoint, rightPoint ) ) ::: ( rightPoint :: findHull( set2, rightPoint, leftPoint ) )

  }

  def perimeter(poly:List[Point]):Double = {
    poly match {
      case Nil => 0
      case head :: tail =>
        val ( _, perimeter ) =
          ( tail :+ head ) .foldLeft(( head, 0.0 )){ case ( (previousPoint, perimeterSum), point ) =>
            ( point, perimeterSum + Math.sqrt( Math.pow( point.x - previousPoint.x , 2 ) + Math.pow( point.y - previousPoint.y, 2 ) ) )
          }
        perimeter
    }
  }


  def main(args: Array[String]): Unit = {

    val n = StdIn.readInt()
    val list = ( for( i <- 1 to n ) yield StdIn.readLine() ).map { str =>
      val coords = str.split(" ",2)
      Point( coords(0).toDouble, coords(1).toDouble)

    }

//    val maxc = 10
//    val dScale = 1000
//    val n = 10000000 // StdIn.readInt()
//    val list = List( (1.0, 1.0), (2.0, 5.0), ( 3.0, 3.0 ), ( 5.0, 3.0 ), (3.0, 2.0 ), ( 2.0, 2.0 ) ) //
//    val list = for( i <- 1 to n ) yield ( util.Random.nextInt(maxc*dScale).toDouble/dScale, util.Random.nextInt(maxc*dScale).toDouble/dScale )

//    println( "before", list.distinct )

    // val result = quickHull( list.distinct.map( Point.tupled ).toList )
    val result = quickHull( list.distinct.toList )

//    println( "after", result.flatMap(Point.unapply) )
//    println("perimeter", perimeter(result))
    println( perimeter(result))
  }

}
