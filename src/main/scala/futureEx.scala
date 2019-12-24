import org.scalameter.measure

import scala.concurrent.Future
import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

object futureEx extends App{

  sealed trait Tree{
    // non-parallel version of tree sum method
    def sum(res: Int=0): Int = {
      this match {
        case Node(v,l,r) => {
          v + l.sum(res) + r.sum(res)
        }
        case End => res
      }
    }

    def sumPar(res: Int=0): Future[Int] =
      this match {
        case Node(v, l, r) =>  {
                    val lRes = l.sumPar(res)
                    val rRes = r.sumPar(res)
                    for {
                      lR <- lRes
                      rR <- rRes
                    } yield (v+lR+rR)
//        val fList = List(l.sumPar(res), r.sumPar(res)) //this solution works longer than the one with for comprehension above
//          Future.sequence(fList).map(list => list.sum+v) // hence I come back to for comprehension as final solution
        }
        case End => Future{res}
      }

  }
  // branch of tree
  case class Node(value: Int, left: Tree, right: Tree) extends Tree
  // leaf of tree
  case object End extends Tree

    // test tree
    val tree: Tree = Node(1,
      Node(2, Node(4, End, End), End),
      Node(3, End, Node(5, End, End)) )
    // big test tree created manually
    val complexTree: Tree = Node(1,
      Node(2,
        Node(4,
          Node(8,
            Node(16, End, End),
            Node(24, End, End)),
          Node(14,
            Node(17, End, End),
            Node(25, End, End))
        ),
        Node(6,
          Node(15,
            Node(18, End, End),
            Node(26, End, End)),
          Node(9,
            Node(19, End, End),
            Node(27, End, End))
        )
      ),
      Node(3,
        Node(7,
          Node(13,
            Node(20, End, End),
            Node(28, End, End)),
          Node(10,
            Node(21, End, End),
            Node(29, End, End))
        ),
        Node(5,
          Node(11,
            Node(22, End, End),
            Node(30, End, End)),
          Node(12,
            Node(23, End, End),
            Node(31, End, End))
        )
      )
    )
    // simple test tree
    val simpleTree: Tree = Node(1,
      Node(2, End, End),
      Node(3, End, End) )
    // Measuring performance of 3 manually created tree on sequential sum method
    val time1seq = measure {
      val test1 = simpleTree.sum()
//      println(test1)
    }
  val time2seq = measure {
    val test2 = tree.sum()
//    println(test2)
  }
  val time3seq = measure {
    val test3 = complexTree.sum()
//    println(test3)
  }

    println("Summation simpleTree executed sequentially in "+time1seq)
    println("Summation tree executed sequentially in "+time2seq)
    println("Summation complexTree executed sequentially in "+time3seq)
    println("**********")

    // measuring execution of tree sum in parallel
    val time1par = measure {
      val parTest1 = simpleTree.sumPar()
//      parTest1.onComplete { case Success(result) => println(result) }
    }

  val time2par = measure {
    val parTest2 = tree.sumPar()
//    parTest2.onComplete { case Success(value) => println(value) }
  }

  val time3par = measure {
    val parTest3 = complexTree.sumPar()
//    parTest3.onComplete { case Success(value) => println(value) }
  }

    println("Summation simpleTree executed in parallel in " + time1par)
    println("Summation tree executed in parallel in " + time2par)
    println("Summation complexTree executed in parallel in " + time3par)
    println("**********")

    //method to generate test tree with value of 1 but with greater depth
    def generateTree(depth: Int): Tree = {
      depth match  {
        case 0 =>Node(1, End, End)
        case _ =>Node(1, generateTree(depth-1), generateTree(depth-1))
      }
    }
    // generating deep test tree
    val genTree = generateTree(20)
    // measuring sequential sum of generated tree
    val time4seq = measure {
      val test4 = genTree.sum()
//        println(test4)
    }
    println("Summation (generated tree) executed sequentially in "+time4seq)
    println("**********")
    //measuring parallel sum of generated tree
    val time4par = measure {
      val parTest4 = genTree.sumPar()
//      parTest4.onComplete{ case Success(value) => println(value)}
    }
    println("Summation (generated tree) executed in parallel in "+time4par)
    println("**********")

}
