/**
  * Created by knoldus on 23/7/17.
  */


class Sudo {

  //0 indicates blank position

  def initializeWithUserInput():Map[(Int, Int), Int] = {

    (0 to 8).toList.flatMap(x => (0 to 8).toList.map(y => (x,y)->scala.io.StdIn.readInt())).toMap

  }

  def initialize(): Map[(Int, Int), Int] = {

    val list = List(0,0,4,8,0,0,0,1,7,6,7,0,9,0,0,0,0,0,5,0,8,0,3,0,0,0,4,3,0,0,7,4,0,1,0,0,0,6,9,0,0,0,7,8,0,0,0,1,
      0,6,9,0,0,5,1,0,0,0,8,0,3,0,6,0,0,0,0,0,6,0,9,1,2,4,0,0,0,1,5,0,0)

    (0 to 8).toList.flatMap(x => (0 to 8).toList.map(y => (x,y)->list(x*9 + y))).toMap    //(x,y)->value

  }


  def solveSudoKu(map : Map[(Int, Int), Int], index: Int): Boolean   = {

    if(index>80)
    {  display(map);    true}

   else if (map(index / 9, index % 9) == 0) {
      for (i <- 1 to 9)
        if (isSafe(map, index / 9, index % 9, i)) {
          val newmap = map.updated((index / 9, index % 9), i)

       //   println((index / 9, index % 9), i)
         if (solveSudoKu(newmap, index + 1))
            return true

        }
      false
    }
    else
      solveSudoKu(map, index+1)

  }


private  def isSafe(map : Map[(Int, Int), Int], row:Int, col:Int, value:Int) = {

    isRowSafe(map, row, value) && isColSafe(map, col, value) && isQuadSafe(map, row-row%3, col-col%3, value)
  }

 private def isRowSafe(map : Map[(Int, Int), Int], row:Int, value:Int ) : Boolean={

    !(0 to 8).toList.exists(x => map(row,x)==value)
  }

private  def isColSafe(map : Map[(Int, Int), Int], col:Int, value:Int ):Boolean={
    !(0 to 8).toList.exists(x => map(x,col)==value)
  }

 private def isQuadSafe(map : Map[(Int, Int), Int], row:Int,col:Int, value:Int ):Boolean={

   /* for(i<- 0 to 2; j<-0 to 2 if map(row+i,col+j) == value)
      return  false

    true*/
    !(0 to 2).toList.exists(x => (0 to 2).toList.exists(y=> map(row+x, col+y)==value))
  }

  //def getSolvedSudoku:Map[(Int, Int), Int] = m

  def display(m: Map[(Int, Int), Int]):Unit = {

    (0 to 8).foreach(x => {(0 to 8).foreach(y=>print(m(x,y)+" ")); println()})
  }

  def compute():Unit = {

    if(solveSudoKu(initialize(), 0))
      println("sudoku solved")
    else
      println("failed to solve Sudoku")


    }



}

