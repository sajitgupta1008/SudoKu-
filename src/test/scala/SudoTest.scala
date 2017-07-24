/**
  * Created by knoldus on 24/7/17.
  */
class SudoTest extends org.scalatest.FunSuite {

  val sudo = new Sudo()

  test("testing solveSudoKu"){
    assert(sudo.solveSudoKu(sudo.initialize(), 0))
  }

}
