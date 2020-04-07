import scala.collection.mutable
import scala.math.pow

/*
電卓仕様
四則演算、%、^と()に対応している
与えられる数値の範囲は正の値のみ
必ず空白区切りにする
*/

object Main extends App {
  val in = "( 1 + ( 6 / 3 )) * ( 7 - 3 )"
//  val in = "( 1 * 2 ) * ( 3 * 4 )"
//  val in = "3 + 6 * ( 6 + 5 ) * ( 7 * 2 )"
//  val in = "602 % 3 * 3"
//  val in = "1 + 4"
  val compiler = new Compiler(in)
  val result = compiler.compile()

  println(result)
}
/*
計算式を受け取り木構造に変換し、変換した木構造から計算結果を出す
 */
class Compiler(in: String) {
  private val nodeParser = new NodeParser
  val partsList: List[Parts] = nodeParser.parse(in)
  private val stack: mutable.Stack[Node] = mutable.Stack()
  private val tempStack: mutable.Stack[IterableOnce[Node]] = mutable.Stack()
  private val priorityQueue: mutable.Queue[Int] = mutable.Queue()

  /*
  PartsをNodeに変換し木構造を作成する
  Partsはカッコを含むため、木構造ではカッコをなくすため
  　*/
  def compile(parts: List[Parts] = partsList, currentPriority: Int = 0): Int = parts match {
    /*
    スタックに3ノード以上ある場合は、終了せずに再帰を続ける
    　*/
    case ::(head: Node, Nil) if stack.length > 3 =>
      val partialTree = createPartTree(head, stack.pop(), stack.pop())
      compile(List(partialTree), 50)
    /*
    木構造が完成したら、計算結果を返す
    　*/
    case ::(head: Node, Nil) =>
      val woodStructure = createPartTree(head, stack.pop(), stack.pop())
      woodStructure.result()
    /*
    ) が来た場合は、stack内のノードを計算しNumberにした後に、退避させていたNodeを戻す
     */
    case ::(head: Brackets, tail) if head.value == ")"  =>
      val partialTree = createPartTree(stack.pop(), stack.pop(), stack.pop())
      stack.pushAll(tempStack.pop())
      val currentPriority = priorityQueue.dequeue()
      compile(new Number(partialTree.result().toString) +: tail, currentPriority)
    /*
    ( が来た場合は、stackをすべて退避させる
     */
    case ::(head: Brackets, tail) if head.value == "(" =>
      tempStack.push(stack.popAll())
      priorityQueue += currentPriority
      compile(tail, 0)
    /*
    数字のPartsの場合は必ずstackさせる
     */
    case ::(head: Number, tail) =>
      stack.push(head)
      compile(tail, currentPriority)
    /*
    現在の優先順位より高いものが来た場合はstackする
     */
    case ::(head: Sign, tail) if currentPriority < head.priority =>
      stack.push(head)
      compile(tail, head.priority)
    /*
    初めに現在の優先順位より低いものが来た場合は、stack内の要素で部分木を作成する
    最後に現在のstackの中身にNodeが一つしか存在しない場合、次の要素をstackするために現在の優先順位を下げる
     */
    case ::(head: Sign, tail) if currentPriority >= head.priority =>
      val partialTree = createPartTree(stack.pop(), stack.pop(), stack.pop())
      stack.push(partialTree)
      val priority = if (stack.length == 1) 0 else currentPriority
      compile(head :: tail, priority)
    case node => throw new Exception(s"no match $node")
  }

  /*
  ノードと符号を受け取ることで部分木を作成する
   */
  def createPartTree(left: Node, symbol: Node, right: Node): Node = {
    new Sign(symbol.value, Some(left), Some(right))
  }

  /*
 デバック用
  */
  def print(): Unit = {
    println("====")
    partsList.foreach(node => println(node.value))
    println("====")
  }
}

/*　
文字列で受け取った式を一つずつNodeに変換していく
 */
class NodeParser() {
  private var numberDigit: String = ""
  private var spaceFlg: Boolean = true
  private var partsSeq: List[Parts] = List()
  private var bracketsCount: Int = 0


  /*
  入力された文字をPartsに変換をしていく
   */
  def parse(in: String): List[Parts] = {
    val isNum = """[0-9]""".r
    val isSign = """[\*/\+\-\^%]""".r
    val isBrackets = """[\(\)]""".r
    in.split("").foreach(s => s match {
      case isNum() => patternInt(s)
      case isSign() => partsSeq = partsSeq :+ patternSign(s)
      case isBrackets() => partsSeq = partsSeq :+ patternBrackets(s)
      case s if s == " " =>
        spaceFlg = true
        if (numberDigit != "") {
          partsSeq = partsSeq :+ createNode(numberDigit)
          numberDigit = ""
        }
      case _ =>
    })

    if (numberDigit != "") {
      partsSeq = partsSeq :+ createNode(numberDigit)
      numberDigit = ""
    }
    partsSeq
  }

  /*
  数字の場合、数字は2桁以上の場合ああるので、それを考慮している
   */
  private def patternInt(s: String): Unit = partsSeq.lastOption match {
    case Some(_: Sign) if !spaceFlg =>
      throw new Exception("空白の位置が正しくありません")
    case Some(_: Number) =>
      throw new Exception("文法が正しくありません")
    case _ =>
      spaceFlg = false
      numberDigit += s
  }

  /*
  記号が入力された時の処理
   */
  private def patternSign(s: String): Parts = partsSeq.lastOption match {
    case _ if !spaceFlg =>
      throw new Exception("空白の位置が正しくありません")
    case Some(_: Sign)  =>
      throw new Exception("文法が正しくありません")
    case None =>
      throw new Exception("文法が正しくありません")
    case _ =>
      spaceFlg = false
      createNode(s)

  }

  /*
  カッコが入ってきた時の処理
   */
  private def patternBrackets(s: String): Parts = partsSeq.lastOption match {
    case _ if !spaceFlg =>
      throw new Exception("空白の位置が正しくありません")
    case Some(_: Number) if (s == ")" && bracketsCount > 0) =>
      bracketsCount -= 1
      createNode(s)
    case Some(_: Brackets) if (s == ")" && bracketsCount > 0) =>
      bracketsCount += 1
      createNode(s)
    case Some(_: Sign) if s == "(" =>
      bracketsCount += 1
      createNode(s)
    case None =>
      bracketsCount += 1
      createNode(s)
    case _ =>
      throw new Exception("文法が正しくありません")
  }

  /*
  受け取った文字からNodeを作成している
   */
  private def createNode(s: String): Parts = s match {
    case "(" => new Brackets(s)
    case ")" => new Brackets(s)
    case "+" => new Sign(s, None, None)
    case "-" => new Sign(s, None, None)
    case "/" => new Sign(s, None, None)
    case "*" => new Sign(s, None, None)
    case "%" => new Sign(s, None, None)
    case "^" => new Sign(s, None, None)
    case i => new Number(i)
  }

  /*
  デバック用
   */
  def print(): Unit = {
    println("====")
    partsSeq.foreach(node => println(node.value))
    println("====")
  }
}


/*
数字と記号の抽象
 */
trait Node extends Parts {
  def result(): Int
}

trait Parts {
  val value: String
}

class Sign(val value: String, val left: Option[Node], val right: Option[Node]) extends Node {
  val priority: Int = value match {
    case "+" => 1
    case "-" => 1
    case "/" => 2
    case "*" => 2
    case "%" => 2
    case "^" => 3
    case _ => throw new Exception(s"存在しない文字 $value")
  }

  override def result(): Int = value match {
    case "+" => right.fold(0)(_.result()) + left.fold(0)(_.result())
    case "-" => right.fold(0)(_.result()) - left.fold(0)(_.result())
    case "/" => right.fold(0)(_.result()) / left.fold(0)(_.result())
    case "*" => right.fold(0)(_.result()) * left.fold(0)(_.result())
    case "%" => right.fold(0)(_.result()) % left.fold(0)(_.result())
    case "^" => pow(right.fold(0)(_.result()), left.fold(0)(_.result())).toInt
    case _ => throw new Exception(s"存在しないパターン $value")
  }
}

class Number(val value: String) extends Node {
  override def result(): Int = value.toInt
}

class Brackets(val value: String) extends Parts
