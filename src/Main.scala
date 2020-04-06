import scala.collection.mutable
import scala.math.pow

/*
電卓仕様
四則演算、%、^と()に対応している
与えられる数値の範囲は正の値のみ
必ず空白区切りにする
*/

object Main extends App {
//  val in = "(1 + (6 / 3)) * (7 - 3)"
//  val in = "(1 + 2) + (7 - 3)"
  val in = "3 + 6 * (6 + 5) - (7 * 2)"
//  val in = "602 % 3 * 3"
  val nodeParser = new NodeParser()
  val nodeSeq = nodeParser.parse(in)
  val compiler = new Compiler()
  val result = compiler.compile(nodeSeq)

  println(result)
}
/*
Seq[Node]を受け取り木構造に変換し、変換した木構造の計算結果を返す
 */
class Compiler() {
  private val stack: mutable.Stack[Node] = mutable.Stack()

  /*
  ノードのリストから木構造を作成する
  */
  def compile(nodes: Seq[Node], currentPriority: Int = 0): Int = nodes match {
    /*
    スタックに3ノード以上ある場合は、終了せずに再帰を続ける
     */
    case head :: tail if tail == Nil && stack.length > 3 =>
      val partialTree = createPartTree(head, stack.pop(), stack.pop())
      compile(Seq(partialTree), 50)
    /*
    最後のノードで木構造を完成させる
    */
    case head :: tail if tail == Nil =>
      val woodStructure = createPartTree(head, stack.pop(), stack.pop())
      woodStructure.result()
    /*
    数字のノードの場合は必ずstackさせる
     */
    case head :: tail if head.priority % 100 == 10 =>
      stack.push(head)
      compile(tail, currentPriority)
    /*
    現在の優先順位より高いものが来た場合はstackする
     */
    case head :: tail if currentPriority < head.priority =>
      stack.push(head)
      compile(tail, head.priority)
    /*
    初めに現在の優先順位より低いものが来た場合は、stack内の要素で部分機を作成する
    最後に現在のstackの中身に符号が存在しない場合、次の要素をstackするために現在の優先順位を下げる
     */
    case head :: tail if currentPriority >= head.priority =>
      val partialTree = createPartTree(stack.pop(), stack.pop(), stack.pop())
      stack.push(partialTree)
      val priority = if (stack.length == 1) 0 else currentPriority
      compile(head :: tail, priority)
  }

  /*
  ノードと符号を受け取ることで部分機を作成する
   */
  def createPartTree(left: Node, symbol: Node, right: Node): Node = {
    new Sign(symbol.value, Some(left), Some(right), symbol.priority)
  }
}

/*
priority
0  空白
10 数字
20 + -
25 * / % ^
30 (
40 )

文字列で受け取った式を一つずつNodeに変換していく
 */
class NodeParser() {
  private var currentPriority: Int = 0
  private var currentString: String = ""
  private var spaceFlg: Boolean = true
  private var nodeSeq: Seq[Node] = Seq()
  private var bracketsPriority: Int = 0
  private var bracketsEndCount: Int = 0

  /*
  入力された文字をNodeに変換をしていく
  基本は空白が来るたびに、プロパティの値を見てNodeに変換をする
   */
  def parse(in: String): Seq[Node] = {
    val isNum = """[0-9]""".r
    val isSign = """[\*/\+\-\^%]""".r
    val isBrackets = """[\(\)]""".r
    in.split("").foreach(s => s match {
      case isNum() => patternInt(s)
      case isSign() => patternSign(s)
      case isBrackets() => patternBrackets(s)
      case s if s == " " =>
        spaceFlg = true
        nodeSeq = nodeSeq :+ createNode(currentString)
        if (bracketsEndCount > 0) {
          bracketsPriority -= 100 * bracketsEndCount
          bracketsEndCount = 0
        }
        nodeSeq
      case s => throw new Exception(s)
    })

    nodeSeq :+ createNode(currentString)
  }

  /*
  数字の場合、数字は2桁以上の場合ああるので、それを考慮している
   */
  private def patternInt(s: String): Unit = s match {
    case s if currentPriority == 10 =>
      currentString += s
      spaceFlg = false
    case _ if !spaceFlg =>
      throw new Exception("空白の位置が正しくありません")
    case s if currentPriority != 40 =>
      currentString = s
      currentPriority = 10
      spaceFlg = false
    case _ =>
      throw new Exception("文法が正しくありません")
  }

  /*
  記号が入力された時の処理
   */
  private def patternSign(s: String): Unit = s match {
    case _ if !spaceFlg =>
      throw new Exception("sign 空白の位置が正しくありません")
    case s if currentPriority != 20 && currentPriority != 30  =>
      currentString = s
      currentPriority = 20
      spaceFlg = false
    case _ =>
      throw new Exception("文法が正しくありません")
  }

  /*
  カッコが入ってきた時の処理
  Nodeに変換する際にカッコは変換せずに、カッコ内の数字と記号のpriorityを上げる処理をする
  そのためのフラグ管理などを行っている
   */
  private def patternBrackets(s: String): Unit = s match {
    case s if s == "(" && (currentPriority == 20 || currentPriority == 0) =>
      currentPriority = 30
      bracketsPriority += 100
    case _ if spaceFlg =>
      throw new Exception("空白の位置が正しくありません")
    case s if s == ")" && (currentPriority == 10 || currentPriority == 40) =>
      bracketsEndCount += 1
    case _ =>
      throw new Exception("文法が正しくありません")
  }

  /*
  受け取った文字からNodeを作成している
   */
  private def createNode(s: String): Node = currentPriority match {
    case i if i == 10 => new Number(s, None, None, 10 + bracketsPriority)
    case _ if s == "+" => new Sign(s, None, None, 20 + bracketsPriority)
    case _ if s == "-" => new Sign(s, None, None, 20 + bracketsPriority)
    case _ if s == "/" => new Sign(s, None, None, 25 + bracketsPriority)
    case _ if s == "*" => new Sign(s, None, None, 25 + bracketsPriority)
    case _ if s == "%" => new Sign(s, None, None, 25 + bracketsPriority)
    case _ if s == "^" => new Sign(s, None, None, 25 + bracketsPriority)
    case i => throw new Exception(s"正しくない文字 ${i} ${s}")
  }

  /*
  デバック用
   */
  def print(): Unit = {
    nodeSeq.foreach(node => println(node.value))
  }
}

/*
数字と記号の抽象
leftとrightがNoneのNodeは葉ノード
leftとrightにNodeがある場合は内部ノード
 */
trait Node {
  val priority: Int
  val value: String
  val left: Option[Node]
  val right: Option[Node]

  def result(): Int
}

class Sign(val value: String, val left: Option[Node], val right: Option[Node], val priority: Int) extends Node {
  override def result(): Int = value match {
    case "+" => right.get.result() + left.get.result()
    case "-" => right.get.result() - left.get.result()
    case "/" => right.get.result() / left.get.result()
    case "*" => right.get.result() * left.get.result()
    case "%" => right.get.result() % left.get.result()
    case "^" => pow(right.get.result(), left.get.result()).toInt
    case _ => throw new Exception(s"存在しないパターン $value")
  }
}

class Number(val value: String, val left: Option[Node], val right: Option[Node], val priority: Int) extends Node {
  override def result(): Int = value.toInt
}
