
import scala.math._

object Main extends App {
  // 商品を作っている
  val product1 = Product("product1", 100)
  val product2 = Product("product2", 200)
  val product3 = Product("product3", 300)
  val product4 = Product("product4", 400)
  val product5 = Product("product5", 500)

  // 商品ラックを作っている
  val rack1 = Rack("rack1", Seq(product1, product2, product3))
  val rack2 = Rack("rack1", Seq(product1, product2, product3))
  val rack3 = Rack("rack1", Seq(product1, product2, product3))
  val rack4 = Rack("rack1", Seq(product1, product2, product3))
  val rack5 = Rack("rack2", Seq(product4, product5, product1, rack1))
  val rack6 = Rack("rack3", Seq(product1, product2, product4, rack2, rack3))

  // 倉庫を作っている
  val warehouse = Warehouse("warehouse1", Seq(rack4, rack5, rack6))
  println("この倉庫のproduct1の数は", warehouse.search("product1"))
}

// 葉と節のインターフェース
trait Component {
  val name: String
  // 再帰処理で使用する葉と節の関数
  def search(productName: String): Int
}

// 商品
case class Product(name: String, price: Int) extends Component {
  // 商品の場合は同じ商品名であれば１を返す
  override def search(productName: String): Int = if(name == productName) 1 else 0
}

// 商品ラック
case class Rack(name: String, componentList: Seq[Component]) extends Component {
    // 商品ラックの場合は自身の要素に対してsearch関数を実行する
  override def search(productName: String): Int = componentList.foldLeft(0)((total, component) => {
    total + component.search(productName)
  })
}

// 倉庫
case class Warehouse(name: String, componentList: Seq[Component]) extends Component {
  // 倉庫の場合も自身の要素に対してsearch関数を実行する
  override def search(productName: String): Int = componentList.foldLeft(0)((total, component) => {
    total + component.search(productName)
  })
}
