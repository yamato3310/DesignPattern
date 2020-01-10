/*
今回の目的は、予算範囲内で商品を買うか判断する戦略と特定の種類の商品だけを買うか判断する戦略
商品には3種類("beauty", "food", "toy")あり、購入者はそれぞれの種類に対して一つの商品で予算の何パーセントのまでの金額を出せるかを決めている
商品を購入した際は購入した商品と残りの残高を表示する
- 予算範囲内で商品を買うか判断する戦略: 予算内で上記の条件に従って商品を購入する。予算が足りないときは購入しない
- 特定の種類の商品だけを買うか判断する戦略: 指定した種類の商品だけを購入するが、予算が足りない場合は万引きする
*/

import scala.math._

object Main extends App {
  val user = new User(10000, 10000, 0.2, 0.5, 0.2)
  val products = new ProductRepository().create(20)
  val shop = Shop(products)
  val strategy = new UnlimitedGenreStrategy()
  user.shopping(shop, strategy)
}

class ProductRepository() {
  private val genres = List("beauty", "food", "toy")

  def create(num: Int): List[Product] = for {
    i <- (1 to num).toList
  } yield {
    val genre = genres(floor(random() * genres.length).toInt)
    Product(i, s"$genre$i", floor(random() * 5000).toInt, genre)
  }
}

case class Product(id: Int, name: String, price: Int, genre: String)

class User(private var wallet: Int, val budget: Int, val maxBeautyPercentage: Double, val maxFoodPercentage: Double, val maxToyPercentage: Double) {
  private def buy(product: Product): Unit = {
    setWallet(product.price)
    println(s"${product.price}円の${product.name} を購入した！　残り$wallet 円")
  }

  private def notBuy(product: Product): Unit = println(s"${product.price}円の${product.name}を購入しませんでした")

  private def setWallet(price: Int): Unit = wallet -= price

  def getWallet(): Int = wallet

  def shopping(shop: Shop, strategy: ShoppingStrategy): Unit =
    shop.products.foreach(product => if (strategy.judgeBuy(this, product)) buy(product) else notBuy(product))
}

case class Shop(products: List[Product])

trait ShoppingStrategy {
  def judgeBuy(user: User, product: Product): Boolean
}

class UnlimitedGenreStrategy() extends ShoppingStrategy {
  override def judgeBuy(user: User, product: Product): Boolean = {
    val budgetPercentage = product.price.toDouble / user.budget
    println(budgetPercentage)
    product.genre match {
      case _ if user.getWallet() < product.price => false
      case "beauty" => budgetPercentage <= user.maxBeautyPercentage
      case "food" => budgetPercentage <= user.maxFoodPercentage
      case "toy" => budgetPercentage <= user.maxToyPercentage
    }
  }
}
