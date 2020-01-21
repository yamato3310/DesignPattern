/*
今回のサンプルコードは、レジシステムです
ShoppingBasketでbuilderパターンを使用しています
このクラスに購入する商品を入れていき、receiptBuild関数を使用することでレシートを作っています

レジに商品を入れることで、会計をしてレシートを出力する

*/

import scala.math._

object Main extends App {
  val products = new ProductRepository().create(10)
  val user = User("nabeshi", 10000, products)
  val register = new Register(new ReceiptBuilder())
  val receipt = register.addProducts(user.shoppingBasket).takeProduct(user.shoppingBasket(1)).accounting(user.money)
  receipt.print()
}

class Register(builder: ReceiptBuilder) {
  def addProducts(products: Seq[Product]): Register= {
    new Register(products.foldLeft(builder)((builder, product) => builder.addProduct(product)))
  }
  def takeProduct(product: Product): Register = {
    new Register(builder.takeProduct(product))
  }
  def accounting(money: Int): Receipt = builder.build(money)
}

class ProductRepository() {
  def create(num: Int): List[Product] = for {
    i <- (1 to num).toList
  } yield {
    Product(s"product$i", floor(random() * 1000).toInt)
  }
}

case class Product(name: String, price: Int)

case class Receipt(products: Seq[Product], payment: Int) {
  val totalFee = products.map(_.price).sum
  def print(): Unit = {
    products.foreach(product => println(s"${product.name}: ${product.price} 円"))
    println(s"合計: $totalFee 円")
    println(s"お預かり: $payment 円")
    println(s"お釣り: ${payment - totalFee} 円")
  }
}

class ReceiptBuilder(private val productList: Seq[Product] = Seq()) {
  // レジを通した商品を追加していく
  def addProduct(product: Product): ReceiptBuilder = new ReceiptBuilder(productList :+ product)
  // キャンセルした商品を取り出す
  def takeProduct(product: Product): ReceiptBuilder = new ReceiptBuilder(productList.diff(Seq(product)))
  // レシートを生成する
  def build(money: Int): Receipt = Receipt(productList, money)
}

case class User(name: String, money: Int, shoppingBasket: Seq[Product])