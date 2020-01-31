/*
今回のサンプルコードは、レジシステムです
ShoppingBasketでbuilderパターンを使用しています
このクラスに購入する商品を入れていき、receiptBuild関数を使用することでレシートを作っています

購入する商品をレジに登録し、会計をしてレシートを発行する。

*/

import scala.math._

object Main extends App {
  val products = new ProductRepository().create(10)
  val user = User("nabeshi", 10000, products)
  val register = new Register(new ReceiptBuilder())
  val receipt = register
    .addProducts(user.shoppingBasket)
    .cancelProduct(user.shoppingBasket(1))
    .calculate(user.money)
  receipt.print()
}

// ReceiptBuilderを操作するクラス。ユーザーが商品を買う時はこのクラスを使用する
class Register(builder: ReceiptBuilder) {
  // 商品をbuilderに追加する
  def addProducts(products: Seq[Product]): Register= {
    new Register(products.foldLeft(builder)((builder, product) => builder.addProduct(product)))
  }
  // 商品をキャンセルする
  def cancelProduct(product: Product): Register = {
    new Register(builder.cancelProduct(product))
  }
  // 計算をしてレシートを出力する
  def calculate(money: Int): Receipt = builder.build(money)
}

//　商品を取り出すためのクラス
class ProductRepository() {
  // 商品を取り出す
  def create(num: Int): List[Product] = (1 to num).toList.map(i => Product(s"product$i", floor(random() * 1000).toInt))
}

// 商品
case class Product(name: String, price: Int)

// 購入した商品をまとめるクラス
case class Receipt(products: Seq[Product], payment: Int, price: Int) {
  // レシートを表示する
  def print(): Unit = {
    products.foreach(product => println(s"${product.name}: ${product.price} 円"))
    println(s"合計: $price 円")
    println(s"お預かり: $payment 円")
    println(s"お釣り: ${payment - price} 円")
  }
}

// builderクラス。商品を追加していきbuild関数が呼び出されたタイミングで、レシートを生成している
class ReceiptBuilder(private val productList: Seq[Product] = Seq()) {
  // レジを通した商品を追加していく
  def addProduct(product: Product): ReceiptBuilder = new ReceiptBuilder(productList :+ product)
  // 商品をキャンセルする
  def cancelProduct(product: Product): ReceiptBuilder = new ReceiptBuilder(productList.diff(Seq(product)))
  // レシートを生成する
  def build(money: Int): Receipt = Receipt(productList, money, productList.map(_.price).sum)
}

// 商品を買うユーザー
case class User(name: String, money: Int, shoppingBasket: Seq[Product])