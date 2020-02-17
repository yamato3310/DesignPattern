
import scala.math._

object Main extends App {
  execution(new ItalianRestaurantFactory)

  def execution(restaurantFactory: RestaurantFactory): Unit = {
    val restaurant = restaurantFactory.create(10)
    restaurant.printMenu()
  }
}

// レストランの抽象クラス。今回はメニューを表示するだけ
trait Restaurant {
  val menu: Seq[Menu]
  def printMenu(): Unit = println(menu)
}

// イタリアンレストラン
class ItalianRestaurant(val menu: Seq[Menu]) extends Restaurant

// 洋食レストラン
class WesternFoodRestaurant(val menu: Seq[Menu]) extends Restaurant

// レストランのメニュー
case class Menu(name: String, price: Int)

// レストランのFactoryクラス
trait RestaurantFactory {
  // この関数を呼ぶことで、レストランを作成することができる
  def create(menuNum: Int): Restaurant
}

// イタリアンレストランの抽象クラス
class ItalianRestaurantFactory() extends RestaurantFactory {
  // イタリアンレストランを作成するための初期処理を行っている
  override def create(menuNum: Int): ItalianRestaurant = {
    val menuSuffix = Seq("Pasta", "Pizza", "Soup")
    val menu = (0 to menuNum).map(i => {
      Menu(s"Italian${menuSuffix(i % 3)}$i", (random() * 2000).toInt)
    })
    new ItalianRestaurant(menu)
  }
}

// 洋食レストランの抽象クラス
class WesternFoodRestaurantFactory() extends RestaurantFactory {
  // 洋食レストランを作成するための初期処理を行っている
  override def create(menuNum: Int): WesternFoodRestaurant = {
    val menuSuffix = Seq("Steak", "Croquette", "Hamburger")
    val menu = (0 to menuNum).map(i => {
      Menu(s"WesternFood${menuSuffix(i % 3)}$i", (random() * 2000).toInt)
    })
    new WesternFoodRestaurant(menu)
  }
}
