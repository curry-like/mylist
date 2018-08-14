import scala.annotation.tailrec

trait MyListTrait[+A] {

  def head: A

  def tail: MyListTrait[A]

  def ::[B >: A](b: B): MyListTrait[B] = MyList(b, this)

  def :::[B >: A](myListTrait: MyListTrait[B]): MyListTrait[B]

  def +:[B >: A](b: B): MyListTrait[B] = this ::: MyList(b, MyNil)

  def map[B](f: A => B): MyListTrait[B]

  def flatMap[B](f: A => MyListTrait[B]): MyListTrait[B]

  def reverse: MyListTrait[A]

  def foldLeft[B](acc: B)(f: (B, A) => B): B

  def foldRight[B](acc: B)(f: (A, B) => B): B = {
    @tailrec
    def loop(list: MyListTrait[A], function: B => B): B => B = {
      list match {
        case MyNil => function
        case _ => loop(list.tail, function.compose(a => f(list.head, a)))
      }
    }
    loop(this, a => a)(acc)
  }

  def filter(f: A => Boolean): MyListTrait[A]

  def zip[B](myListTrait: MyListTrait[B]): MyListTrait[(A, B)]

  def foreach[U](f: A => U): Unit
}

case class MyList[A](
                      head: A,
                      tail: MyListTrait[A]
                    ) extends MyListTrait[A] {

  override def :::[B >: A](myListTrait: MyListTrait[B]): MyList[B] = {
    myListTrait match {
      case MyNil => this.map(_.asInstanceOf[B])
      case _ => MyList(myListTrait.head, this.:::(myListTrait.tail))
    }
  }

  override def map[B](f: A => B): MyList[B] = MyList(f(this.head), this.tail.map(f))

  override def flatMap[B](f: A => MyListTrait[B]): MyListTrait[B] = f(this.head) ::: this.tail.flatMap(f)

  override def reverse: MyList[A] = {
    @tailrec
    def r(list: MyListTrait[A], tailList: MyListTrait[A]): MyList[A] = {
      list.tail match {
        case MyNil => MyList(list.head, tailList)
        case _ => r(list.tail, MyList(list.head, tailList))
      }
    }
    r(this, MyNil)
  }

  override def foldLeft[B](acc: B)(f: (B, A) => B): B = this.tail.foldLeft[B](f(acc, this.head))(f)

  override def filter(f: A => Boolean): MyListTrait[A] = {
    if (f(this.head)) {
      MyList(this.head, this.tail.filter(f))
    } else {
      this.tail.filter(f)
    }
  }

  override def zip[B](myListTrait: MyListTrait[B]): MyListTrait[(A, B)] = {
    myListTrait match {
      case MyNil => MyNil
      case _ => MyList((this.head, myListTrait.asInstanceOf[MyList[B]].head), this.tail.zip(myListTrait.asInstanceOf[MyList[B]].tail))
    }
  }

  override def foreach[U](f: A => U): Unit = {
    f(this.head)
    this.tail.foreach(f)
  }
}

case object MyNil extends MyListTrait[Nothing] {

  override def head: Nothing = throw new Exception

  override def tail: MyListTrait[Nothing] = throw new Exception

  override def :::[B >: Nothing](myListTrait: MyListTrait[B]): MyListTrait[B] = myListTrait

  override def map[B](f: Nothing => B): MyListTrait[B] = MyNil

  override def flatMap[B](f: Nothing => MyListTrait[B]): MyListTrait[B] = MyNil

  override def reverse: MyListTrait[Nothing] = MyNil

  override def foldLeft[B](acc: B)(f: (B, Nothing) => B): B = acc

  override def filter(f: Nothing => Boolean): MyListTrait[Nothing] = MyNil

  override def zip[B](myListTrait: MyListTrait[B]): MyListTrait[(Nothing, B)] = MyNil

  override def foreach[U](f: Nothing => U): Unit = {}
}
