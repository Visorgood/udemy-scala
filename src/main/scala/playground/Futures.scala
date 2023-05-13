package playground

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Random, Success, Try}

object Futures extends App {

  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] =
    first.flatMap(_ => second)

  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]
    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)
    promise.future
  }

  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]
    val checkAndComplete = (result: Try[A]) =>
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)
    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)
    lastPromise.future
  }

  val fast = Future {
    Thread.sleep(50)
    50
  }
  val slow = Future {
    Thread.sleep(60)
    60
  }
  first(fast, slow).foreach(println)
  last(fast, slow).foreach(println)

  Thread.sleep(1000)

  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] = {
    action().filter(condition).recoverWith {
      case _ => retryUntil(action, condition)
    }
  }

  println("Starting retryUntil...")
  val r = new Random()
  val action = () => Future {
    Thread.sleep(300)
    val nextValue = r.nextInt(500)
    println("generated " + nextValue)
    nextValue
  }

  retryUntil(action, (x: Int) => x < 50).foreach(r => println("settled at " + r))
  Thread.sleep(10000)
}
