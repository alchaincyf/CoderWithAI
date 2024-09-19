---
title: 深入理解JavaScript中的Future和Promise
date: 2023-10-05
description: 本课程详细讲解JavaScript中的Future和Promise，帮助你掌握异步编程的核心概念和实际应用。
slug: understanding-futures-and-promises-in-javascript
tags:
  - JavaScript
  - 异步编程
  - Future
  - Promise
category: 编程教程
keywords:
  - JavaScript Future
  - JavaScript Promise
  - 异步编程
  - 前端开发
---

# Future 和 Promise 教程

## 概述

在现代编程中，异步编程是一个非常重要的概念。Scala 提供了 `Future` 和 `Promise` 来帮助我们处理异步操作。`Future` 表示一个可能在未来完成的计算结果，而 `Promise` 则是一个可以控制 `Future` 结果的容器。

## 1. Future 简介

### 1.1 什么是 Future？

`Future` 是一个表示异步计算结果的容器。它代表一个可能在未来某个时间点完成的计算。`Future` 可以处于以下几种状态：

- **Pending**: 计算尚未完成。
- **Completed**: 计算已经完成，结果可能是成功或失败。

### 1.2 Future 的基本用法

在 Scala 中，`Future` 通常通过 `Future` 伴生对象的 `apply` 方法创建。以下是一个简单的例子：

```scala
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object FutureExample extends App {
  val futureResult: Future[Int] = Future {
    Thread.sleep(1000) // 模拟耗时操作
    42
  }

  val result: Int = Await.result(futureResult, 2.seconds)
  println(s"Future result: $result")
}
```

在这个例子中，我们创建了一个 `Future`，它会在 1 秒后返回结果 `42`。我们使用 `Await.result` 来等待 `Future` 完成并获取结果。

### 1.3 Future 的回调函数

除了使用 `Await`，我们还可以使用回调函数来处理 `Future` 的结果。常用的回调函数包括 `onComplete`、`onSuccess` 和 `onFailure`。

```scala
futureResult.onComplete {
  case scala.util.Success(value) => println(s"Success: $value")
  case scala.util.Failure(ex) => println(s"Failure: ${ex.getMessage}")
}
```

## 2. Promise 简介

### 2.1 什么是 Promise？

`Promise` 是一个可以控制 `Future` 结果的容器。通过 `Promise`，我们可以手动设置 `Future` 的结果，无论是成功还是失败。

### 2.2 Promise 的基本用法

以下是一个使用 `Promise` 的简单例子：

```scala
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

object PromiseExample extends App {
  val promise: Promise[Int] = Promise[Int]()
  val future: Future[Int] = promise.future

  future.onComplete {
    case scala.util.Success(value) => println(s"Success: $value")
    case scala.util.Failure(ex) => println(s"Failure: ${ex.getMessage}")
  }

  // 模拟异步操作
  Future {
    Thread.sleep(1000)
    promise.success(42)
  }
}
```

在这个例子中，我们创建了一个 `Promise`，并通过 `promise.success` 手动设置 `Future` 的结果为 `42`。

## 3. Future 和 Promise 的组合

### 3.1 组合多个 Future

在实际应用中，我们经常需要组合多个 `Future`。Scala 提供了多种方法来组合 `Future`，例如 `map`、`flatMap`、`zip` 等。

```scala
val future1: Future[Int] = Future { 1 }
val future2: Future[Int] = Future { 2 }

val combinedFuture: Future[(Int, Int)] = future1.zip(future2)

combinedFuture.onComplete {
  case scala.util.Success((value1, value2)) => println(s"Combined result: ($value1, $value2)")
  case scala.util.Failure(ex) => println(s"Failure: ${ex.getMessage}")
}
```

### 3.2 使用 Promise 控制多个 Future

我们也可以使用 `Promise` 来控制多个 `Future` 的结果。例如，我们可以创建一个 `Promise`，并在多个 `Future` 完成后设置其结果。

```scala
val promise: Promise[Int] = Promise[Int]()

val future1: Future[Int] = Future { 1 }
val future2: Future[Int] = Future { 2 }

Future.sequence(List(future1, future2)).onComplete {
  case scala.util.Success(values) => promise.success(values.sum)
  case scala.util.Failure(ex) => promise.failure(ex)
}

promise.future.onComplete {
  case scala.util.Success(value) => println(s"Sum of results: $value")
  case scala.util.Failure(ex) => println(s"Failure: ${ex.getMessage}")
}
```

## 4. 实践练习

### 4.1 练习 1: 使用 Future 计算斐波那契数列

编写一个程序，使用 `Future` 计算斐波那契数列的前 10 个数字，并打印结果。

```scala
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object FibonacciFuture extends App {
  def fibonacci(n: Int): Future[Int] = Future {
    if (n <= 1) n
    else fibonacci(n - 1).flatMap(a => fibonacci(n - 2).map(b => a + b))
  }

  val fibonacciNumbers: List[Future[Int]] = (0 to 9).map(fibonacci).toList

  Future.sequence(fibonacciNumbers).onComplete {
    case scala.util.Success(values) => println(s"Fibonacci numbers: $values")
    case scala.util.Failure(ex) => println(s"Failure: ${ex.getMessage}")
  }
}
```

### 4.2 练习 2: 使用 Promise 实现超时机制

编写一个程序，使用 `Promise` 实现一个超时机制。如果 `Future` 在指定时间内没有完成，则返回一个失败的结果。

```scala
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TimeoutPromise extends App {
  def timeout[T](future: Future[T], duration: FiniteDuration): Future[T] = {
    val promise: Promise[T] = Promise[T]()

    future.onComplete(promise.tryComplete)

    Future {
      Thread.sleep(duration.toMillis)
      promise.tryFailure(new Exception("Timeout"))
    }

    promise.future
  }

  val future: Future[Int] = Future {
    Thread.sleep(2000)
    42
  }

  timeout(future, 1.second).onComplete {
    case scala.util.Success(value) => println(s"Success: $value")
    case scala.util.Failure(ex) => println(s"Failure: ${ex.getMessage}")
  }
}
```

## 5. 总结

`Future` 和 `Promise` 是 Scala 中处理异步编程的重要工具。通过 `Future`，我们可以表示一个可能在未来完成的计算结果；通过 `Promise`，我们可以手动控制 `Future` 的结果。掌握这两个概念对于编写高效、可维护的异步代码至关重要。

希望这篇教程能帮助你更好地理解 `Future` 和 `Promise`，并在实际编程中灵活运用它们。