---
title: Scala.js 入门教程：编译到 JavaScript
date: 2023-10-05
description: 本课程将带你深入了解如何使用Scala.js将Scala代码编译为JavaScript，从而在前端开发中利用Scala的强大功能。
slug: scala-js-tutorial
tags:
  - Scala
  - JavaScript
  - 前端开发
category: 编程语言
keywords:
  - Scala.js
  - Scala编译到JavaScript
  - 前端开发
---

# Scala.js (编译到 JavaScript)

## 概述

Scala.js 是一个将 Scala 代码编译成 JavaScript 的工具。它允许开发者使用 Scala 语言编写前端代码，并将其运行在浏览器中。Scala.js 结合了 Scala 的强大功能和 JavaScript 的广泛应用，为开发者提供了一种高效且现代的开发方式。

## 环境搭建

### 安装 JDK

首先，确保你已经安装了 JDK (Java Development Kit)。Scala 是基于 JVM 的语言，因此需要 JDK 来运行。

### 安装 Scala

接下来，安装 Scala。你可以通过以下命令在终端中安装 Scala：

```bash
brew install scala
```

### 安装 SBT

SBT (Scala Build Tool) 是 Scala 的构建工具，类似于 Maven 或 Gradle。你可以通过以下命令安装 SBT：

```bash
brew install sbt
```

### 安装 IDE

推荐使用 IntelliJ IDEA 作为 Scala 的集成开发环境。你可以从 JetBrains 官网下载并安装 IntelliJ IDEA，并安装 Scala 插件。

## 创建第一个 Scala.js 项目

### 初始化项目

在终端中，创建一个新的目录并初始化 SBT 项目：

```bash
mkdir my-scalajs-project
cd my-scalajs-project
sbt new scala/scala-seed.g8
```

### 添加 Scala.js 插件

在 `project/plugins.sbt` 文件中添加 Scala.js 插件：

```scala
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.5.1")
```

### 配置项目

在 `build.sbt` 文件中配置 Scala.js：

```scala
enablePlugins(ScalaJSPlugin)

name := "MyScalaJSProject"
scalaVersion := "2.13.6"

// 添加 DOM 库
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
```

### 编写第一个 Scala.js 程序

在 `src/main/scala` 目录下创建一个新的 Scala 文件 `Main.scala`：

```scala
import org.scalajs.dom
import org.scalajs.dom.document

object Main {
  def main(args: Array[String]): Unit = {
    val paragraph = document.createElement("p")
    paragraph.textContent = "Hello, Scala.js!"
    document.body.appendChild(paragraph)
  }
}
```

### 编译和运行

在终端中运行以下命令来编译和运行项目：

```bash
sbt fastOptJS
```

编译完成后，在 `target/scala-2.13` 目录下会生成一个 JavaScript 文件。你可以将这个文件引入到 HTML 页面中并运行。

## 变量和数据类型

Scala 支持多种数据类型，包括 `Int`、`Double`、`String`、`Boolean` 等。Scala 是静态类型语言，变量在声明时需要指定类型。

```scala
val name: String = "Scala.js"
val version: Double = 1.5
val isAwesome: Boolean = true
```

## 控制结构

### if/else

Scala 的 `if/else` 语句与 Java 类似：

```scala
val x = 10
if (x > 5) {
  println("x is greater than 5")
} else {
  println("x is less than or equal to 5")
}
```

### 循环

Scala 支持多种循环结构，包括 `for` 循环和 `while` 循环：

```scala
for (i <- 1 to 5) {
  println(s"Iteration $i")
}

var i = 0
while (i < 5) {
  println(s"While iteration $i")
  i += 1
}
```

## 函数式编程基础

Scala 是一门函数式编程语言，支持高阶函数、匿名函数等特性。

### 定义函数

```scala
def add(a: Int, b: Int): Int = {
  a + b
}
```

### 匿名函数

```scala
val add = (a: Int, b: Int) => a + b
```

### 高阶函数

```scala
def applyFunction(f: Int => Int, x: Int): Int = {
  f(x)
}

val double = (x: Int) => x * 2
println(applyFunction(double, 5)) // 输出 10
```

## 面向对象编程

Scala 支持面向对象编程，可以定义类和对象。

### 定义类

```scala
class Person(name: String, age: Int) {
  def greet(): String = s"Hello, my name is $name and I am $age years old."
}

val person = new Person("Alice", 30)
println(person.greet())
```

### 单例对象

```scala
object Singleton {
  def greet(): String = "Hello from Singleton!"
}

println(Singleton.greet())
```

## 集合和序列

Scala 提供了丰富的集合库，包括 `List`、`Set`、`Map` 等。

### 列表

```scala
val numbers = List(1, 2, 3, 4, 5)
println(numbers.map(_ * 2)) // 输出 List(2, 4, 6, 8, 10)
```

### 映射

```scala
val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
println(map.get("a")) // 输出 Some(1)
```

## 模式匹配

模式匹配是 Scala 中非常强大的特性，类似于其他语言中的 `switch` 语句。

```scala
def matchTest(x: Any): String = x match {
  case 1 => "one"
  case "two" => "two"
  case _ => "many"
}

println(matchTest(1)) // 输出 "one"
println(matchTest("two")) // 输出 "two"
println(matchTest(3)) // 输出 "many"
```

## 异常处理

Scala 的异常处理与 Java 类似，使用 `try/catch` 语句。

```scala
try {
  val result = 10 / 0
} catch {
  case e: ArithmeticException => println("Division by zero!")
}
```

## 特质 (Traits)

特质类似于 Java 中的接口，但可以包含方法的实现。

```scala
trait Greeter {
  def greet(): String
}

class EnglishGreeter extends Greeter {
  def greet(): String = "Hello!"
}

val greeter = new EnglishGreeter
println(greeter.greet())
```

## 隐式转换

隐式转换允许你在不修改源代码的情况下，自动将一种类型的对象转换为另一种类型。

```scala
implicit def intToString(x: Int): String = x.toString

val str: String = 42
println(str) // 输出 "42"
```

## 泛型

泛型允许你编写可以处理多种类型的代码。

```scala
class Box[T](value: T) {
  def getValue: T = value
}

val intBox = new Box(42)
println(intBox.getValue) // 输出 42

val stringBox = new Box("Hello")
println(stringBox.getValue) // 输出 "Hello"
```

## 高阶函数

高阶函数是指接受函数作为参数或返回函数的函数。

```scala
def applyTwice(f: Int => Int, x: Int): Int = f(f(x))

val double = (x: Int) => x * 2
println(applyTwice(double, 5)) // 输出 20
```

## 闭包

闭包是指捕获了外部变量的函数。

```scala
def makeAdder(x: Int): Int => Int = {
  (y: Int) => x + y
}

val add5 = makeAdder(5)
println(add5(10)) // 输出 15
```

## Akka Actor 模型

Akka 是一个用于构建并发和分布式应用的库，基于 Actor 模型。

```scala
import akka.actor.{Actor, ActorSystem, Props}

class HelloActor extends Actor {
  def receive = {
    case "hello" => println("Hello back at you!")
    case _       => println("Unknown message")
  }
}

val system = ActorSystem("HelloSystem")
val helloActor = system.actorOf(Props[HelloActor], name = "helloactor")
helloActor ! "hello"
helloActor ! "unknown"
```

## Future 和 Promise

`Future` 和 `Promise` 是 Scala 中用于处理异步操作的工具。

```scala
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val future = Future {
  Thread.sleep(1000)
  42
}

val result = Await.result(future, 2.seconds)
println(result) // 输出 42
```

## 并行集合

Scala 提供了并行集合，可以自动并行化集合操作。

```scala
val numbers = (1 to 1000000).par
val sum = numbers.sum
println(sum) // 输出 500000500000
```

## 响应式编程基础

响应式编程是一种编程范式，关注数据流和变化的传播。

```scala
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val future1 = Future { 1 + 2 }
val future2 = Future { 3 + 4 }

val combined = for {
  a <- future1
  b <- future2
} yield a + b

combined.foreach(println) // 输出 10
```

## SBT (Scala Build Tool)

SBT 是 Scala 的构建工具，类似于 Maven 或 Gradle。

```scala
name := "MyProject"
version := "1.0"
scalaVersion := "2.13.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
```

## ScalaTest 测试框架

ScalaTest 是 Scala 的测试框架，支持多种测试风格。

```scala
import org.scalatest.funsuite.AnyFunSuite

class MyTest extends AnyFunSuite {
  test("addition should work") {
    assert(1 + 1 == 2)
  }
}
```

## Scala REPL

Scala REPL (Read-Eval-Print Loop) 是一个交互式解释器，可以实时执行 Scala 代码。

```scala
scala> val x = 10
x: Int = 10

scala> println(x)
10
```

## 常用库介绍 (Cats, Scalaz)

Cats 和 Scalaz 是 Scala 中的函数式编程库，提供了丰富的类型类和函数式编程工具。

```scala
import cats.implicits._

val list = List(1, 2, 3)
val result = list.map(_ * 2).foldLeft(0)(_ + _)
println(result) // 输出 12
```

## 代码风格和规范

良好的代码风格和规范可以提高代码的可读性和可维护性。推荐使用 Scala 社区的代码风格指南。

## 函数式设计模式

函数式设计模式是一种基于函数式编程的设计模式，如 Monad、Functor 等。

```scala
import cats.Monad
import cats.implicits._

def sequence[F[_]: Monad, A](list: List[F[A]]): F[List[A]] =
  list.foldLeft(List.empty[A].pure[F]) { (acc, fa) =>
    for {
      a <- fa
      as <- acc
    } yield a :: as
  }.map(_.reverse)
```

## 性能优化技巧

性能优化是编程中的重要环节，可以通过减少内存分配、使用并行集合等方式进行优化。

## 错误处理策略

Scala 提供了多种错误处理策略，包括 `Option`、`Either`、`Try` 等。

```scala
def divide(a: Int, b: Int): Either[String, Int] =
  if (b == 0) Left("Division by zero")
  else Right(a / b)

println(divide(10, 2)) // 输出 Right(5)
println(divide(10, 0)) // 输出 Left("Division by zero")
```

## 类型系统深入

Scala 的类型系统非常强大，支持高级类型操作，如类型类、类型约束等。

```scala
trait Show[A] {
  def show(a: A): String
}

implicit val intShow: Show[Int] = new Show[Int] {
  def show(a: Int): String = a.toString
}

def printShow[A](a: A)(implicit s: Show[A]): Unit = {
  println(s.show(a))
}

printShow(42) // 输出 42
```

## 宏编程

宏编程允许你在编译时生成代码，提供了强大的元编程能力。

```scala
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

def helloMacro: Unit = macro helloMacroImpl

def helloMacroImpl(c: Context): c.Expr[Unit] = {
  import c.universe._
  c.Expr(q"""println("Hello, Macro!")""")
}

helloMacro // 输出 Hello, Macro!
```

## 元编程

元编程是指编写能够操作程序的程序，Scala 提供了丰富的反射和宏编程工具。

## 与 Java 互操作

Scala 可以与 Java 代码无缝互操作，可以直接调用 Java 类库。

```scala
import java.util.ArrayList

val list = new ArrayList[String]()
list.add("Hello")
list.add("World")
println(list) // 输出 [Hello, World]
```

## Web 应用开发 (使用 Play Framework)

Play Framework 是一个用于构建 Web 应用的 Scala 框架。

```scala
import play.api.mvc._
import play.api.routing.sird._
import play.api.routing._

object MyRouter extends SimpleRouter {
  def routes: Router.Routes = {
    case GET(p"/hello/$to") => Action {
      Results.Ok(s"Hello $to")
    }
  }
}
```

## 大数据处理 (使用 Apache Spark)

Apache Spark 是一个用于大数据处理的分布式计算框架。

```scala
import org.apache.spark.sql.SparkSession

val spark = SparkSession.builder.appName("SimpleApp").getOrCreate()
val data = spark.read.textFile("README.md")
val wordCounts = data.flatMap(line => line.split(" ")).countByValue()
println(wordCounts)
```

## RESTful API 服务

Scala 可以用于构建 RESTful API 服务，使用 Play Framework 或 Akka HTTP。

```scala
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import scala.io.StdIn

object WebServer {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("my-system")
    implicit val materializer = SystemMaterializer(system).materializer
    implicit val executionContext = system.dispatcher

    val route =
      path("hello") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
        }
      }

    val bindingFuture = Http().newServerAt("localhost", 8080).bind(route)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}
```

## IntelliJ IDEA 与 Scala 插件

IntelliJ IDEA 是 Scala 开发的首选 IDE，提供了强大的代码补全、调试和重构功能。

## Scala 文档和学习资源

Scala 官方文档和社区提供了丰富的学习资源，包括书籍、教程