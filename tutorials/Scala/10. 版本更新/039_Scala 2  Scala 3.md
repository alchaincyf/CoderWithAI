---
title: 从 Scala 2 迁移到 Scala 3 教程
date: 2023-10-05
description: 本课程详细讲解如何将现有的 Scala 2 项目迁移到最新的 Scala 3 版本，涵盖关键变化、迁移策略和最佳实践。
slug: scala-2-to-scala-3-migration
tags:
  - Scala
  - 编程迁移
  - Scala 3
category: 编程语言
keywords:
  - Scala 2
  - Scala 3
  - 迁移指南
  - 编程教程
---

# 从 Scala 2 迁移到 Scala 3

## 概述

Scala 3（也称为 Dotty）是 Scala 语言的重大更新版本，引入了许多新特性和改进。本教程将帮助你从 Scala 2 迁移到 Scala 3，涵盖关键的语法变化、新特性和迁移策略。

## 1. Scala 3 新特性概览

### 1.1 类型系统改进

Scala 3 引入了更强大的类型系统，包括：

- **Union Types**: 允许类型为多个类型的联合。
- **Intersection Types**: 允许类型为多个类型的交集。
- **Type Lambdas**: 允许在类型级别上定义匿名函数。

```scala
// Union Types
def printEither(value: String | Int): Unit = value match {
  case s: String => println(s"String: $s")
  case i: Int => println(s"Int: $i")
}

// Intersection Types
trait A { def a: String }
trait B { def b: Int }
def foo(x: A & B): Unit = println(s"a: ${x.a}, b: ${x.b}")
```

### 1.2 模式匹配改进

Scala 3 对模式匹配进行了改进，包括：

- **Match Types**: 允许在类型级别上进行模式匹配。
- **Extractor Objects**: 简化了提取器对象的定义。

```scala
// Match Types
type Elem[X] = X match {
  case List[t] => t
  case Array[t] => t
}

// Extractor Objects
object Even {
  def unapply(n: Int): Option[Int] = if (n % 2 == 0) Some(n) else None
}

val x = 4
x match {
  case Even(n) => println(s"$n is even")
  case _ => println("Not even")
}
```

### 1.3 隐式转换和隐式参数改进

Scala 3 对隐式转换和隐式参数进行了改进，包括：

- **Given Instances**: 替代了 Scala 2 中的 `implicit` 关键字。
- **Using Clauses**: 简化了隐式参数的使用。

```scala
// Given Instances
given intOrdering: Ordering[Int] = Ordering.Int

// Using Clauses
def max[A](x: A, y: A)(using ord: Ordering[A]): A = if (ord.gt(x, y)) x else y

val maxInt = max(3, 5) // 使用 given intOrdering
```

## 2. 迁移策略

### 2.1 逐步迁移

建议采用逐步迁移的策略，逐步将代码库从 Scala 2 迁移到 Scala 3。可以先从新项目或小模块开始，逐步扩展到整个代码库。

### 2.2 使用迁移工具

Scala 3 提供了一些工具来帮助迁移，包括：

- **Scalafix**: 一个代码重构工具，可以帮助自动迁移代码。
- **Scala 3 Migration Guide**: 官方提供的迁移指南，包含详细的迁移步骤和示例。

```bash
# 使用 Scalafix 进行迁移
scalafix --rules=ProcedureSyntax
```

### 2.3 测试和验证

在迁移过程中，确保对代码进行充分的测试和验证。使用 ScalaTest 或其他测试框架来确保迁移后的代码行为与 Scala 2 一致。

```scala
// 使用 ScalaTest 进行测试
import org.scalatest.funsuite.AnyFunSuite

class MyTest extends AnyFunSuite {
  test("addition should work") {
    assert(1 + 1 == 2)
  }
}
```

## 3. 实践练习

### 3.1 迁移一个简单的 Scala 2 项目到 Scala 3

1. **创建一个简单的 Scala 2 项目**：

   ```scala
   // src/main/scala/Main.scala
   object Main {
     def main(args: Array[String]): Unit = {
       println("Hello, Scala 2!")
     }
   }
   ```

2. **使用 Scalafix 进行迁移**：

   ```bash
   scalafix --rules=ProcedureSyntax
   ```

3. **验证迁移后的代码**：

   ```scala
   // src/main/scala/Main.scala
   object Main:
     def main(args: Array[String]): Unit =
       println("Hello, Scala 3!")
   ```

### 3.2 使用 Scala 3 的新特性

1. **使用 Union Types**：

   ```scala
   def printEither(value: String | Int): Unit = value match {
     case s: String => println(s"String: $s")
     case i: Int => println(s"Int: $i")
   }

   printEither("Hello")
   printEither(42)
   ```

2. **使用 Given Instances**：

   ```scala
   given intOrdering: Ordering[Int] = Ordering.Int

   def max[A](x: A, y: A)(using ord: Ordering[A]): A = if (ord.gt(x, y)) x else y

   val maxInt = max(3, 5)
   println(s"Max: $maxInt")
   ```

## 4. 总结

Scala 3 引入了许多新特性和改进，使得代码更加简洁和强大。通过逐步迁移和使用迁移工具，你可以顺利地将 Scala 2 项目迁移到 Scala 3。希望本教程能帮助你更好地理解和应用 Scala 3 的新特性。

## 5. 进一步学习资源

- **Scala 3 Documentation**: [https://dotty.epfl.ch/docs/](https://dotty.epfl.ch/docs/)
- **Scala 3 Migration Guide**: [https://docs.scala-lang.org/scala3/guides/migration/](https://docs.scala-lang.org/scala3/guides/migration/)
- **Scalafix**: [https://scalacenter.github.io/scalafix/](https://scalacenter.github.io/scalafix/)

通过这些资源，你可以进一步深入学习和掌握 Scala 3 的特性和迁移技巧。