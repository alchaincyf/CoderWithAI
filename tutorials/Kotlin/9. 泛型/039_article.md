---
title: 深入理解型变：协变与逆变
date: 2023-10-05
description: 本课程详细讲解了编程中的型变概念，包括协变和逆变的定义、应用场景及其在实际编程中的重要性。
slug: covariance-contravariance-in-programming
tags:
  - 型变
  - 协变
  - 逆变
category: 编程理论
keywords:
  - 型变
  - 协变
  - 逆变
  - 编程理论
  - 类型系统
---

# 型变（协变和逆变）

## 概述

在编程中，型变（Variance）是指泛型类型参数在子类型关系中的行为。Kotlin 支持协变（Covariance）和逆变（Contravariance），这两种机制允许我们在泛型类型中灵活地处理子类型关系。理解型变对于编写安全且灵活的代码至关重要。

## 协变（Covariance）

### 理论解释

协变是指泛型类型参数在子类型关系中保持一致。换句话说，如果 `A` 是 `B` 的子类型，那么 `C<A>` 也是 `C<B>` 的子类型。在 Kotlin 中，协变通过 `out` 关键字来实现。

### 代码示例

```kotlin
open class Animal
class Cat : Animal()

class Box<out T>(val item: T)

fun main() {
    val catBox: Box<Cat> = Box(Cat())
    val animalBox: Box<Animal> = catBox  // 协变允许将 Box<Cat> 赋值给 Box<Animal>
    println(animalBox.item)
}
```

### 实践练习

1. 创建一个 `Dog` 类，继承自 `Animal`。
2. 创建一个 `Box<Dog>` 实例，并尝试将其赋值给 `Box<Animal>`。
3. 解释为什么这种赋值是安全的。

## 逆变（Contravariance）

### 理论解释

逆变是指泛型类型参数在子类型关系中反转。换句话说，如果 `A` 是 `B` 的子类型，那么 `C<B>` 是 `C<A>` 的子类型。在 Kotlin 中，逆变通过 `in` 关键字来实现。

### 代码示例

```kotlin
open class Animal
class Cat : Animal()

class Box<in T> {
    fun setItem(item: T) {
        println("Item set: $item")
    }
}

fun main() {
    val animalBox: Box<Animal> = Box<Animal>()
    val catBox: Box<Cat> = animalBox  // 逆变允许将 Box<Animal> 赋值给 Box<Cat>
    catBox.setItem(Cat())
}
```

### 实践练习

1. 创建一个 `Dog` 类，继承自 `Animal`。
2. 创建一个 `Box<Dog>` 实例，并尝试将其赋值给 `Box<Animal>`。
3. 解释为什么这种赋值是安全的。

## 不变（Invariance）

### 理论解释

不变是指泛型类型参数在子类型关系中既不保持一致也不反转。换句话说，`C<A>` 和 `C<B>` 之间没有子类型关系。这是 Kotlin 中泛型类型的默认行为。

### 代码示例

```kotlin
class Box<T>(val item: T)

fun main() {
    val catBox: Box<Cat> = Box(Cat())
    // val animalBox: Box<Animal> = catBox  // 这行代码会编译错误，因为 Box 是不变的
}
```

### 实践练习

1. 尝试将 `Box<Cat>` 赋值给 `Box<Animal>`，并观察编译错误。
2. 解释为什么这种赋值是不安全的。

## 总结

型变（协变和逆变）是 Kotlin 中处理泛型类型子类型关系的重要机制。协变通过 `out` 关键字实现，允许将子类型的泛型实例赋值给父类型的泛型实例。逆变通过 `in` 关键字实现，允许将父类型的泛型实例赋值给子类型的泛型实例。不变是默认行为，不允许这种赋值。

理解型变有助于编写更灵活和安全的代码，特别是在处理集合、函数参数和返回值时。

## 下一步

接下来，我们将学习类型投影（Type Projection），它允许我们在特定情况下放宽型变规则。