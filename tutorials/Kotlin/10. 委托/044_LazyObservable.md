---
title: 深入理解标准委托：Lazy、Observable 等高级特性
date: 2023-10-05
description: 本课程详细讲解了标准委托的高级特性，包括Lazy、Observable等，帮助开发者优化代码性能和响应性。
slug: standard-delegates-lazy-observable
tags:
  - 委托
  - Lazy
  - Observable
category: 编程高级教程
keywords:
  - 标准委托
  - Lazy委托
  - Observable委托
  - 代码优化
  - 响应式编程
---

# 标准委托 (lazy, observable 等)

## 概述

在 Kotlin 中，委托是一种设计模式，它允许一个对象将某些功能委托给另一个对象。Kotlin 通过 `by` 关键字提供了对委托的直接支持。标准委托是 Kotlin 标准库中预定义的委托，包括 `lazy`、`observable` 等。这些委托可以帮助我们简化代码，提高代码的可读性和可维护性。

## 1. 延迟初始化委托 (`lazy`)

### 1.1 理论解释

`lazy` 是一种用于延迟初始化属性的委托。它确保属性在第一次访问时才进行初始化，而不是在对象创建时就立即初始化。这对于那些初始化开销较大或初始化时机不确定的属性非常有用。

### 1.2 代码示例

```kotlin
val lazyValue: String by lazy {
    println("Computing lazyValue")
    "Hello, Lazy!"
}

fun main() {
    println("Before accessing lazyValue")
    println(lazyValue)
    println(lazyValue)
}
```

### 1.3 输出结果

```
Before accessing lazyValue
Computing lazyValue
Hello, Lazy!
Hello, Lazy!
```

### 1.4 解释

- `lazyValue` 是一个延迟初始化的属性。
- 在第一次访问 `lazyValue` 时，`lazy` 委托会执行初始化块中的代码，并返回结果。
- 后续访问 `lazyValue` 时，直接返回之前计算的结果，不会再次执行初始化块。

## 2. 可观察属性委托 (`observable`)

### 2.1 理论解释

`observable` 委托用于监听属性的变化。每当属性被赋值时，`observable` 委托会触发一个回调函数，通知我们属性的新值和旧值。

### 2.2 代码示例

```kotlin
import kotlin.properties.Delegates

class User {
    var name: String by Delegates.observable("<no name>") {
        prop, old, new ->
        println("${prop.name} changed from $old to $new")
    }
}

fun main() {
    val user = User()
    user.name = "Alice"
    user.name = "Bob"
}
```

### 2.3 输出结果

```
name changed from <no name> to Alice
name changed from Alice to Bob
```

### 2.4 解释

- `name` 属性使用了 `Delegates.observable` 委托。
- 每当 `name` 属性被赋值时，`observable` 委托会调用回调函数，打印属性的变化。

## 3. 非空属性委托 (`notNull`)

### 3.1 理论解释

`notNull` 委托用于确保属性在被访问之前已经被初始化。如果属性在访问时未被初始化，会抛出异常。

### 3.2 代码示例

```kotlin
import kotlin.properties.Delegates

class User {
    var age: Int by Delegates.notNull()
}

fun main() {
    val user = User()
    // user.age = 25
    println(user.age) // 如果未初始化，会抛出异常
}
```

### 3.3 输出结果

```
Exception in thread "main" java.lang.IllegalStateException: Property age should be initialized before get.
```

### 3.4 解释

- `age` 属性使用了 `Delegates.notNull` 委托。
- 如果 `age` 属性在访问时未被初始化，会抛出 `IllegalStateException` 异常。

## 4. 映射属性委托 (`map`)

### 4.1 理论解释

`map` 委托允许我们从 `Map` 中读取和写入属性值。这在处理 JSON 解析或配置文件时非常有用。

### 4.2 代码示例

```kotlin
class User(map: Map<String, Any>) {
    val name: String by map
    val age: Int by map
}

fun main() {
    val user = User(mapOf(
        "name" to "Alice",
        "age" to 30
    ))
    println("Name: ${user.name}, Age: ${user.age}")
}
```

### 4.3 输出结果

```
Name: Alice, Age: 30
```

### 4.4 解释

- `User` 类的构造函数接受一个 `Map` 参数。
- `name` 和 `age` 属性通过 `map` 委托从 `Map` 中读取值。

## 5. 实践练习

### 5.1 练习 1: 使用 `lazy` 委托

创建一个类 `HeavyObject`，其中包含一个开销较大的初始化操作。使用 `lazy` 委托确保该操作在第一次访问时才执行。

### 5.2 练习 2: 使用 `observable` 委托

创建一个类 `Person`，其中包含一个 `name` 属性。使用 `observable` 委托监听 `name` 属性的变化，并在每次变化时打印一条消息。

### 5.3 练习 3: 使用 `notNull` 委托

创建一个类 `Car`，其中包含一个 `speed` 属性。使用 `notNull` 委托确保 `speed` 属性在访问之前已经被初始化。

### 5.4 练习 4: 使用 `map` 委托

创建一个类 `Config`，其中包含多个配置项。使用 `map` 委托从 `Map` 中读取配置项的值。

## 6. 总结

Kotlin 的标准委托（如 `lazy`、`observable`、`notNull` 和 `map`）为我们提供了强大的工具，帮助我们简化代码、提高代码的可读性和可维护性。通过理解和掌握这些委托，我们可以更高效地编写 Kotlin 代码。

希望这篇教程能帮助你更好地理解和使用 Kotlin 的标准委托。继续探索 Kotlin 的更多特性，享受编程的乐趣吧！