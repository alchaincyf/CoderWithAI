---
title: 深入理解Elvis操作符 (?:) 在编程中的应用
date: 2023-10-05
description: 本课程详细讲解了Elvis操作符 (?:) 在编程中的使用，包括其语法、应用场景以及如何在不同编程语言中实现。
slug: elvis-operator-programming-tutorial
tags:
  - 编程基础
  - 操作符
  - 条件语句
category: 编程语言基础
keywords:
  - Elvis操作符
  - 三元操作符
  - 编程技巧
---

# Elvis 操作符 (?:)

## 1. 简介

在 Kotlin 编程语言中，Elvis 操作符（`?:`）是一个非常有用的工具，用于处理可空类型（nullable types）。它的名字来源于其外观类似于猫王（Elvis Presley）的发型。Elvis 操作符允许你在某个值为 `null` 时提供一个默认值，从而简化代码并减少空指针异常的风险。

## 2. 基本语法

Elvis 操作符的基本语法如下：

```kotlin
val result = nullableValue ?: defaultValue
```

- `nullableValue` 是一个可空类型的变量或表达式。
- `defaultValue` 是当 `nullableValue` 为 `null` 时返回的默认值。

如果 `nullableValue` 不为 `null`，则 `result` 的值为 `nullableValue`；如果 `nullableValue` 为 `null`，则 `result` 的值为 `defaultValue`。

## 3. 代码示例

### 3.1 基本使用

```kotlin
fun main() {
    val nullableString: String? = null
    val nonNullString: String? = "Hello, Kotlin"

    val result1 = nullableString ?: "Default Value"
    val result2 = nonNullString ?: "Default Value"

    println(result1) // 输出: Default Value
    println(result2) // 输出: Hello, Kotlin
}
```

在这个例子中，`nullableString` 为 `null`，所以 `result1` 的值为 `"Default Value"`。而 `nonNullString` 不为 `null`，所以 `result2` 的值为 `"Hello, Kotlin"`。

### 3.2 结合其他操作符

Elvis 操作符可以与其他操作符结合使用，例如安全调用操作符（`?.`）：

```kotlin
fun main() {
    val nullableString: String? = null
    val length = nullableString?.length ?: -1

    println(length) // 输出: -1
}
```

在这个例子中，`nullableString?.length` 返回 `null`，因为 `nullableString` 为 `null`。因此，`length` 的值为 `-1`。

## 4. 实践练习

### 4.1 练习 1：处理用户输入

编写一个程序，要求用户输入一个整数。如果用户输入为空或无效，程序应返回 `-1`。

```kotlin
fun main() {
    print("请输入一个整数: ")
    val input = readLine()
    val number = input?.toIntOrNull() ?: -1

    println("你输入的整数是: $number")
}
```

### 4.2 练习 2：处理列表中的空值

编写一个函数，接受一个包含可空整数的列表，并返回一个包含非空整数的列表。如果列表中某个元素为 `null`，则将其替换为 `0`。

```kotlin
fun filterNonNull(list: List<Int?>): List<Int> {
    return list.map { it ?: 0 }
}

fun main() {
    val list = listOf(1, null, 3, null, 5)
    val filteredList = filterNonNull(list)

    println(filteredList) // 输出: [1, 0, 3, 0, 5]
}
```

## 5. 总结

Elvis 操作符是 Kotlin 中处理可空类型的一个强大工具。它允许你在值为 `null` 时提供一个默认值，从而简化代码并减少空指针异常的风险。通过结合其他操作符（如安全调用操作符 `?.`），你可以更灵活地处理可空类型。

希望这篇教程能帮助你更好地理解和使用 Elvis 操作符。继续练习和探索 Kotlin 的其他特性，你会发现这门语言的强大和优雅之处。