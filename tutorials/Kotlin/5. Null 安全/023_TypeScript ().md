---
title: 深入理解TypeScript中的非空断言 (!!)
date: 2023-10-05
description: 本课程详细讲解TypeScript中的非空断言操作符 (!!)，帮助开发者理解其使用场景和注意事项，避免潜在的运行时错误。
slug: typescript-non-null-assertion
tags:
  - TypeScript
  - 非空断言
  - 编程技巧
category: 编程语言
keywords:
  - TypeScript
  - 非空断言
  - !!操作符
---

# 非空断言 (`!!`) 教程

## 概述

在 Kotlin 中，类型系统区分可空类型和非空类型。可空类型允许变量持有 `null` 值，而非空类型则不允许。然而，在某些情况下，我们可能需要强制将一个可空类型的变量转换为非空类型，这时就可以使用非空断言操作符 (`!!`)。

## 理论解释

### 可空类型与非空类型

在 Kotlin 中，类型后面加上 `?` 表示该类型是可空的。例如，`String?` 表示一个可以为 `null` 的字符串。而 `String` 则表示一个不能为 `null` 的字符串。

```kotlin
var nullableString: String? = "Hello"
nullableString = null // 合法

var nonNullableString: String = "World"
// nonNullableString = null // 编译错误
```

### 非空断言 (`!!`)

非空断言操作符 (`!!`) 用于将一个可空类型的变量强制转换为非空类型。如果该变量实际上是 `null`，则会抛出 `NullPointerException`。

```kotlin
val nullableString: String? = "Hello"
val nonNullableString: String = nullableString!! // 强制转换为非空类型
```

如果 `nullableString` 是 `null`，则会在运行时抛出 `NullPointerException`。

## 代码示例

### 示例 1：基本用法

```kotlin
fun main() {
    val nullableString: String? = "Hello"
    val nonNullableString: String = nullableString!!
    println(nonNullableString) // 输出: Hello
}
```

### 示例 2：抛出异常

```kotlin
fun main() {
    val nullableString: String? = null
    val nonNullableString: String = nullableString!! // 抛出 NullPointerException
    println(nonNullableString) // 不会执行到这里
}
```

### 示例 3：结合 `try-catch`

```kotlin
fun main() {
    val nullableString: String? = null
    try {
        val nonNullableString: String = nullableString!!
        println(nonNullableString)
    } catch (e: NullPointerException) {
        println("NullPointerException 被捕获")
    }
}
```

## 实践练习

### 练习 1：安全调用与非空断言

编写一个函数，接受一个可空字符串参数，并返回其长度。如果参数为 `null`，则返回 `-1`。

```kotlin
fun getLength(str: String?): Int {
    return try {
        str!!.length
    } catch (e: NullPointerException) {
        -1
    }
}

fun main() {
    println(getLength("Hello")) // 输出: 5
    println(getLength(null))    // 输出: -1
}
```

### 练习 2：结合 `Elvis 操作符`

编写一个函数，接受一个可空字符串参数，并返回其长度。如果参数为 `null`，则返回 `0`。

```kotlin
fun getLength(str: String?): Int {
    return str?.length ?: 0
}

fun main() {
    println(getLength("Hello")) // 输出: 5
    println(getLength(null))    // 输出: 0
}
```

## 总结

非空断言 (`!!`) 是一个强大的工具，可以将可空类型的变量强制转换为非空类型。然而，使用时需要谨慎，因为它可能会在运行时抛出 `NullPointerException`。在实际开发中，应尽量使用安全调用 (`?.`) 和 `Elvis 操作符` (`?:`) 来避免空指针异常。

通过本教程，你应该已经掌握了非空断言的基本用法和注意事项。在实际编程中，合理使用这些工具将帮助你编写更健壮和安全的代码。