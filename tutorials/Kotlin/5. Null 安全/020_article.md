---
title: 深入理解可空类型与非空类型
date: 2023-10-05
description: 本课程详细讲解了编程中的可空类型与非空类型，帮助开发者理解如何在代码中安全地处理可能为空的值。
slug: nullable-and-non-nullable-types
tags:
  - 类型系统
  - 编程基础
  - 数据类型
category: 编程基础
keywords:
  - 可空类型
  - 非空类型
  - 类型安全
---

# 可空类型和非空类型

在 Kotlin 中，类型系统的一个重要特性是区分可空类型（Nullable Types）和非空类型（Non-Nullable Types）。这一特性帮助开发者避免常见的空指针异常（NullPointerException），从而提高代码的健壮性和安全性。

## 1. 可空类型和非空类型的基本概念

### 1.1 非空类型

非空类型是指变量在任何情况下都不能为 `null`。例如，`Int`、`String`、`Boolean` 等类型默认都是非空的。

```kotlin
val nonNullableString: String = "Hello, Kotlin"
```

在这个例子中，`nonNullableString` 是一个非空类型的变量，它的值不能为 `null`。

### 1.2 可空类型

可空类型是指变量可以为 `null`。在 Kotlin 中，通过在类型后面加上 `?` 来表示一个类型是可空的。

```kotlin
val nullableString: String? = null
```

在这个例子中，`nullableString` 是一个可空类型的变量，它的值可以为 `null`。

## 2. 安全调用操作符 (?.)

安全调用操作符 `?.` 允许你在调用可空类型变量的方法或属性时，避免空指针异常。如果变量为 `null`，则整个表达式的结果也为 `null`。

```kotlin
val nullableString: String? = "Hello, Kotlin"
val length: Int? = nullableString?.length
```

在这个例子中，如果 `nullableString` 为 `null`，则 `length` 也为 `null`，不会抛出异常。

## 3. Elvis 操作符 (?:)

Elvis 操作符 `?:` 用于在可空类型为 `null` 时提供一个默认值。

```kotlin
val nullableString: String? = null
val length: Int = nullableString?.length ?: 0
```

在这个例子中，如果 `nullableString` 为 `null`，则 `length` 的值为 `0`。

## 4. 非空断言 (!!)

非空断言操作符 `!!` 用于强制将可空类型转换为非空类型。如果变量为 `null`，则会抛出 `NullPointerException`。

```kotlin
val nullableString: String? = null
val length: Int = nullableString!!.length // 这里会抛出 NullPointerException
```

在这个例子中，由于 `nullableString` 为 `null`，使用 `!!` 会导致程序抛出异常。

## 5. 平台类型

平台类型是指从 Java 代码中引入的类型，Kotlin 无法确定这些类型是否可空。平台类型的变量在 Kotlin 中既可以作为可空类型使用，也可以作为非空类型使用。

```kotlin
val platformString: String! = javaMethodReturningString()
val length: Int = platformString.length // 这里假设 platformString 不为 null
```

在这个例子中，`platformString` 是一个平台类型，Kotlin 无法确定它是否为 `null`，因此需要开发者自行处理。

## 6. 实践练习

### 6.1 练习1：安全调用操作符

编写一个函数，接受一个可空类型的 `String`，并返回其长度。如果字符串为 `null`，则返回 `null`。

```kotlin
fun getLengthSafely(str: String?): Int? {
    return str?.length
}
```

### 6.2 练习2：Elvis 操作符

编写一个函数，接受一个可空类型的 `String`，并返回其长度。如果字符串为 `null`，则返回 `0`。

```kotlin
fun getLengthOrDefault(str: String?): Int {
    return str?.length ?: 0
}
```

### 6.3 练习3：非空断言

编写一个函数，接受一个可空类型的 `String`，并返回其长度。如果字符串为 `null`，则抛出 `NullPointerException`。

```kotlin
fun getLengthOrThrow(str: String?): Int {
    return str!!.length
}
```

## 7. 总结

Kotlin 的可空类型和非空类型机制为开发者提供了强大的工具来处理 `null` 值，从而避免常见的空指针异常。通过使用安全调用操作符 `?.`、Elvis 操作符 `?:` 和非空断言 `!!`，开发者可以编写更安全、更健壮的代码。

希望这篇教程能帮助你更好地理解 Kotlin 中的可空类型和非空类型，并在实际编程中应用这些知识。