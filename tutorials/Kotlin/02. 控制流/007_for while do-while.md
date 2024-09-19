---
title: 深入理解循环结构：for, while, do-while
date: 2023-10-05
description: 本课程详细讲解编程中的循环结构，包括for循环、while循环和do-while循环，帮助你掌握循环的基本概念和实际应用。
slug: understanding-loops-for-while-do-while
tags:
  - 循环
  - for循环
  - while循环
category: 编程基础
keywords:
  - 循环结构
  - for循环
  - while循环
---

# 循环 (for, while, do-while)

在编程中，循环是一种重要的控制结构，它允许我们重复执行一段代码，直到满足某个条件为止。Kotlin 提供了三种主要的循环结构：`for`、`while` 和 `do-while`。每种循环都有其特定的使用场景和语法。

## 1. `for` 循环

`for` 循环通常用于遍历集合或范围。它的语法简洁明了，适合处理已知次数的循环。

### 1.1 基本语法

```kotlin
for (item in collection) {
    // 循环体
}
```

### 1.2 示例

#### 1.2.1 遍历数组

```kotlin
val numbers = arrayOf(1, 2, 3, 4, 5)
for (number in numbers) {
    println(number)
}
```

#### 1.2.2 遍历范围

```kotlin
for (i in 1..5) {
    println(i)
}
```

### 1.3 实践练习

编写一个程序，使用 `for` 循环计算 1 到 100 的所有偶数的和。

```kotlin
var sum = 0
for (i in 1..100) {
    if (i % 2 == 0) {
        sum += i
    }
}
println("Sum of even numbers from 1 to 100: $sum")
```

## 2. `while` 循环

`while` 循环在执行循环体之前检查条件。如果条件为真，则执行循环体；否则，循环终止。

### 2.1 基本语法

```kotlin
while (condition) {
    // 循环体
}
```

### 2.2 示例

#### 2.2.1 基本 `while` 循环

```kotlin
var count = 0
while (count < 5) {
    println("Count: $count")
    count++
}
```

### 2.3 实践练习

编写一个程序，使用 `while` 循环计算 1 到 100 的所有奇数的和。

```kotlin
var sum = 0
var i = 1
while (i <= 100) {
    if (i % 2 != 0) {
        sum += i
    }
    i++
}
println("Sum of odd numbers from 1 to 100: $sum")
```

## 3. `do-while` 循环

`do-while` 循环与 `while` 循环类似，但它在执行循环体之后检查条件。这意味着循环体至少会执行一次。

### 3.1 基本语法

```kotlin
do {
    // 循环体
} while (condition)
```

### 3.2 示例

#### 3.2.1 基本 `do-while` 循环

```kotlin
var count = 0
do {
    println("Count: $count")
    count++
} while (count < 5)
```

### 3.3 实践练习

编写一个程序，使用 `do-while` 循环计算 1 到 100 的所有 3 的倍数的和。

```kotlin
var sum = 0
var i = 1
do {
    if (i % 3 == 0) {
        sum += i
    }
    i++
} while (i <= 100)
println("Sum of multiples of 3 from 1 to 100: $sum")
```

## 4. 跳转表达式

在循环中，我们有时需要提前终止循环或跳过当前迭代。Kotlin 提供了 `break` 和 `continue` 来实现这些功能。

### 4.1 `break`

`break` 用于立即终止循环。

```kotlin
for (i in 1..10) {
    if (i == 5) {
        break
    }
    println(i)
}
```

### 4.2 `continue`

`continue` 用于跳过当前迭代，继续下一次迭代。

```kotlin
for (i in 1..10) {
    if (i % 2 == 0) {
        continue
    }
    println(i)
}
```

### 4.3 实践练习

编写一个程序，使用 `for` 循环和 `break` 计算 1 到 100 的所有数的和，直到和超过 1000 为止。

```kotlin
var sum = 0
for (i in 1..100) {
    sum += i
    if (sum > 1000) {
        break
    }
}
println("Sum exceeded 1000 at: $sum")
```

## 5. 总结

循环是编程中的基本结构之一，掌握 `for`、`while` 和 `do-while` 循环的使用，以及 `break` 和 `continue` 的用法，对于编写高效的程序至关重要。通过实践练习，你可以更好地理解和应用这些循环结构。

## 6. 下一步

接下来，我们将学习 Kotlin 中的函数定义和调用，这是编程中的另一个重要主题。函数允许我们将代码模块化，提高代码的可读性和可维护性。