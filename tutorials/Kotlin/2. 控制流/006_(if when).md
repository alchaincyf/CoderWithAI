---
title: 条件语句 (if, when) 编程教程
date: 2023-10-05
description: 本课程详细讲解编程中的条件语句，包括if和when的使用方法，帮助你掌握如何根据不同条件执行不同的代码块。
slug: conditional-statements-if-when
tags:
  - 编程基础
  - 条件语句
  - 控制流
category: 编程基础
keywords:
  - if语句
  - when语句
  - 条件控制
---

# 条件语句 (if, when)

## 概述

在编程中，条件语句用于根据不同的条件执行不同的代码块。Kotlin 提供了两种主要的条件语句：`if` 和 `when`。`if` 语句用于简单的条件判断，而 `when` 语句则适用于更复杂的条件分支。

## 1. `if` 语句

### 1.1 基本语法

`if` 语句的基本语法如下：

```kotlin
if (条件) {
    // 条件为真时执行的代码
} else {
    // 条件为假时执行的代码
}
```

### 1.2 示例

```kotlin
fun main() {
    val number = 10

    if (number > 5) {
        println("Number is greater than 5")
    } else {
        println("Number is not greater than 5")
    }
}
```

### 1.3 多重条件

你还可以使用 `else if` 来处理多个条件：

```kotlin
fun main() {
    val number = 3

    if (number > 5) {
        println("Number is greater than 5")
    } else if (number == 5) {
        println("Number is equal to 5")
    } else {
        println("Number is less than 5")
    }
}
```

### 1.4 `if` 表达式

在 Kotlin 中，`if` 语句可以作为表达式使用，这意味着它可以返回一个值。

```kotlin
fun main() {
    val number = 10
    val result = if (number > 5) "Greater than 5" else "Not greater than 5"
    println(result)
}
```

## 2. `when` 语句

### 2.1 基本语法

`when` 语句类似于其他语言中的 `switch` 语句，但它更加强大和灵活。

```kotlin
when (变量) {
    值1 -> {
        // 当变量等于值1时执行的代码
    }
    值2 -> {
        // 当变量等于值2时执行的代码
    }
    else -> {
        // 当变量不等于任何值时执行的代码
    }
}
```

### 2.2 示例

```kotlin
fun main() {
    val day = "Monday"

    when (day) {
        "Monday" -> println("It's Monday")
        "Tuesday" -> println("It's Tuesday")
        "Wednesday" -> println("It's Wednesday")
        else -> println("It's another day")
    }
}
```

### 2.3 多重条件

你可以在 `when` 语句中使用多个条件：

```kotlin
fun main() {
    val number = 3

    when (number) {
        1, 2 -> println("Number is 1 or 2")
        3, 4 -> println("Number is 3 or 4")
        else -> println("Number is neither 1, 2, 3, nor 4")
    }
}
```

### 2.4 范围条件

你还可以使用范围条件：

```kotlin
fun main() {
    val number = 7

    when (number) {
        in 1..5 -> println("Number is between 1 and 5")
        in 6..10 -> println("Number is between 6 and 10")
        else -> println("Number is out of range")
    }
}
```

### 2.5 `when` 表达式

与 `if` 类似，`when` 也可以作为表达式使用：

```kotlin
fun main() {
    val number = 7
    val result = when (number) {
        in 1..5 -> "Between 1 and 5"
        in 6..10 -> "Between 6 and 10"
        else -> "Out of range"
    }
    println(result)
}
```

## 3. 实践练习

### 3.1 练习 1: 温度转换

编写一个程序，根据用户输入的温度值和单位（摄氏度或华氏度），将其转换为另一种单位并输出结果。

```kotlin
fun main() {
    print("Enter temperature: ")
    val temperature = readLine()!!.toDouble()
    print("Enter unit (C for Celsius, F for Fahrenheit): ")
    val unit = readLine()!!

    val result = when (unit) {
        "C" -> "Fahrenheit: ${temperature * 9 / 5 + 32}"
        "F" -> "Celsius: ${(temperature - 32) * 5 / 9}"
        else -> "Invalid unit"
    }

    println(result)
}
```

### 3.2 练习 2: 分数等级

编写一个程序，根据用户输入的分数，输出对应的等级（A, B, C, D, F）。

```kotlin
fun main() {
    print("Enter your score: ")
    val score = readLine()!!.toInt()

    val grade = when (score) {
        in 90..100 -> "A"
        in 80..89 -> "B"
        in 70..79 -> "C"
        in 60..69 -> "D"
        else -> "F"
    }

    println("Your grade is $grade")
}
```

## 4. 总结

条件语句是编程中非常重要的工具，它们允许你根据不同的条件执行不同的代码块。`if` 语句适用于简单的条件判断，而 `when` 语句则适用于更复杂的条件分支。通过练习，你可以更好地掌握这些语句的使用。

希望这篇教程能帮助你理解 Kotlin 中的条件语句。继续练习和探索，你将能够更熟练地使用这些工具来编写更复杂的程序。