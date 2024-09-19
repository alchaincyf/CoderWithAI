---
title: 控制结构：if/else 语句与循环
date: 2023-10-05
description: 本课程详细讲解编程中的控制结构，包括if/else语句和各种循环结构，帮助你掌握条件判断和重复执行的编程技巧。
slug: control-structures-if-else-loops
tags:
  - 控制结构
  - if/else
  - 循环
category: 编程基础
keywords:
  - 控制结构
  - if/else语句
  - 循环结构
---

# 控制结构 (if/else, 循环)

在编程中，控制结构是用于控制程序执行流程的重要工具。Scala 提供了多种控制结构，包括 `if/else` 语句和各种循环结构。这些结构帮助我们根据条件执行不同的代码块，或者重复执行某段代码。

## 1. `if/else` 语句

`if/else` 语句用于根据条件执行不同的代码块。它的基本结构如下：

```scala
if (条件) {
  // 条件为真时执行的代码
} else {
  // 条件为假时执行的代码
}
```

### 1.1 基本用法

```scala
val number = 10

if (number > 0) {
  println("Number is positive")
} else {
  println("Number is non-positive")
}
```

在这个例子中，如果 `number` 大于 0，程序会输出 "Number is positive"；否则，输出 "Number is non-positive"。

### 1.2 `else if` 语句

你还可以使用 `else if` 来处理多个条件：

```scala
val number = 0

if (number > 0) {
  println("Number is positive")
} else if (number < 0) {
  println("Number is negative")
} else {
  println("Number is zero")
}
```

在这个例子中，程序会根据 `number` 的值输出不同的结果。

### 1.3 嵌套 `if/else`

`if/else` 语句可以嵌套使用，以处理更复杂的条件逻辑：

```scala
val number = 10

if (number > 0) {
  if (number % 2 == 0) {
    println("Number is positive and even")
  } else {
    println("Number is positive and odd")
  }
} else {
  println("Number is non-positive")
}
```

在这个例子中，程序首先检查 `number` 是否为正数，然后进一步检查它是奇数还是偶数。

## 2. 循环结构

循环结构用于重复执行某段代码，直到满足某个条件为止。Scala 提供了多种循环结构，包括 `while` 循环、`do-while` 循环和 `for` 循环。

### 2.1 `while` 循环

`while` 循环在条件为真时重复执行代码块：

```scala
var i = 0

while (i < 5) {
  println(s"Current value of i: $i")
  i += 1
}
```

在这个例子中，程序会输出 `i` 的值，直到 `i` 不再小于 5。

### 2.2 `do-while` 循环

`do-while` 循环与 `while` 循环类似，但它会先执行一次代码块，然后再检查条件：

```scala
var i = 0

do {
  println(s"Current value of i: $i")
  i += 1
} while (i < 5)
```

在这个例子中，即使 `i` 的初始值不满足条件，代码块也会至少执行一次。

### 2.3 `for` 循环

`for` 循环用于遍历集合或范围：

```scala
for (i <- 1 to 5) {
  println(s"Current value of i: $i")
}
```

在这个例子中，程序会输出从 1 到 5 的所有整数。

### 2.4 嵌套循环

循环可以嵌套使用，以处理更复杂的逻辑：

```scala
for (i <- 1 to 3) {
  for (j <- 1 to 3) {
    println(s"i: $i, j: $j")
  }
}
```

在这个例子中，程序会输出所有 `i` 和 `j` 的组合。

## 3. 实践练习

### 3.1 练习 1: 判断闰年

编写一个程序，判断给定的年份是否为闰年。闰年的条件是：
- 能被 4 整除但不能被 100 整除，或者
- 能被 400 整除。

```scala
def isLeapYear(year: Int): Boolean = {
  if ((year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)) {
    true
  } else {
    false
  }
}

val year = 2024
if (isLeapYear(year)) {
  println(s"$year is a leap year")
} else {
  println(s"$year is not a leap year")
}
```

### 3.2 练习 2: 计算阶乘

编写一个程序，计算给定整数的阶乘。阶乘的定义是：`n! = n * (n-1) * (n-2) * ... * 1`。

```scala
def factorial(n: Int): Int = {
  var result = 1
  for (i <- 1 to n) {
    result *= i
  }
  result
}

val number = 5
println(s"Factorial of $number is ${factorial(number)}")
```

### 3.3 练习 3: 打印九九乘法表

编写一个程序，打印九九乘法表。

```scala
for (i <- 1 to 9) {
  for (j <- 1 to 9) {
    print(s"$i * $j = ${i * j}\t")
  }
  println()
}
```

## 4. 总结

在本教程中，我们学习了 Scala 中的控制结构，包括 `if/else` 语句和各种循环结构。这些结构是编程中的基本工具，帮助我们根据条件执行不同的代码块，或者重复执行某段代码。通过实践练习，我们进一步巩固了这些概念。

在接下来的课程中，我们将学习函数式编程基础，进一步扩展我们的编程技能。