---
title: 跳转表达式详解：break, continue, return
date: 2023-10-05
description: 本课程详细讲解编程中的跳转表达式，包括break、continue和return的使用方法及应用场景，帮助你更好地控制程序流程。
slug: jump-expressions-break-continue-return
tags:
  - 编程基础
  - 控制结构
  - 流程控制
category: 编程基础
keywords:
  - break
  - continue
  - return
  - 跳转表达式
  - 编程教程
---

# 跳转表达式 (break, continue, return)

在编程中，跳转表达式用于控制程序的流程，特别是在循环和函数中。Kotlin 提供了三种主要的跳转表达式：`break`、`continue` 和 `return`。这些表达式可以帮助你更灵活地控制代码的执行路径。

## 1. `break` 表达式

`break` 表达式用于立即终止最内层的循环。当你在循环中遇到 `break` 时，循环将立即停止，程序将继续执行循环后的代码。

### 1.1 代码示例

```kotlin
fun main() {
    for (i in 1..10) {
        if (i == 5) {
            break // 当 i 等于 5 时，终止循环
        }
        println(i)
    }
    println("循环结束")
}
```

### 1.2 输出结果

```
1
2
3
4
循环结束
```

### 1.3 解释

在这个例子中，当 `i` 等于 5 时，`break` 表达式被执行，循环立即终止。因此，`println(i)` 只打印了 1 到 4，循环后的代码 `println("循环结束")` 被执行。

## 2. `continue` 表达式

`continue` 表达式用于跳过当前循环的剩余部分，并继续下一次循环。当你在循环中遇到 `continue` 时，当前迭代将立即结束，程序将开始下一次迭代。

### 2.1 代码示例

```kotlin
fun main() {
    for (i in 1..10) {
        if (i % 2 == 0) {
            continue // 当 i 是偶数时，跳过当前迭代
        }
        println(i)
    }
    println("循环结束")
}
```

### 2.2 输出结果

```
1
3
5
7
9
循环结束
```

### 2.3 解释

在这个例子中，当 `i` 是偶数时，`continue` 表达式被执行，当前迭代被跳过，程序直接进入下一次迭代。因此，`println(i)` 只打印了奇数。

## 3. `return` 表达式

`return` 表达式用于从函数中返回一个值，并终止函数的执行。当你在函数中遇到 `return` 时，函数将立即返回指定的值（如果有），并且函数执行结束。

### 3.1 代码示例

```kotlin
fun findFirstEven(numbers: List<Int>): Int? {
    for (number in numbers) {
        if (number % 2 == 0) {
            return number // 找到第一个偶数时，返回该值并终止函数
        }
    }
    return null // 如果没有找到偶数，返回 null
}

fun main() {
    val numbers = listOf(1, 3, 5, 6, 7, 8)
    val firstEven = findFirstEven(numbers)
    println("第一个偶数是: $firstEven")
}
```

### 3.2 输出结果

```
第一个偶数是: 6
```

### 3.3 解释

在这个例子中，`findFirstEven` 函数遍历 `numbers` 列表，当找到第一个偶数时，使用 `return` 表达式返回该值并终止函数。如果没有找到偶数，函数返回 `null`。

## 4. 实践练习

### 4.1 练习 1: 使用 `break` 和 `continue`

编写一个程序，使用 `break` 和 `continue` 表达式来打印 1 到 20 之间的所有奇数，但跳过 15。

### 4.2 练习 2: 使用 `return`

编写一个函数，接受一个字符串列表，并返回第一个包含字母 "a" 的字符串。如果没有找到这样的字符串，返回 `null`。

### 4.3 练习 3: 综合练习

编写一个程序，使用 `break`、`continue` 和 `return` 表达式来实现一个简单的猜数字游戏。程序随机生成一个 1 到 100 之间的数字，用户有 5 次机会猜测。如果用户在 5 次内猜对，程序输出 "恭喜你猜对了！" 并结束。如果用户在 5 次内没有猜对，程序输出 "很遗憾，你没有猜对。" 并结束。

## 5. 总结

`break`、`continue` 和 `return` 是 Kotlin 中用于控制程序流程的重要表达式。`break` 用于终止循环，`continue` 用于跳过当前迭代，`return` 用于从函数中返回值并终止函数。通过合理使用这些表达式，你可以编写出更加灵活和高效的代码。

希望这篇教程能帮助你更好地理解和使用 Kotlin 中的跳转表达式。继续练习和探索，你将能够更熟练地应用这些概念。