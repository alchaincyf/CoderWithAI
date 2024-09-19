---
title: 深入理解Swift中的循环结构：for-in, while, repeat-while
date: 2023-10-05
description: 本课程详细讲解Swift编程语言中的三种主要循环结构：for-in, while, 和 repeat-while。通过实例和练习，帮助你掌握如何在不同场景下高效使用这些循环。
slug: swift-loop-structures
tags:
  - Swift
  - 循环
  - 编程基础
category: 编程基础
keywords:
  - Swift循环
  - for-in循环
  - while循环
  - repeat-while循环
---

# 循环 (for-in, while, repeat-while)

在编程中，循环是一种重复执行代码块的结构。Swift 提供了多种循环结构，包括 `for-in`、`while` 和 `repeat-while`。这些循环结构可以帮助你处理需要重复执行的任务，如遍历数组、处理集合数据或执行特定次数的操作。

## 1. `for-in` 循环

`for-in` 循环用于遍历序列（如数组、集合、字典、范围等）中的每一个元素。每次循环，变量会自动获取序列中的下一个元素。

### 1.1 基本语法

```swift
for 变量 in 序列 {
    // 循环体
}
```

### 1.2 代码示例

#### 遍历数组

```swift
let fruits = ["Apple", "Banana", "Cherry"]

for fruit in fruits {
    print("I like \(fruit)")
}
```

输出：

```
I like Apple
I like Banana
I like Cherry
```

#### 遍历范围

```swift
for number in 1...5 {
    print(number)
}
```

输出：

```
1
2
3
4
5
```

#### 遍历字典

```swift
let scores = ["Alice": 90, "Bob": 85, "Charlie": 88]

for (name, score) in scores {
    print("\(name) scored \(score)")
}
```

输出：

```
Alice scored 90
Bob scored 85
Charlie scored 88
```

### 1.3 实践练习

编写一个 `for-in` 循环，遍历一个包含 10 个整数的数组，并计算它们的总和。

```swift
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
var sum = 0

for number in numbers {
    sum += number
}

print("The sum is \(sum)")
```

## 2. `while` 循环

`while` 循环在每次循环开始时检查条件，如果条件为 `true`，则执行循环体中的代码。如果条件为 `false`，则循环结束。

### 2.1 基本语法

```swift
while 条件 {
    // 循环体
}
```

### 2.2 代码示例

#### 计算 1 到 10 的总和

```swift
var i = 1
var sum = 0

while i <= 10 {
    sum += i
    i += 1
}

print("The sum is \(sum)")
```

输出：

```
The sum is 55
```

### 2.3 实践练习

编写一个 `while` 循环，打印出 1 到 20 之间的所有偶数。

```swift
var number = 1

while number <= 20 {
    if number % 2 == 0 {
        print(number)
    }
    number += 1
}
```

## 3. `repeat-while` 循环

`repeat-while` 循环类似于 `while` 循环，但它在每次循环结束时检查条件。这意味着循环体至少会执行一次。

### 3.1 基本语法

```swift
repeat {
    // 循环体
} while 条件
```

### 3.2 代码示例

#### 猜数字游戏

```swift
var target = 7
var guess = 0

repeat {
    print("Guess a number between 1 and 10:")
    guess = Int(readLine()!)!
} while guess != target

print("You guessed it!")
```

### 3.3 实践练习

编写一个 `repeat-while` 循环，要求用户输入一个正整数，直到输入的数字大于 100 为止。

```swift
var number = 0

repeat {
    print("Enter a positive integer:")
    number = Int(readLine()!)!
} while number <= 100

print("The number is greater than 100.")
```

## 4. 总结

- `for-in` 循环适用于遍历序列中的元素。
- `while` 循环在每次循环开始时检查条件。
- `repeat-while` 循环在每次循环结束时检查条件，确保循环体至少执行一次。

通过这些循环结构，你可以有效地处理需要重复执行的任务，提高代码的效率和可读性。

## 5. 下一步

在掌握了循环结构后，你可以继续学习控制转移语句（如 `break` 和 `continue`），它们可以帮助你在循环中更灵活地控制流程。接下来，我们将深入探讨这些内容。