---
title: 深入理解循环结构：for 和 while 循环
date: 2023-10-05
description: 本课程将详细介绍编程中的循环结构，包括for循环和while循环的使用方法、区别及实际应用场景。
slug: understanding-loops-for-while
tags:
  - 编程基础
  - 循环结构
  - 算法
category: 编程基础
keywords:
  - for循环
  - while循环
  - 循环结构
---

# 循环 (for, while)

## 概述

在编程中，循环是一种重复执行代码块的结构。Python 提供了两种主要的循环结构：`for` 循环和 `while` 循环。`for` 循环通常用于遍历序列（如列表、元组、字符串等），而 `while` 循环则用于在条件为真时重复执行代码块。

## for 循环

### 理论解释

`for` 循环用于遍历序列（如列表、元组、字符串等）或其他可迭代对象。每次循环，`for` 语句会将序列中的一个元素赋值给循环变量，并执行循环体中的代码。

### 代码示例

```python
# 遍历列表
fruits = ["apple", "banana", "cherry"]
for fruit in fruits:
    print(fruit)

# 遍历字符串
for char in "Python":
    print(char)

# 使用 range() 函数
for i in range(5):
    print(i)  # 输出 0 到 4
```

### 实践练习

1. 创建一个包含 5 个整数的列表，使用 `for` 循环打印每个元素的平方。
2. 使用 `for` 循环计算 1 到 10 的和。

## while 循环

### 理论解释

`while` 循环在条件为真时重复执行代码块。条件在每次循环开始时检查，如果条件为假，循环终止。

### 代码示例

```python
# 基本 while 循环
count = 0
while count < 5:
    print(count)
    count += 1

# 使用 break 语句
while True:
    user_input = input("输入 'exit' 退出循环: ")
    if user_input == "exit":
        break
```

### 实践练习

1. 编写一个 `while` 循环，提示用户输入一个数字，直到用户输入 0 为止。
2. 使用 `while` 循环计算 1 到 10 的积。

## 循环控制语句

### 理论解释

Python 提供了 `break` 和 `continue` 语句来控制循环的执行流程：

- `break`：立即退出循环。
- `continue`：跳过当前循环的剩余代码，继续下一次循环。

### 代码示例

```python
# 使用 break
for i in range(10):
    if i == 5:
        break
    print(i)  # 输出 0 到 4

# 使用 continue
for i in range(10):
    if i % 2 == 0:
        continue
    print(i)  # 输出 1, 3, 5, 7, 9
```

### 实践练习

1. 使用 `break` 语句在 `for` 循环中找到列表中的第一个负数。
2. 使用 `continue` 语句在 `while` 循环中跳过所有偶数。

## 嵌套循环

### 理论解释

嵌套循环是指在一个循环内部包含另一个循环。这种结构常用于处理二维数据（如矩阵）或生成复杂的输出。

### 代码示例

```python
# 嵌套 for 循环
for i in range(3):
    for j in range(3):
        print(f"({i}, {j})")

# 嵌套 while 循环
i = 0
while i < 3:
    j = 0
    while j < 3:
        print(f"({i}, {j})")
        j += 1
    i += 1
```

### 实践练习

1. 使用嵌套 `for` 循环打印一个 5x5 的星号矩阵。
2. 使用嵌套 `while` 循环计算 1 到 10 的阶乘。

## 总结

循环是编程中的基本结构之一，能够帮助我们高效地处理重复性任务。通过 `for` 循环和 `while` 循环，我们可以遍历数据、执行条件判断、控制程序流程等。掌握循环的使用，是学习编程的重要一步。

## 下一步

在掌握了循环的基本概念和使用方法后，你可以继续学习异常处理 (`try, except`)，这将帮助你更好地处理程序中的错误和异常情况。