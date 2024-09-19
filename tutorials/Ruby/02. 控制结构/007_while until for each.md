---
title: 深入理解循环结构：while, until, for, each
date: 2023-10-05
description: 本课程详细讲解了编程中的循环结构，包括while、until、for和each循环的使用方法和实际应用场景。
slug: understanding-loops-while-until-for-each
tags:
  - 循环结构
  - 编程基础
  - 控制流
category: 编程基础
keywords:
  - while循环
  - until循环
  - for循环
  - each循环
---

# 循环 (while, until, for, each)

在编程中，循环是一种重复执行代码块的结构。Ruby 提供了多种循环结构，包括 `while`、`until`、`for` 和 `each`。本教程将详细介绍这些循环结构，并通过代码示例和实践练习帮助你掌握它们。

## 1. `while` 循环

`while` 循环在条件为 `true` 时重复执行代码块。一旦条件变为 `false`，循环将停止执行。

### 1.1 语法

```ruby
while condition
  # 代码块
end
```

### 1.2 示例

```ruby
counter = 0
while counter < 5
  puts "Counter: #{counter}"
  counter += 1
end
```

### 1.3 解释

- `counter < 5` 是循环的条件。
- 每次循环时，`counter` 的值会增加 1。
- 当 `counter` 的值达到 5 时，条件变为 `false`，循环停止。

### 1.4 实践练习

编写一个 `while` 循环，打印从 1 到 10 的所有奇数。

## 2. `until` 循环

`until` 循环与 `while` 循环相反，它在条件为 `false` 时重复执行代码块。一旦条件变为 `true`，循环将停止执行。

### 2.1 语法

```ruby
until condition
  # 代码块
end
```

### 2.2 示例

```ruby
counter = 0
until counter >= 5
  puts "Counter: #{counter}"
  counter += 1
end
```

### 2.3 解释

- `counter >= 5` 是循环的条件。
- 每次循环时，`counter` 的值会增加 1。
- 当 `counter` 的值达到 5 时，条件变为 `true`，循环停止。

### 2.4 实践练习

编写一个 `until` 循环，打印从 10 到 1 的所有偶数。

## 3. `for` 循环

`for` 循环用于遍历一个范围或集合中的每个元素。

### 3.1 语法

```ruby
for variable in collection
  # 代码块
end
```

### 3.2 示例

```ruby
for i in 1..5
  puts "Number: #{i}"
end
```

### 3.3 解释

- `1..5` 是一个范围对象，表示从 1 到 5 的所有整数。
- `i` 是循环变量，每次循环时会依次取范围中的值。

### 3.4 实践练习

编写一个 `for` 循环，打印从 1 到 10 的所有整数。

## 4. `each` 循环

`each` 循环是 Ruby 中常用的迭代器，用于遍历数组或哈希表中的每个元素。

### 4.1 语法

```ruby
collection.each do |variable|
  # 代码块
end
```

### 4.2 示例

```ruby
numbers = [1, 2, 3, 4, 5]
numbers.each do |number|
  puts "Number: #{number}"
end
```

### 4.3 解释

- `numbers` 是一个数组。
- `number` 是循环变量，每次循环时会依次取数组中的值。

### 4.4 实践练习

编写一个 `each` 循环，打印数组 `[2, 4, 6, 8, 10]` 中的每个元素。

## 5. 综合练习

编写一个程序，使用 `while`、`until`、`for` 和 `each` 循环分别打印以下内容：

1. 从 1 到 10 的所有奇数。
2. 从 10 到 1 的所有偶数。
3. 从 1 到 10 的所有整数。
4. 数组 `[3, 6, 9, 12, 15]` 中的每个元素。

## 6. 总结

通过本教程，你已经学习了 Ruby 中的四种主要循环结构：`while`、`until`、`for` 和 `each`。每种循环都有其特定的用途和语法，理解它们将帮助你编写更高效的代码。继续练习和探索，你将能够熟练地使用这些循环结构来解决各种编程问题。

## 7. 下一步

接下来，你可以学习 Ruby 中的迭代器和块，它们是 Ruby 编程中非常强大的工具。继续深入学习，你将能够编写更复杂和功能丰富的程序。