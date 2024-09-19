---
title: 条件语句详解：if, unless, case
date: 2023-10-05
description: 本课程详细讲解编程中的条件语句，包括if、unless和case语句的使用方法和实际应用场景。
slug: conditional-statements-if-unless-case
tags:
  - 编程基础
  - 条件语句
  - 控制流
category: 编程入门
keywords:
  - if语句
  - unless语句
  - case语句
  - 条件控制
  - 编程教程
---

# 条件语句 (if, unless, case)

在编程中，条件语句用于根据不同的条件执行不同的代码块。Ruby 提供了多种条件语句，包括 `if`、`unless` 和 `case`。这些语句帮助我们编写更具逻辑性和灵活性的代码。

## 1. `if` 语句

`if` 语句是最常用的条件语句之一。它根据条件的真假来决定是否执行某个代码块。

### 1.1 基本语法

```ruby
if condition
  # 当 condition 为真时执行的代码
end
```

### 1.2 示例

```ruby
x = 10

if x > 5
  puts "x 大于 5"
end
```

在这个例子中，`x > 5` 是一个条件表达式。如果 `x` 大于 5，则输出 "x 大于 5"。

### 1.3 `if-else` 语句

`if-else` 语句在条件为假时提供了另一种执行路径。

```ruby
if condition
  # 当 condition 为真时执行的代码
else
  # 当 condition 为假时执行的代码
end
```

### 1.4 示例

```ruby
x = 3

if x > 5
  puts "x 大于 5"
else
  puts "x 小于或等于 5"
end
```

在这个例子中，如果 `x` 大于 5，则输出 "x 大于 5"；否则输出 "x 小于或等于 5"。

### 1.5 `if-elsif-else` 语句

`if-elsif-else` 语句允许我们检查多个条件。

```ruby
if condition1
  # 当 condition1 为真时执行的代码
elsif condition2
  # 当 condition2 为真时执行的代码
else
  # 当所有条件都为假时执行的代码
end
```

### 1.6 示例

```ruby
x = 7

if x > 10
  puts "x 大于 10"
elsif x > 5
  puts "x 大于 5 但小于或等于 10"
else
  puts "x 小于或等于 5"
end
```

在这个例子中，程序会根据 `x` 的值输出不同的信息。

## 2. `unless` 语句

`unless` 语句与 `if` 语句相反，它在条件为假时执行代码块。

### 2.1 基本语法

```ruby
unless condition
  # 当 condition 为假时执行的代码
end
```

### 2.2 示例

```ruby
x = 3

unless x > 5
  puts "x 小于或等于 5"
end
```

在这个例子中，如果 `x` 不大于 5，则输出 "x 小于或等于 5"。

### 2.3 `unless-else` 语句

`unless-else` 语句在条件为真时提供了另一种执行路径。

```ruby
unless condition
  # 当 condition 为假时执行的代码
else
  # 当 condition 为真时执行的代码
end
```

### 2.4 示例

```ruby
x = 7

unless x > 5
  puts "x 小于或等于 5"
else
  puts "x 大于 5"
end
```

在这个例子中，如果 `x` 不大于 5，则输出 "x 小于或等于 5"；否则输出 "x 大于 5"。

## 3. `case` 语句

`case` 语句用于多重条件判断，类似于其他语言中的 `switch` 语句。

### 3.1 基本语法

```ruby
case expression
when value1
  # 当 expression 等于 value1 时执行的代码
when value2
  # 当 expression 等于 value2 时执行的代码
else
  # 当 expression 不等于任何值时执行的代码
end
```

### 3.2 示例

```ruby
grade = "B"

case grade
when "A"
  puts "优秀"
when "B"
  puts "良好"
when "C"
  puts "中等"
else
  puts "不及格"
end
```

在这个例子中，程序会根据 `grade` 的值输出不同的评价。

### 3.3 范围匹配

`case` 语句还可以使用范围进行匹配。

```ruby
score = 85

case score
when 90..100
  puts "优秀"
when 80..89
  puts "良好"
when 70..79
  puts "中等"
else
  puts "不及格"
end
```

在这个例子中，程序会根据 `score` 的值输出不同的评价。

## 4. 实践练习

### 4.1 练习 1: 温度转换

编写一个程序，根据用户输入的温度值和单位（摄氏度或华氏度），输出转换后的温度值。

```ruby
puts "请输入温度值:"
temperature = gets.chomp.to_f

puts "请输入单位 (C 或 F):"
unit = gets.chomp.upcase

case unit
when "C"
  converted_temperature = (temperature * 9 / 5) + 32
  puts "#{temperature}°C 转换为华氏度是 #{converted_temperature}°F"
when "F"
  converted_temperature = (temperature - 32) * 5 / 9
  puts "#{temperature}°F 转换为摄氏度是 #{converted_temperature}°C"
else
  puts "无效的单位"
end
```

### 4.2 练习 2: 判断闰年

编写一个程序，判断用户输入的年份是否为闰年。

```ruby
puts "请输入年份:"
year = gets.chomp.to_i

if (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
  puts "#{year} 是闰年"
else
  puts "#{year} 不是闰年"
end
```

## 5. 总结

条件语句是编程中非常重要的工具，它们帮助我们根据不同的条件执行不同的代码。通过 `if`、`unless` 和 `case` 语句，我们可以编写更加灵活和逻辑性强的程序。希望这篇教程能帮助你更好地理解和使用这些条件语句。