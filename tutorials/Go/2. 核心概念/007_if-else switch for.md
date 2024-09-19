---
title: 控制流：if-else, switch, for 语句详解
date: 2023-10-05
description: 本课程详细讲解编程中的控制流结构，包括if-else条件语句、switch多分支选择语句以及for循环语句的使用方法和实际应用场景。
slug: control-flow-if-else-switch-for
tags:
  - 控制流
  - 条件语句
  - 循环语句
category: 编程基础
keywords:
  - if-else
  - switch
  - for循环
  - 控制流
  - 编程基础
---

# 控制流 (if-else, switch, for)

在编程中，控制流是指程序执行的顺序。通过控制流，我们可以根据不同的条件执行不同的代码块，或者重复执行某段代码。Go 语言提供了三种主要的控制流结构：`if-else`、`switch` 和 `for`。

## 1. if-else 语句

`if-else` 语句用于根据条件执行不同的代码块。如果条件为真，则执行 `if` 块中的代码；否则，执行 `else` 块中的代码。

### 1.1 基本语法

```go
if condition {
    // 当 condition 为真时执行的代码
} else {
    // 当 condition 为假时执行的代码
}
```

### 1.2 代码示例

```go
package main

import "fmt"

func main() {
    age := 18

    if age >= 18 {
        fmt.Println("你已经成年了！")
    } else {
        fmt.Println("你还未成年。")
    }
}
```

### 1.3 实践练习

编写一个程序，根据用户输入的分数判断其成绩等级（优秀、良好、及格、不及格）。

## 2. switch 语句

`switch` 语句用于根据不同的条件执行不同的代码块。它比多个 `if-else` 语句更简洁。

### 2.1 基本语法

```go
switch expression {
case value1:
    // 当 expression 等于 value1 时执行的代码
case value2:
    // 当 expression 等于 value2 时执行的代码
default:
    // 当 expression 不等于任何 case 时执行的代码
}
```

### 2.2 代码示例

```go
package main

import "fmt"

func main() {
    day := "Monday"

    switch day {
    case "Monday":
        fmt.Println("今天是星期一。")
    case "Tuesday":
        fmt.Println("今天是星期二。")
    default:
        fmt.Println("今天是其他日子。")
    }
}
```

### 2.3 实践练习

编写一个程序，根据用户输入的月份判断季节（春、夏、秋、冬）。

## 3. for 循环

`for` 循环用于重复执行某段代码。Go 语言中没有 `while` 循环，所有的循环都通过 `for` 实现。

### 3.1 基本语法

```go
for initialization; condition; post {
    // 循环体
}
```

### 3.2 代码示例

```go
package main

import "fmt"

func main() {
    for i := 0; i < 5; i++ {
        fmt.Println("当前值:", i)
    }
}
```

### 3.3 实践练习

编写一个程序，计算并输出 1 到 100 之间所有偶数的和。

## 4. 嵌套控制流

你可以在 `if-else`、`switch` 和 `for` 中嵌套使用这些控制流结构，以实现更复杂的逻辑。

### 4.1 代码示例

```go
package main

import "fmt"

func main() {
    for i := 1; i <= 3; i++ {
        fmt.Printf("外层循环: %d\n", i)
        for j := 1; j <= 3; j++ {
            fmt.Printf("内层循环: %d\n", j)
        }
    }
}
```

### 4.2 实践练习

编写一个程序，输出九九乘法表。

## 5. 总结

通过本教程，你已经学习了 Go 语言中的三种主要控制流结构：`if-else`、`switch` 和 `for`。这些结构是编写复杂程序的基础。通过实践练习，你可以更好地掌握这些概念。

## 6. 下一步

接下来，你将学习 Go 语言中的数组和切片，它们是处理数据集合的重要工具。继续前进，探索更多 Go 语言的强大功能吧！