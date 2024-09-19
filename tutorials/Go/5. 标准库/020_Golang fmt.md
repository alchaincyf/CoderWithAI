---
title: Golang fmt 包详解：格式化输入输出
date: 2023-10-05
description: 本课程详细介绍了Go语言中的fmt包，包括其常用的格式化输入输出函数和使用技巧。
slug: golang-fmt-package-tutorial
tags:
  - Go语言
  - 编程基础
  - 输入输出
category: 编程语言
keywords:
  - Go语言
  - fmt包
  - 格式化输入输出
---

# fmt 包教程

## 概述

`fmt` 包是 Go 语言标准库中非常重要的一个包，主要用于格式化输入和输出。它提供了多种函数来处理字符串、数字、布尔值等的格式化输出，以及从标准输入读取数据。本教程将详细介绍 `fmt` 包的功能和使用方法，并通过代码示例和实践练习帮助你掌握这些知识。

## 1. 基本输出函数

### 1.1 Print

`Print` 函数用于将参数打印到标准输出。它不会在输出后添加换行符。

```go
package main

import "fmt"

func main() {
    fmt.Print("Hello, ")
    fmt.Print("World!")
}
```

**输出:**

```
Hello, World!
```

### 1.2 Println

`Println` 函数与 `Print` 类似，但它会在输出后自动添加换行符。

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, ")
    fmt.Println("World!")
}
```

**输出:**

```
Hello, 
World!
```

### 1.3 Printf

`Printf` 函数用于格式化输出。它支持多种格式化动词（verbs），如 `%s` 表示字符串，`%d` 表示整数，`%f` 表示浮点数等。

```go
package main

import "fmt"

func main() {
    name := "Alice"
    age := 30
    fmt.Printf("Name: %s, Age: %d\n", name, age)
}
```

**输出:**

```
Name: Alice, Age: 30
```

## 2. 格式化动词

`fmt` 包支持多种格式化动词，以下是一些常用的动词：

- `%v`：默认格式
- `%T`：类型
- `%t`：布尔值
- `%d`：十进制整数
- `%b`：二进制整数
- `%f`：浮点数
- `%s`：字符串
- `%q`：带引号的字符串

### 示例

```go
package main

import "fmt"

func main() {
    var i int = 42
    var f float64 = 3.14
    var b bool = true
    var s string = "Hello"

    fmt.Printf("i: %v (%T)\n", i, i)
    fmt.Printf("f: %v (%T)\n", f, f)
    fmt.Printf("b: %v (%T)\n", b, b)
    fmt.Printf("s: %v (%T)\n", s, s)
}
```

**输出:**

```
i: 42 (int)
f: 3.14 (float64)
b: true (bool)
s: Hello (string)
```

## 3. 输入函数

### 3.1 Scan

`Scan` 函数用于从标准输入读取数据，并将读取的值存储到变量中。

```go
package main

import "fmt"

func main() {
    var name string
    var age int

    fmt.Print("Enter your name: ")
    fmt.Scan(&name)

    fmt.Print("Enter your age: ")
    fmt.Scan(&age)

    fmt.Printf("Name: %s, Age: %d\n", name, age)
}
```

**输入:**

```
Enter your name: Alice
Enter your age: 30
```

**输出:**

```
Name: Alice, Age: 30
```

### 3.2 Scanln

`Scanln` 函数与 `Scan` 类似，但它会在读取到换行符时停止。

```go
package main

import "fmt"

func main() {
    var name string
    var age int

    fmt.Print("Enter your name and age: ")
    fmt.Scanln(&name, &age)

    fmt.Printf("Name: %s, Age: %d\n", name, age)
}
```

**输入:**

```
Enter your name and age: Alice 30
```

**输出:**

```
Name: Alice, Age: 30
```

### 3.3 Scanf

`Scanf` 函数用于格式化输入，类似于 `Printf`。

```go
package main

import "fmt"

func main() {
    var name string
    var age int

    fmt.Print("Enter your name and age: ")
    fmt.Scanf("%s %d", &name, &age)

    fmt.Printf("Name: %s, Age: %d\n", name, age)
}
```

**输入:**

```
Enter your name and age: Alice 30
```

**输出:**

```
Name: Alice, Age: 30
```

## 4. 实践练习

### 练习1：格式化输出

编写一个程序，要求用户输入姓名和年龄，然后使用 `Printf` 函数格式化输出这些信息。

### 练习2：读取多个值

编写一个程序，要求用户输入多个整数，然后使用 `Scan` 函数读取这些整数并计算它们的和。

### 练习3：格式化输入

编写一个程序，要求用户输入一个浮点数和一个字符串，然后使用 `Scanf` 函数读取这些值并格式化输出。

## 5. 总结

`fmt` 包是 Go 语言中用于格式化输入和输出的重要工具。通过本教程，你已经学习了如何使用 `fmt` 包中的基本输出和输入函数，以及如何使用格式化动词进行格式化输出。希望这些知识能够帮助你在实际编程中更好地处理输入和输出。

## 下一步

接下来，你可以继续学习 Go 语言的其他标准库包，如 `io`、`os`、`net/http` 等，或者深入了解 Go 语言的并发编程、错误处理等高级主题。