---
title: 控制流基础：if, loop, while, for 语句详解
date: 2023-10-05
description: 本课程详细讲解编程中的控制流结构，包括if语句、loop循环、while循环和for循环，帮助你掌握如何使用这些关键语句来控制程序的执行流程。
slug: control-flow-if-loop-while-for
tags:
  - 控制流
  - 编程基础
  - 循环结构
category: 编程基础
keywords:
  - 控制流
  - if语句
  - loop循环
  - while循环
  - for循环
---

# 控制流 (if, loop, while, for)

在编程中，控制流是指程序执行的顺序。通过控制流，我们可以根据条件执行不同的代码块，或者重复执行某段代码。Rust 提供了多种控制流结构，包括 `if`、`loop`、`while` 和 `for`。本教程将详细介绍这些控制流结构，并通过代码示例和实践练习帮助你掌握它们。

## 1. `if` 语句

`if` 语句用于根据条件执行不同的代码块。条件通常是一个布尔表达式，如果条件为 `true`，则执行 `if` 代码块中的代码；如果条件为 `false`，则跳过 `if` 代码块。

### 1.1 基本语法

```rust
fn main() {
    let number = 5;

    if number < 10 {
        println!("number is less than 10");
    } else {
        println!("number is greater than or equal to 10");
    }
}
```

### 1.2 `else if` 语句

你可以使用 `else if` 来处理多个条件：

```rust
fn main() {
    let number = 20;

    if number < 10 {
        println!("number is less than 10");
    } else if number < 20 {
        println!("number is between 10 and 20");
    } else {
        println!("number is greater than or equal to 20");
    }
}
```

### 1.3 条件表达式

Rust 中的 `if` 语句还可以用作表达式，返回一个值：

```rust
fn main() {
    let condition = true;
    let number = if condition { 5 } else { 6 };

    println!("The value of number is: {}", number);
}
```

## 2. `loop` 循环

`loop` 循环用于无限循环执行某段代码，直到你手动中断它。

### 2.1 基本语法

```rust
fn main() {
    let mut count = 0;

    loop {
        count += 1;
        println!("count: {}", count);

        if count == 5 {
            break; // 中断循环
        }
    }
}
```

### 2.2 `break` 和 `continue`

- `break` 用于立即退出循环。
- `continue` 用于跳过当前迭代，继续下一次迭代。

```rust
fn main() {
    let mut count = 0;

    loop {
        count += 1;

        if count % 2 == 0 {
            continue; // 跳过偶数
        }

        println!("count: {}", count);

        if count == 5 {
            break; // 中断循环
        }
    }
}
```

## 3. `while` 循环

`while` 循环在条件为 `true` 时重复执行代码块。

### 3.1 基本语法

```rust
fn main() {
    let mut number = 3;

    while number != 0 {
        println!("{}!", number);
        number -= 1;
    }

    println!("LIFTOFF!!!");
}
```

### 3.2 `break` 和 `continue`

`while` 循环中也可以使用 `break` 和 `continue`：

```rust
fn main() {
    let mut number = 10;

    while number > 0 {
        if number % 2 == 0 {
            number -= 1;
            continue; // 跳过偶数
        }

        println!("{}!", number);
        number -= 1;
    }

    println!("LIFTOFF!!!");
}
```

## 4. `for` 循环

`for` 循环用于遍历集合（如数组、范围等）中的每个元素。

### 4.1 基本语法

```rust
fn main() {
    let a = [10, 20, 30, 40, 50];

    for element in a.iter() {
        println!("the value is: {}", element);
    }
}
```

### 4.2 范围循环

你可以使用 `..` 运算符来创建一个范围，并遍历它：

```rust
fn main() {
    for number in 1..4 {
        println!("{}!", number);
    }
    println!("LIFTOFF!!!");
}
```

### 4.3 `rev` 方法

你可以使用 `rev` 方法来反转范围的顺序：

```rust
fn main() {
    for number in (1..4).rev() {
        println!("{}!", number);
    }
    println!("LIFTOFF!!!");
}
```

## 5. 实践练习

### 练习 1: 猜数字游戏

编写一个简单的猜数字游戏。程序随机生成一个 1 到 100 之间的数字，用户输入猜测的数字，程序根据用户的输入给出提示（太大或太小），直到用户猜对为止。

```rust
use rand::Rng;
use std::io;

fn main() {
    let secret_number = rand::thread_rng().gen_range(1..=100);

    loop {
        let mut guess = String::new();

        println!("Please input your guess.");

        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read line");

        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        println!("You guessed: {}", guess);

        match guess.cmp(&secret_number) {
            std::cmp::Ordering::Less => println!("Too small!"),
            std::cmp::Ordering::Greater => println!("Too big!"),
            std::cmp::Ordering::Equal => {
                println!("You win!");
                break;
            }
        }
    }
}
```

### 练习 2: 斐波那契数列

编写一个程序，输出斐波那契数列的前 20 个数字。

```rust
fn main() {
    let mut a = 0;
    let mut b = 1;

    for _ in 0..20 {
        println!("{}", a);
        let temp = a;
        a = b;
        b = temp + b;
    }
}
```

### 练习 3: 计算阶乘

编写一个程序，计算并输出一个数字的阶乘。

```rust
fn main() {
    let number = 5;
    let mut factorial = 1;

    for i in 1..=number {
        factorial *= i;
    }

    println!("The factorial of {} is {}", number, factorial);
}
```

## 总结

通过本教程，你学习了 Rust 中的四种基本控制流结构：`if`、`loop`、`while` 和 `for`。这些结构是编程中的基础，掌握它们将帮助你编写更加灵活和功能强大的程序。通过实践练习，你可以更好地理解和应用这些概念。继续探索 Rust 的其他特性，你会发现这门语言的强大之处。