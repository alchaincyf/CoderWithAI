---
title: 深入理解 Rust 的 no_std 开发
date: 2023-10-05
description: 本课程将深入探讨如何在 Rust 中进行 no_std 开发，帮助开发者在没有标准库的环境中构建高效、可靠的应用程序。
slug: rust-no-std-development
tags:
  - Rust
  - no_std
  - Embedded Systems
category: 编程与开发
keywords:
  - Rust no_std
  - Embedded Rust
  - Rust 嵌入式开发
---

# no_std 开发

## 概述

在 Rust 中，`no_std` 开发是指在不依赖标准库（`std`）的情况下编写代码。这种开发方式通常用于嵌入式系统、操作系统内核、或者对内存和性能有严格要求的场景。`no_std` 开发需要使用 Rust 的核心库（`core`），它提供了基本的类型和功能，但不包括与操作系统交互的功能。

## 环境搭建

在开始 `no_std` 开发之前，你需要确保你的 Rust 环境已经配置好。你可以使用 `rustup` 来管理 Rust 工具链。

```bash
rustup update
rustup target add thumbv7em-none-eabihf  # 例如，添加一个嵌入式目标
```

## 创建第一个 `no_std` 程序

首先，我们创建一个新的 Rust 项目：

```bash
cargo new no_std_example
cd no_std_example
```

接下来，我们需要修改 `Cargo.toml` 文件，以指示 Cargo 我们不需要标准库：

```toml
[dependencies]
core = "*"

[profile.dev]
panic = "abort"

[profile.release]
panic = "abort"
```

然后，在 `src/main.rs` 中，我们需要禁用标准库并使用核心库：

```rust
#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
pub extern "C" fn _start() -> ! {
    loop {}
}
```

在这个示例中，我们定义了一个简单的 `panic` 处理函数和一个 `_start` 函数，这是在没有操作系统的情况下程序的入口点。

## 变量和数据类型

在 `no_std` 环境中，我们仍然可以使用 Rust 的核心库中的基本数据类型，如 `i32`、`f64`、`bool` 等。

```rust
let x: i32 = 42;
let y: f64 = 3.14;
let is_true: bool = true;
```

## 控制流

控制流语句如 `if`、`loop`、`while` 和 `for` 在 `no_std` 环境中同样可用。

```rust
let condition = true;
if condition {
    // do something
} else {
    // do something else
}

let mut counter = 0;
loop {
    counter += 1;
    if counter == 10 {
        break;
    }
}

while counter > 0 {
    counter -= 1;
}

for i in 0..5 {
    // do something with i
}
```

## 所有权系统

Rust 的所有权系统在 `no_std` 环境中同样适用。所有权系统帮助你管理内存，防止内存泄漏和数据竞争。

```rust
let s1 = "hello";
let s2 = s1;  // s1 的所有权转移到了 s2
// println!("{}", s1);  // 这行代码会报错，因为 s1 已经失效
```

## 借用和引用

借用和引用在 `no_std` 环境中同样重要。它们允许你在不获取所有权的情况下访问数据。

```rust
let s = "hello";
let len = calculate_length(&s);

fn calculate_length(s: &str) -> usize {
    s.len()
}
```

## 生命周期

生命周期是 Rust 中用于确保引用有效的机制。在 `no_std` 环境中，生命周期同样重要。

```rust
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
```

## 结构体和枚举

结构体和枚举在 `no_std` 环境中同样可用，它们是组织数据的有效方式。

```rust
struct Point {
    x: i32,
    y: i32,
}

enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}
```

## 模式匹配

模式匹配是 Rust 中处理枚举和结构体的强大工具。

```rust
let msg = Message::Move { x: 10, y: 20 };

match msg {
    Message::Quit => {
        // handle quit
    }
    Message::Move { x, y } => {
        // handle move
    }
    _ => {
        // handle other cases
    }
}
```

## 模块系统

模块系统帮助你组织代码。在 `no_std` 环境中，模块系统同样适用。

```rust
mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
    }
}

pub fn eat_at_restaurant() {
    front_of_house::hosting::add_to_waitlist();
}
```

## Cargo 包管理器

Cargo 是 Rust 的包管理器，它可以帮助你管理依赖、构建项目和运行测试。在 `no_std` 环境中，Cargo 同样适用。

```bash
cargo build
cargo test
```

## 创建和发布 crate

你可以使用 Cargo 创建和发布自己的 crate。

```bash
cargo new my_crate --lib
cd my_crate
cargo publish
```

## 使用外部依赖

在 `no_std` 环境中，你可以使用外部依赖，但需要确保这些依赖也支持 `no_std`。

```toml
[dependencies]
my_no_std_crate = "0.1.0"
```

## 泛型

泛型允许你编写适用于多种类型的代码。

```rust
fn largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];

    for item in list {
        if item > largest {
            largest = item;
        }
    }

    largest
}
```

## 特质 (Traits)

特质定义了类型必须实现的行为。

```rust
trait Summary {
    fn summarize(&self) -> String;
}

struct NewsArticle {
    headline: String,
    location: String,
    author: String,
    content: String,
}

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{}, by {} ({})", self.headline, self.author, self.location)
    }
}
```

## 错误处理

在 `no_std` 环境中，错误处理通常通过返回 `Result` 类型来实现。

```rust
fn divide(a: i32, b: i32) -> Result<i32, &'static str> {
    if b == 0 {
        Err("division by zero")
    } else {
        Ok(a / b)
    }
}
```

## 智能指针

智能指针是 Rust 中用于管理内存的工具。在 `no_std` 环境中，你可以使用 `Box`、`Rc` 和 `Arc` 等智能指针。

```rust
let b = Box::new(5);
println!("b = {}", b);
```

## 并发编程

在 `no_std` 环境中，并发编程通常通过消息传递和共享状态来实现。

```rust
use core::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn increment() {
    COUNTER.fetch_add(1, Ordering::SeqCst);
}
```

## 栈与堆

栈和堆是内存管理的两种方式。在 `no_std` 环境中，你需要了解如何使用栈和堆。

```rust
let stack_var = 42;  // 栈变量
let heap_var = Box::new(42);  // 堆变量
```

## unsafe Rust

`unsafe` 允许你绕过 Rust 的安全检查，但需要你手动确保代码的安全性。

```rust
unsafe {
    // unsafe code
}
```

## 智能指针详解

`Box`、`Rc` 和 `Arc` 是常用的智能指针。

```rust
let b = Box::new(5);
let rc = Rc::new(5);
let arc = Arc::new(5);
```

## Future 和 async/await

`Future` 和 `async/await` 是 Rust 中处理异步编程的工具。

```rust
async fn hello_world() {
    println!("Hello, world!");
}

#[tokio::main]
async fn main() {
    hello_world().await;
}
```

## tokio 运行时

`tokio` 是一个异步运行时，支持 `no_std` 环境。

```rust
#[tokio::main]
async fn main() {
    println!("Hello, world!");
}
```

## 异步 I/O

异步 I/O 允许你高效地处理 I/O 操作。

```rust
use tokio::io::{self, AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;

#[tokio::main]
async fn main() -> io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:6142").await?;

    loop {
        let (mut socket, _) = listener.accept().await?;

        tokio::spawn(async move {
            let mut buf = [0; 1024];

            // In a loop, read data from the socket and write the data back.
            loop {
                let n = match socket.read(&mut buf).await {
                    // socket closed
                    Ok(n) if n == 0 => return,
                    Ok(n) => n,
                    Err(e) => {
                        eprintln!("failed to read from socket; err = {:?}", e);
                        return;
                    }
                };

                // Write the data back
                if let Err(e) = socket.write_all(&buf[0..n]).await {
                    eprintln!("failed to write to socket; err = {:?}", e);
                    return;
                }
            }
        });
    }
}
```

## 单元测试

单元测试帮助你验证代码的正确性。

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
```

## 集成测试

集成测试验证多个模块之间的交互。

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integration() {
        assert_eq!(add(2, 2), 4);
    }
}
```

## 文档测试

文档测试允许你在文档中嵌入测试代码。

```rust
/// Adds two numbers together.
///
/// # Examples
///
/// ```
/// let result = my_crate::add(2, 2);
/// assert_eq!(result, 4);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

## 生成文档

你可以使用 `cargo doc` 生成项目的文档。

```bash
cargo doc --open
```

## 性能分析工具

性能分析工具帮助你优化代码的性能。

```bash
cargo build --release
cargo run --release
```

## 编译优化

编译优化可以提高代码的性能。

```toml
[profile.release]
opt-level = 3
```

## 并行计算

并行计算允许你同时执行多个任务。

```rust
use rayon::prelude::*;

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    let sum: i32 = numbers.par_iter().sum();
    println!("Sum: {}", sum);
}
```

## SIMD

SIMD（单指令多数据）允许你同时处理多个数据。

```rust
use packed_simd::*;

fn main() {
    let a = i32x4::new(1, 2, 3, 4);
    let b = i32x4::new(10, 20, 30, 40);
    let c = a + b;
    println!("{:?}", c);
}
```

## 常用库介绍

`serde`、`rayon` 和 `clap` 是常用的 Rust 库。

```toml
[dependencies]
serde = "1.0"
rayon = "1.5"
clap = "3.0"
```

## Web 开发

`Actix` 和 `Rocket` 是常用的 Rust Web 框架。

```rust
use actix_web::{get, web, App, HttpServer, Responder};

#[get("/")]
async fn index() -> impl Responder {
    "Hello, world!"
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new().service(index)
    })
    .bind("127.0.0.1:8080")?
    .run()
    .await
}
```

## 系统编程

系统编程允许你编写与操作系统交互的代码。

```rust
use std::process::Command;

fn main() {
    let output = Command::new("ls")
        .arg("-l")
        .output()
        .expect("failed to execute process");

    println!("{}", String::from_utf8_lossy(&output.stdout));
}
```

## 嵌入式开发

嵌入式开发允许你编写运行在嵌入式设备上的代码。

```rust
#![no_std]
#![no_main]

use cortex_m_rt::entry;
use panic_halt as _;
use stm32f4xx_hal::{prelude::*, stm32};

#[entry]
fn main() -> ! {
    let dp = stm32::Peripherals::take().unwrap();
    let gpioa = dp.GPIOA.split();
    let mut led = gpioa.pa5.into_push_pull_output();

    loop {
        led.set_high().unwrap();
        cortex_m::asm::delay(1_000_000);
        led.set_low().unwrap();
        cortex_m::asm::delay(1_000_000);
    }
}
```

## 代码组织和项目结构

良好的代码组织和项目结构有助于维护和扩展代码。

```bash
my_project/
├── Cargo.toml
├── src/
│   ├── main.rs
│   ├── lib.rs
│   └── utils/
│       └── mod.rs
└── tests/
    └── integration_test.rs
```

## 错误处理策略

良好的错误处理策略可以提高代码的健壮性。

```rust
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let result = divide(10, 0)?;
    println!("Result: {}", result);
    Ok(())
}
```

## 设计模式在 Rust 中的应用

设计模式是解决常见问题的通用解决方案。

```rust
struct Singleton;

impl Singleton {
    fn instance() -> &'static Singleton {
        static INSTANCE: Singleton = Singleton;
        &INSTANCE
    }
}
```

## 代码风格和规范

良好的代码风格和规范有助于提高代码的可读性和可维护性。

```rust
fn main() {
    let x = 42;
    let y = 3.14;
    let is_true = true;

    if is_true {
        println!("x: {}, y: {}", x, y);
    }
}
```

## 宏编程

宏编程允许你编写可重用的代码。

```rust
macro_rules! say_hello {
    () => {
        println!("Hello, world!");
    };
}

fn main() {
    say_hello!();
}
```

## FFI (与其他语言交互)

FFI 允许你与其他语言交互。

```rust
extern