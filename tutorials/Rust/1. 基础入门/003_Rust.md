---
title: 创建你的第一个 Rust 程序
date: 2023-10-05
description: 本教程将引导你通过简单的步骤创建并运行你的第一个 Rust 程序，适合初学者入门。
slug: first-rust-program
tags:
  - Rust
  - 编程入门
  - 初学者
category: 编程教程
keywords:
  - Rust 编程
  - 第一个 Rust 程序
  - Rust 入门
---

# 创建第一个 Rust 程序

## 概述

在本教程中，我们将引导你完成创建你的第一个 Rust 程序的过程。无论你是编程新手还是有经验的开发者，本教程都将帮助你理解 Rust 的基本概念和语法。我们将从 Rust 的安装开始，然后编写并运行一个简单的 Rust 程序。

## Rust 简介和特性

Rust 是一种系统编程语言，以其内存安全、并发性和高性能著称。Rust 的设计目标是提供与 C/C++ 相当的性能，同时避免常见的内存错误，如空指针引用和数据竞争。Rust 通过其所有权系统、借用和生命周期机制来实现这一目标。

## 环境搭建

### 安装 Rust

首先，你需要在你的计算机上安装 Rust。Rust 的安装非常简单，只需使用 `rustup` 工具。`rustup` 是 Rust 的版本管理工具，可以帮助你管理不同版本的 Rust 编译器和工具链。

打开终端并运行以下命令：

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

按照提示完成安装过程。安装完成后，你可以通过以下命令验证 Rust 是否安装成功：

```bash
rustc --version
```

你应该会看到类似 `rustc x.y.z (abcabcabc yyyy-mm-dd)` 的输出，表示 Rust 编译器已经成功安装。

### 安装 Cargo

`Cargo` 是 Rust 的包管理器和构建系统。它可以帮助你管理项目依赖、构建代码和运行测试。`Cargo` 通常会随 `rustup` 一起安装。你可以通过以下命令验证 `Cargo` 是否安装成功：

```bash
cargo --version
```

你应该会看到类似 `cargo x.y.z` 的输出，表示 `Cargo` 已经成功安装。

## 创建第一个 Rust 程序

### 使用 Cargo 创建新项目

`Cargo` 提供了一个命令来创建新的 Rust 项目。我们将使用 `cargo new` 命令来创建一个新的项目。

打开终端并运行以下命令：

```bash
cargo new hello_world
```

这将在当前目录下创建一个名为 `hello_world` 的新目录，并在其中生成一个基本的 Rust 项目结构。

### 项目结构

进入 `hello_world` 目录，你会看到以下文件和目录：

```
hello_world/
├── Cargo.toml
└── src
    └── main.rs
```

- `Cargo.toml` 是项目的配置文件，包含了项目的元数据和依赖项。
- `src/main.rs` 是项目的源代码文件，我们将在这里编写我们的第一个 Rust 程序。

### 编写代码

打开 `src/main.rs` 文件，你会看到以下代码：

```rust
fn main() {
    println!("Hello, world!");
}
```

这段代码定义了一个名为 `main` 的函数，这是 Rust 程序的入口点。`println!` 是一个宏，用于在控制台打印文本。

### 运行程序

在终端中，进入 `hello_world` 目录并运行以下命令来构建和运行程序：

```bash
cargo run
```

你应该会看到以下输出：

```
   Compiling hello_world v0.1.0 (/path/to/hello_world)
    Finished dev [unoptimized + debuginfo] target(s) in 0.42s
     Running `target/debug/hello_world`
Hello, world!
```

恭喜！你已经成功编写并运行了你的第一个 Rust 程序。

## 实践练习

### 练习 1: 修改程序

尝试修改 `main.rs` 文件中的代码，打印出不同的文本。例如：

```rust
fn main() {
    println!("Welcome to Rust!");
}
```

然后再次运行 `cargo run`，看看输出是否发生了变化。

### 练习 2: 添加更多输出

尝试在 `main` 函数中添加更多的 `println!` 语句，打印出多行文本。例如：

```rust
fn main() {
    println!("Welcome to Rust!");
    println!("This is my first Rust program.");
}
```

再次运行 `cargo run`，看看输出是否符合你的预期。

## 总结

在本教程中，我们介绍了 Rust 的基本概念，安装了 Rust 和 Cargo，并创建并运行了你的第一个 Rust 程序。通过这些步骤，你已经迈出了学习 Rust 的第一步。接下来，我们将深入探讨 Rust 的更多高级特性，如变量、数据类型、控制流等。

继续学习，你将能够掌握 Rust 的强大功能，并将其应用于各种编程任务中。