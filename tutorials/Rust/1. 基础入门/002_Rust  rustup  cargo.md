---
title: Rust 环境搭建教程：使用 rustup 和 cargo
date: 2023-10-05
description: 本教程将指导你如何使用 rustup 和 cargo 搭建 Rust 开发环境，包括安装 Rust 编译器、管理工具链和创建新项目。
slug: rust-environment-setup
tags:
  - Rust
  - 环境搭建
  - 编程教程
category: 编程语言
keywords:
  - Rust 环境搭建
  - rustup
  - cargo
  - Rust 开发环境
---

# 环境搭建 (rustup, cargo)

## 1. Rust 简介和特性

Rust 是一种系统编程语言，由 Mozilla 开发，旨在提供内存安全、并发性和高性能。Rust 的设计目标是消除数据竞争，提供零成本抽象，并且没有垃圾回收机制。Rust 的特性包括：

- **内存安全**：通过所有权系统、借用和生命周期来确保内存安全。
- **并发性**：提供安全的并发编程模型，避免数据竞争。
- **高性能**：编译为高效的机器码，接近 C/C++ 的性能。
- **零成本抽象**：高级抽象不会带来运行时开销。

## 2. 环境搭建

在开始编写 Rust 程序之前，我们需要安装 Rust 的工具链。Rust 的工具链包括 `rustup` 和 `cargo`。

### 2.1 安装 rustup

`rustup` 是 Rust 的版本管理工具，可以方便地安装和管理不同版本的 Rust 编译器和工具链。

#### 2.1.1 在 Linux 和 macOS 上安装 rustup

打开终端并运行以下命令：

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

按照提示完成安装。安装完成后，重启终端或运行以下命令以确保 `rustup` 和 `cargo` 可用：

```bash
source $HOME/.cargo/env
```

#### 2.1.2 在 Windows 上安装 rustup

访问 [rustup 官方网站](https://rustup.rs/)，下载并运行安装程序。按照提示完成安装。

### 2.2 安装 cargo

`cargo` 是 Rust 的包管理器和构建工具。安装 `rustup` 时，`cargo` 会自动安装。

#### 2.2.1 验证安装

运行以下命令验证 `rustup` 和 `cargo` 是否安装成功：

```bash
rustc --version
cargo --version
```

如果显示版本号，说明安装成功。

### 2.3 更新 Rust

Rust 的版本更新频繁，可以使用 `rustup` 来更新 Rust：

```bash
rustup update
```

### 2.4 安装其他工具

Rust 生态系统中有许多有用的工具，可以通过 `cargo` 安装：

- **rustfmt**：代码格式化工具。
- **clippy**：代码检查工具。

安装这些工具：

```bash
cargo install rustfmt
cargo install clippy
```

## 3. 创建第一个 Rust 程序

### 3.1 创建新项目

使用 `cargo` 创建一个新的 Rust 项目：

```bash
cargo new hello_world
cd hello_world
```

`cargo new` 命令会生成一个新目录，包含一个简单的 Rust 项目结构：

```
hello_world/
├── Cargo.toml
└── src
    └── main.rs
```

- `Cargo.toml`：项目的配置文件，包含依赖和元数据。
- `src/main.rs`：主程序文件。

### 3.2 编写代码

打开 `src/main.rs` 文件，编写以下代码：

```rust
fn main() {
    println!("Hello, world!");
}
```

### 3.3 编译和运行

在项目根目录下运行以下命令来编译和运行程序：

```bash
cargo run
```

输出：

```
Hello, world!
```

## 4. 实践练习

### 4.1 修改程序

修改 `src/main.rs` 文件，输出你自己的问候语。例如：

```rust
fn main() {
    println!("你好，Rust!");
}
```

再次运行 `cargo run`，查看输出。

### 4.2 添加依赖

在 `Cargo.toml` 文件中添加一个依赖库，例如 `rand`：

```toml
[dependencies]
rand = "0.8"
```

然后在 `src/main.rs` 中使用 `rand` 库生成一个随机数：

```rust
use rand::Rng;

fn main() {
    let random_number = rand::thread_rng().gen_range(1..101);
    println!("随机数: {}", random_number);
}
```

运行 `cargo run`，查看随机数输出。

## 5. 总结

通过本教程，我们学习了如何安装 Rust 工具链，创建并运行第一个 Rust 程序，以及如何使用 `cargo` 管理依赖。Rust 的环境搭建是学习 Rust 编程的第一步，接下来我们将深入学习 Rust 的各个特性。

## 6. 下一步

在接下来的课程中，我们将学习 Rust 的变量和数据类型，控制流，所有权系统等内容。继续探索 Rust 的世界，编写更复杂的程序！