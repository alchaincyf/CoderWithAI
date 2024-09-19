---
title: 安装 Haskell 和开发环境设置 (GHC, Stack)
date: 2023-10-05
description: 本课程详细讲解如何安装 Haskell 编程语言及其开发环境，包括 GHC 编译器和 Stack 构建工具的配置与使用。
slug: install-haskell-and-setup-development-environment
tags:
  - Haskell
  - GHC
  - Stack
category: 编程语言
keywords:
  - Haskell 安装
  - GHC 配置
  - Stack 使用
---

# 安装 Haskell 和开发环境设置 (GHC, Stack)

## 1. 概述

Haskell 是一种纯函数式编程语言，以其强大的类型系统和惰性求值特性而闻名。为了开始 Haskell 编程之旅，首先需要安装 Haskell 编译器和构建工具。本教程将详细介绍如何安装 Haskell 编译器 GHC（Glasgow Haskell Compiler）和构建工具 Stack，并设置开发环境。

## 2. 安装 Haskell 编译器 (GHC)

### 2.1 下载和安装 GHC

GHC 是 Haskell 的主要编译器，支持多种操作系统和平台。以下是不同操作系统的安装步骤：

#### 2.1.1 Windows

1. **下载 GHC**: 访问 [GHC 官方网站](https://www.haskell.org/ghc/)，选择适合你系统的版本进行下载。
2. **安装 GHC**: 运行下载的安装程序，按照提示完成安装。

#### 2.1.2 macOS

1. **使用 Homebrew 安装**: 打开终端并运行以下命令：
   ```bash
   brew install ghc
   ```

#### 2.1.3 Linux

1. **使用包管理器安装**: 不同的 Linux 发行版有不同的包管理器。例如，在 Ubuntu 上可以运行：
   ```bash
   sudo apt-get update
   sudo apt-get install ghc
   ```

### 2.2 验证安装

安装完成后，可以通过以下命令验证 GHC 是否安装成功：

```bash
ghc --version
```

如果安装成功，将显示 GHC 的版本信息。

## 3. 安装 Haskell 构建工具 (Stack)

### 3.1 下载和安装 Stack

Stack 是一个用于构建 Haskell 项目的工具，简化了依赖管理和项目构建过程。以下是不同操作系统的安装步骤：

#### 3.1.1 Windows

1. **下载 Stack**: 访问 [Stack 官方网站](https://docs.haskellstack.org/en/stable/README/)，下载 Windows 安装包。
2. **安装 Stack**: 运行下载的安装程序，按照提示完成安装。

#### 3.1.2 macOS

1. **使用 Homebrew 安装**: 打开终端并运行以下命令：
   ```bash
   brew install haskell-stack
   ```

#### 3.1.3 Linux

1. **使用脚本安装**: 打开终端并运行以下命令：
   ```bash
   curl -sSL https://get.haskellstack.org/ | sh
   ```

### 3.2 验证安装

安装完成后，可以通过以下命令验证 Stack 是否安装成功：

```bash
stack --version
```

如果安装成功，将显示 Stack 的版本信息。

## 4. 设置开发环境

### 4.1 创建新项目

使用 Stack 创建一个新的 Haskell 项目：

```bash
stack new my-haskell-project
cd my-haskell-project
```

### 4.2 构建和运行项目

在项目目录中，运行以下命令来构建和运行项目：

```bash
stack build
stack exec my-haskell-project-exe
```

### 4.3 使用 GHCi 交互式环境

GHCi 是 Haskell 的交互式解释器，可以用于快速测试代码片段。启动 GHCi：

```bash
stack ghci
```

在 GHCi 中，可以输入 Haskell 表达式并立即查看结果。例如：

```haskell
Prelude> 2 + 2
4
```

## 5. 实践练习

### 5.1 练习 1: 编写简单的 Haskell 程序

在 `src/Lib.hs` 文件中编写一个简单的 Haskell 函数，例如计算两个数的和：

```haskell
module Lib where

add :: Int -> Int -> Int
add x y = x + y
```

然后在 `app/Main.hs` 中调用这个函数：

```haskell
module Main where

import Lib

main :: IO ()
main = do
    putStrLn "Enter two numbers:"
    x <- readLn
    y <- readLn
    putStrLn $ "The sum is: " ++ show (add x y)
```

### 5.2 练习 2: 使用 GHCi 测试函数

在 GHCi 中加载 `Lib.hs` 文件并测试 `add` 函数：

```bash
stack ghci src/Lib.hs
*Lib> add 3 4
7
```

## 6. 总结

通过本教程，你已经成功安装了 Haskell 编译器 GHC 和构建工具 Stack，并设置了开发环境。接下来，你可以开始编写和运行 Haskell 程序，探索 Haskell 的强大功能。

## 7. 下一步

- 学习 Haskell 的基本语法和类型系统。
- 探索 GHCi 的更多功能。
- 编写更复杂的函数和程序。

希望本教程能帮助你顺利开始 Haskell 编程之旅！