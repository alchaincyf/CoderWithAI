---
title: 第一个 TypeScript 程序
date: 2023-10-05
description: 本课程将引导您编写并运行您的第一个 TypeScript 程序，涵盖基础语法和环境设置。
slug: first-typescript-program
tags:
  - TypeScript
  - 编程入门
  - 前端开发
category: 编程基础
keywords:
  - TypeScript 入门
  - 第一个 TypeScript 程序
  - TypeScript 环境设置
---

# 第一个 TypeScript 程序

## 概述

在本教程中，我们将学习如何编写和运行你的第一个 TypeScript 程序。我们将从安装必要的开发环境开始，然后编写一个简单的 TypeScript 程序，并将其编译为 JavaScript 代码。最后，我们将在浏览器或 Node.js 环境中运行这个程序。

## 开发环境搭建

### 安装 Node.js

首先，你需要在你的计算机上安装 Node.js。Node.js 是一个基于 Chrome V8 引擎的 JavaScript 运行时，它允许你在服务器端运行 JavaScript 代码。

1. 访问 [Node.js 官方网站](https://nodejs.org/)。
2. 下载并安装适合你操作系统的 Node.js 版本。
3. 安装完成后，打开终端（命令提示符或 PowerShell）并运行以下命令来验证安装是否成功：

   ```bash
   node -v
   ```

   你应该会看到类似 `v14.17.0` 的输出，表示 Node.js 已成功安装。

### 安装 TypeScript 编译器

TypeScript 编译器（`tsc`）是一个将 TypeScript 代码转换为 JavaScript 代码的工具。你可以通过 npm（Node.js 的包管理器）来安装它。

1. 在终端中运行以下命令来全局安装 TypeScript 编译器：

   ```bash
   npm install -g typescript
   ```

2. 安装完成后，运行以下命令来验证安装是否成功：

   ```bash
   tsc -v
   ```

   你应该会看到类似 `Version 4.3.5` 的输出，表示 TypeScript 编译器已成功安装。

## 编写第一个 TypeScript 程序

### 创建项目目录

首先，创建一个新的项目目录，并在其中初始化一个新的 npm 项目。

1. 在终端中运行以下命令来创建项目目录并进入该目录：

   ```bash
   mkdir my-first-typescript-app
   cd my-first-typescript-app
   ```

2. 初始化一个新的 npm 项目：

   ```bash
   npm init -y
   ```

   这将创建一个 `package.json` 文件，用于管理项目的依赖和脚本。

### 编写 TypeScript 代码

1. 在项目目录中创建一个新的文件 `index.ts`，并在其中编写以下代码：

   ```typescript
   // index.ts
   function greet(name: string): string {
       return `Hello, ${name}!`;
   }

   const userName = "TypeScript";
   console.log(greet(userName));
   ```

   这段代码定义了一个名为 `greet` 的函数，它接受一个字符串参数 `name`，并返回一个问候语。然后，我们定义了一个变量 `userName`，并调用 `greet` 函数来输出问候语。

### 编译 TypeScript 代码

1. 在终端中运行以下命令来编译 TypeScript 代码：

   ```bash
   tsc index.ts
   ```

   这将生成一个名为 `index.js` 的 JavaScript 文件。

### 运行 JavaScript 代码

1. 你可以使用 Node.js 来运行生成的 JavaScript 代码：

   ```bash
   node index.js
   ```

   你应该会看到以下输出：

   ```
   Hello, TypeScript!
   ```

## 实践练习

### 练习 1：修改问候语

1. 修改 `index.ts` 文件中的 `greet` 函数，使其返回的问候语包含当前的日期和时间。
2. 重新编译并运行代码，验证输出是否正确。

### 练习 2：添加更多问候语

1. 在 `index.ts` 文件中添加一个新的函数 `greetAll`，它接受一个字符串数组作为参数，并为每个字符串生成一个问候语。
2. 调用 `greetAll` 函数，并输出所有问候语。

## 总结

在本教程中，我们学习了如何搭建 TypeScript 开发环境，并编写和运行了第一个 TypeScript 程序。通过这些步骤，你已经掌握了 TypeScript 的基本使用方法，并可以继续深入学习 TypeScript 的其他高级特性。

在接下来的教程中，我们将深入探讨 TypeScript 的基本类型、变量声明、函数、接口、类等内容，帮助你全面掌握 TypeScript 编程。