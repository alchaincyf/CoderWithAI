---
title: 环境搭建：Node.js, npm, Angular CLI
date: 2023-10-05
description: 本课程详细介绍如何搭建Node.js、npm和Angular CLI的开发环境，为前端开发打下坚实基础。
slug: environment-setup-nodejs-npm-angular-cli
tags:
  - Node.js
  - npm
  - Angular CLI
category: 前端开发
keywords:
  - Node.js环境搭建
  - npm安装
  - Angular CLI配置
---

# 环境搭建 (Node.js, npm, Angular CLI)

## 1. 概述

在开始使用 Angular 开发应用之前，我们需要搭建一个合适的环境。这包括安装 Node.js、npm（Node Package Manager）以及 Angular CLI（Command Line Interface）。本教程将详细介绍如何完成这些步骤，并确保你能够顺利创建和运行你的第一个 Angular 应用。

## 2. 安装 Node.js 和 npm

### 2.1 Node.js 简介

Node.js 是一个基于 Chrome V8 引擎的 JavaScript 运行时环境。它允许你在服务器端运行 JavaScript 代码，并且非常适合构建高性能的网络应用程序。

### 2.2 安装 Node.js

1. **下载 Node.js**：
   - 访问 [Node.js 官方网站](https://nodejs.org/)。
   - 根据你的操作系统下载相应的安装包（推荐使用 LTS 版本）。

2. **安装 Node.js**：
   - 运行下载的安装包，并按照提示完成安装。

3. **验证安装**：
   - 打开终端（命令提示符或 PowerShell）。
   - 输入以下命令来检查 Node.js 和 npm 是否安装成功：
     ```bash
     node -v
     npm -v
     ```
   - 如果安装成功，你应该会看到 Node.js 和 npm 的版本号。

### 2.3 npm 简介

npm 是 Node.js 的包管理工具，用于安装、管理和分享 JavaScript 库和工具。它是 Node.js 生态系统的重要组成部分。

## 3. 安装 Angular CLI

### 3.1 Angular CLI 简介

Angular CLI 是一个命令行工具，用于初始化、开发和维护 Angular 应用程序。它提供了一系列命令，帮助你快速创建项目、生成组件、服务等。

### 3.2 安装 Angular CLI

1. **使用 npm 安装 Angular CLI**：
   - 在终端中运行以下命令来全局安装 Angular CLI：
     ```bash
     npm install -g @angular/cli
     ```

2. **验证安装**：
   - 输入以下命令来检查 Angular CLI 是否安装成功：
     ```bash
     ng version
     ```
   - 如果安装成功，你应该会看到 Angular CLI 的版本信息。

## 4. 创建第一个 Angular 应用

### 4.1 使用 Angular CLI 创建新项目

1. **创建新项目**：
   - 在终端中运行以下命令来创建一个新的 Angular 项目：
     ```bash
     ng new my-first-app
     ```
   - 系统会提示你选择一些配置选项，例如是否添加 Angular Routing 和选择样式表格式。你可以根据自己的需求进行选择。

2. **进入项目目录**：
   - 进入新创建的项目目录：
     ```bash
     cd my-first-app
     ```

### 4.2 运行 Angular 应用

1. **启动开发服务器**：
   - 在项目目录中运行以下命令来启动开发服务器：
     ```bash
     ng serve
     ```
   - 默认情况下，开发服务器会在 `http://localhost:4200/` 上运行。

2. **访问应用**：
   - 打开浏览器，访问 `http://localhost:4200/`。
   - 你应该会看到一个默认的 Angular 欢迎页面。

## 5. 实践练习

### 5.1 练习目标

通过本练习，你将熟悉如何使用 Angular CLI 创建和运行一个简单的 Angular 应用。

### 5.2 练习步骤

1. **创建新项目**：
   - 使用 `ng new` 命令创建一个新的 Angular 项目，命名为 `practice-app`。

2. **启动应用**：
   - 进入项目目录并使用 `ng serve` 命令启动应用。

3. **修改欢迎页面**：
   - 打开 `src/app/app.component.html` 文件，修改页面内容，例如添加一个新的标题或段落。

4. **查看修改效果**：
   - 保存文件后，刷新浏览器页面，查看修改后的效果。

## 6. 总结

通过本教程，你已经成功搭建了 Angular 开发环境，并创建并运行了你的第一个 Angular 应用。接下来，你可以继续学习 Angular 的其他高级特性，如组件、服务、路由等。希望你能享受 Angular 开发的乐趣，并不断探索和实践！

---

**下一步**：
- 学习 Angular 项目结构
- 创建和使用 Angular 组件
- 掌握 Angular 模板语法和数据绑定