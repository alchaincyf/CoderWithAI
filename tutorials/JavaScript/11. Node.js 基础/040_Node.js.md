---
title: Node.js 环境设置教程
date: 2023-10-05
description: 本教程详细介绍了如何在不同操作系统上设置Node.js开发环境，包括安装Node.js、配置npm以及创建第一个Node.js项目。
slug: node-js-environment-setup
tags:
  - Node.js
  - 环境设置
  - 开发环境
category: 编程教程
keywords:
  - Node.js 安装
  - npm 配置
  - Node.js 开发环境
---

# Node.js 环境设置

## 概述

Node.js 是一个基于 Chrome V8 引擎的 JavaScript 运行时环境，它使得 JavaScript 可以在服务器端运行。本教程将指导你如何在你的计算机上设置 Node.js 开发环境。

## 1. 安装 Node.js

### 1.1 下载 Node.js

首先，你需要从 [Node.js 官方网站](https://nodejs.org/) 下载适合你操作系统的安装包。通常，你会看到两个版本：

- **LTS (Long Term Support)**: 推荐大多数用户使用，稳定且长期支持。
- **Current**: 包含最新的功能，但可能不如 LTS 稳定。

### 1.2 安装 Node.js

下载完成后，运行安装包并按照提示完成安装。安装过程中，Node.js 会自动配置环境变量，使得你可以在命令行中直接使用 `node` 和 `npm` 命令。

### 1.3 验证安装

安装完成后，打开命令行工具（如 Windows 的命令提示符或 macOS/Linux 的终端），输入以下命令来验证 Node.js 和 npm 是否安装成功：

```bash
node -v
npm -v
```

如果安装成功，你会看到 Node.js 和 npm 的版本号。

## 2. 设置开发环境

### 2.1 安装代码编辑器

推荐使用以下代码编辑器之一：

- **Visual Studio Code (VS Code)**: 功能强大且免费，支持丰富的插件。
- **Sublime Text**: 轻量级且快速。
- **Atom**: GitHub 开发的编辑器，社区活跃。

### 2.2 创建项目目录

在你的计算机上创建一个新的目录来存放你的 Node.js 项目。例如：

```bash
mkdir my-node-project
cd my-node-project
```

### 2.3 初始化项目

在项目目录中，使用 `npm` 初始化一个新的 Node.js 项目：

```bash
npm init -y
```

这会生成一个 `package.json` 文件，它是你的项目的配置文件。

## 3. 编写第一个 Node.js 程序

### 3.1 创建主文件

在项目目录中创建一个名为 `index.js` 的文件，这是你的主入口文件。

### 3.2 编写代码

在 `index.js` 文件中编写以下代码：

```javascript
console.log("Hello, Node.js!");
```

### 3.3 运行程序

在命令行中运行以下命令来执行你的 Node.js 程序：

```bash
node index.js
```

你应该会看到输出：

```
Hello, Node.js!
```

## 4. 安装和使用 npm 包

### 4.1 安装 Express

Express 是一个流行的 Node.js 框架，用于构建 Web 应用程序。你可以使用 `npm` 来安装它：

```bash
npm install express
```

### 4.2 使用 Express

在 `index.js` 文件中，编写以下代码来使用 Express：

```javascript
const express = require('express');
const app = express();
const port = 3000;

app.get('/', (req, res) => {
  res.send('Hello, Express!');
});

app.listen(port, () => {
  console.log(`Server is running on http://localhost:${port}`);
});
```

### 4.3 运行服务器

保存文件后，运行以下命令来启动服务器：

```bash
node index.js
```

打开浏览器并访问 `http://localhost:3000`，你应该会看到页面显示 "Hello, Express!"。

## 5. 实践练习

### 5.1 练习 1: 创建一个简单的 API

使用 Express 创建一个简单的 API，返回一个 JSON 对象。例如：

```javascript
app.get('/api/user', (req, res) => {
  res.json({ name: 'Alice', age: 30 });
});
```

### 5.2 练习 2: 使用 npm 包

安装并使用一个 npm 包，例如 `lodash`，来处理数组操作。

```bash
npm install lodash
```

在 `index.js` 中使用 `lodash`：

```javascript
const _ = require('lodash');

const numbers = [1, 2, 3, 4, 5];
const doubled = _.map(numbers, n => n * 2);

console.log(doubled); // 输出: [2, 4, 6, 8, 10]
```

## 6. 总结

通过本教程，你已经学会了如何在你的计算机上设置 Node.js 开发环境，并编写了一个简单的 Node.js 程序。你还了解了如何使用 npm 安装和管理包，以及如何使用 Express 创建一个简单的 Web 服务器。继续探索 Node.js 和 npm 的更多功能，你将能够构建更复杂的应用程序。

## 下一步

- 学习 Node.js 的模块系统。
- 探索 Node.js 的文件系统操作。
- 创建一个完整的 HTTP 服务器。

希望你喜欢这个教程，并继续深入学习 Node.js！