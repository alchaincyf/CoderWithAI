---
title: 环境搭建：Node.js 与 npm 入门指南
date: 2023-10-05
description: 本课程将指导您如何搭建Node.js和npm的开发环境，包括安装、配置和基本使用。
slug: node-js-npm-environment-setup
tags:
  - Node.js
  - npm
  - 环境搭建
category: 编程基础
keywords:
  - Node.js安装
  - npm配置
  - 开发环境搭建
---

# 环境搭建 (Node.js, npm)

在开始使用Express.js之前，我们需要先搭建好开发环境。这包括安装Node.js和npm（Node Package Manager）。本教程将详细介绍如何安装和配置这些工具。

## 1. Node.js 简介

Node.js 是一个基于 Chrome V8 引擎的 JavaScript 运行时环境。它允许你在服务器端运行 JavaScript 代码，非常适合构建高性能的网络应用程序。

### 1.1 Node.js 的特点

- **非阻塞 I/O**: Node.js 使用事件驱动、非阻塞 I/O 模型，使其轻量且高效。
- **单线程**: 尽管是单线程，但通过事件循环和回调机制，Node.js 能够处理大量并发连接。
- **跨平台**: Node.js 可以在 Windows、macOS 和 Linux 上运行。

## 2. 安装 Node.js

### 2.1 下载 Node.js

访问 [Node.js 官方网站](https://nodejs.org/)，下载适合你操作系统的安装包。通常推荐下载 LTS（长期支持）版本，因为它更稳定。

### 2.2 安装 Node.js

1. **Windows**: 运行下载的 `.msi` 文件，按照安装向导的提示完成安装。
2. **macOS**: 运行下载的 `.pkg` 文件，按照安装向导的提示完成安装。
3. **Linux**: 使用包管理器安装，例如在 Ubuntu 上可以使用以下命令：
   ```bash
   sudo apt-get update
   sudo apt-get install nodejs
   ```

### 2.3 验证安装

安装完成后，打开终端（命令提示符）并输入以下命令来验证 Node.js 是否安装成功：

```bash
node -v
```

你应该会看到类似 `v14.17.0` 的输出，表示 Node.js 已成功安装。

## 3. npm 简介

npm 是 Node.js 的包管理器，用于安装和管理 JavaScript 库和工具。

### 3.1 验证 npm 安装

Node.js 安装包通常会自动安装 npm。你可以通过以下命令验证 npm 是否安装成功：

```bash
npm -v
```

你应该会看到类似 `6.14.13` 的输出，表示 npm 已成功安装。

### 3.2 更新 npm

npm 会定期更新，你可以使用以下命令来更新 npm：

```bash
npm install -g npm
```

## 4. 创建第一个 Node.js 项目

### 4.1 初始化项目

在你的工作目录中，创建一个新的文件夹并进入该文件夹：

```bash
mkdir my-first-node-project
cd my-first-node-project
```

然后使用 npm 初始化项目：

```bash
npm init -y
```

这会生成一个 `package.json` 文件，它是项目的配置文件。

### 4.2 安装 Express.js

使用 npm 安装 Express.js：

```bash
npm install express
```

### 4.3 编写第一个 Express 应用

在项目根目录下创建一个 `index.js` 文件，并添加以下代码：

```javascript
const express = require('express');
const app = express();
const port = 3000;

app.get('/', (req, res) => {
  res.send('Hello World!');
});

app.listen(port, () => {
  console.log(`App listening at http://localhost:${port}`);
});
```

### 4.4 运行应用

在终端中运行以下命令来启动应用：

```bash
node index.js
```

打开浏览器并访问 `http://localhost:3000`，你应该会看到 `Hello World!` 的输出。

## 5. 实践练习

### 5.1 练习目标

创建一个简单的 Express 应用，该应用能够处理以下路由：

- `/about`：返回一个简单的关于页面。
- `/contact`：返回一个联系信息页面。

### 5.2 练习步骤

1. 在 `index.js` 文件中添加 `/about` 和 `/contact` 路由。
2. 使用 `res.send()` 方法返回相应的页面内容。
3. 重新启动应用并测试这些路由。

### 5.3 示例代码

```javascript
const express = require('express');
const app = express();
const port = 3000;

app.get('/', (req, res) => {
  res.send('Hello World!');
});

app.get('/about', (req, res) => {
  res.send('This is the about page.');
});

app.get('/contact', (req, res) => {
  res.send('Contact us at contact@example.com');
});

app.listen(port, () => {
  console.log(`App listening at http://localhost:${port}`);
});
```

## 6. 总结

通过本教程，你已经学会了如何安装 Node.js 和 npm，并创建了一个简单的 Express.js 应用。接下来，你可以继续学习 Express.js 的其他高级功能，如路由、中间件、模板引擎等。

希望这篇教程对你有所帮助，祝你在学习 Express.js 的过程中取得成功！