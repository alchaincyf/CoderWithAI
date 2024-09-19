---
title: 创建你的第一个 Node.js 应用
date: 2023-10-05
description: 本课程将引导你从零开始创建你的第一个 Node.js 应用，涵盖基础设置、模块使用和简单的HTTP服务器搭建。
slug: first-nodejs-app
tags:
  - Node.js
  - 初学者
  - 编程入门
category: 编程教程
keywords:
  - Node.js 入门
  - 创建 Node.js 应用
  - 初学者 Node.js
---

# 创建第一个 Node.js 应用

## 1. Node.js 简介和特性

Node.js 是一个基于 Chrome V8 引擎的 JavaScript 运行时环境，它允许开发者使用 JavaScript 编写服务器端代码。Node.js 的主要特性包括：

- **非阻塞 I/O**：Node.js 使用事件驱动、非阻塞 I/O 模型，使其轻量且高效。
- **单线程**：Node.js 使用单线程事件循环机制来处理请求，避免了多线程的复杂性。
- **跨平台**：Node.js 可以在 Windows、macOS 和 Linux 等多种操作系统上运行。
- **丰富的模块生态系统**：Node.js 拥有庞大的模块库（npm），可以快速构建应用。

## 2. 环境搭建和版本管理（nvm）

在开始编写 Node.js 应用之前，首先需要安装 Node.js 和 npm（Node Package Manager）。为了方便管理 Node.js 版本，推荐使用 nvm（Node Version Manager）。

### 2.1 安装 nvm

在 macOS 和 Linux 上，可以使用以下命令安装 nvm：

```bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
```

在 Windows 上，可以从 [nvm-windows](https://github.com/coreybutler/nvm-windows) 下载安装包。

### 2.2 安装 Node.js

安装 nvm 后，可以使用以下命令安装 Node.js：

```bash
nvm install node
```

安装完成后，可以使用以下命令验证安装是否成功：

```bash
node -v
npm -v
```

## 3. 创建第一个 Node.js 应用

### 3.1 初始化项目

首先，创建一个新的项目目录并进入该目录：

```bash
mkdir my-first-node-app
cd my-first-node-app
```

然后，使用 npm 初始化项目：

```bash
npm init -y
```

这会生成一个 `package.json` 文件，用于管理项目的依赖和脚本。

### 3.2 编写第一个 Node.js 脚本

在项目目录中创建一个名为 `index.js` 的文件，并添加以下代码：

```javascript
// index.js
console.log("Hello, Node.js!");
```

### 3.3 运行脚本

在终端中运行以下命令来执行脚本：

```bash
node index.js
```

你应该会看到输出：

```bash
Hello, Node.js!
```

### 3.4 创建一个简单的 HTTP 服务器

接下来，我们将创建一个简单的 HTTP 服务器。在 `index.js` 文件中添加以下代码：

```javascript
// index.js
const http = require('http');

const hostname = '127.0.0.1';
const port = 3000;

const server = http.createServer((req, res) => {
  res.statusCode = 200;
  res.setHeader('Content-Type', 'text/plain');
  res.end('Hello, World!\n');
});

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
```

### 3.5 运行服务器

保存文件后，再次运行 `node index.js`。你应该会看到输出：

```bash
Server running at http://127.0.0.1:3000/
```

打开浏览器并访问 `http://127.0.0.1:3000/`，你应该会看到页面显示 `Hello, World!`。

## 4. 模块系统（CommonJS 和 ES Modules）

Node.js 支持两种模块系统：CommonJS 和 ES Modules。

### 4.1 CommonJS

CommonJS 是 Node.js 默认的模块系统，使用 `require` 和 `module.exports` 来导入和导出模块。

```javascript
// math.js
module.exports = {
  add: (a, b) => a + b,
  subtract: (a, b) => a - b,
};

// index.js
const math = require('./math');

console.log(math.add(5, 3)); // 输出: 8
console.log(math.subtract(5, 3)); // 输出: 2
```

### 4.2 ES Modules

ES Modules 是 ECMAScript 标准的一部分，使用 `import` 和 `export` 来导入和导出模块。

```javascript
// math.mjs
export const add = (a, b) => a + b;
export const subtract = (a, b) => a - b;

// index.mjs
import { add, subtract } from './math.mjs';

console.log(add(5, 3)); // 输出: 8
console.log(subtract(5, 3)); // 输出: 2
```

## 5. 包管理器（npm 和 yarn）

npm 是 Node.js 的默认包管理器，而 yarn 是另一个流行的包管理器。

### 5.1 使用 npm

安装依赖包：

```bash
npm install express
```

在 `package.json` 中添加依赖：

```json
{
  "dependencies": {
    "express": "^4.17.1"
  }
}
```

### 5.2 使用 yarn

安装 yarn：

```bash
npm install -g yarn
```

安装依赖包：

```bash
yarn add express
```

在 `package.json` 中添加依赖：

```json
{
  "dependencies": {
    "express": "^4.17.1"
  }
}
```

## 6. 实践练习

### 6.1 练习 1：创建一个简单的计算器

创建一个名为 `calculator.js` 的文件，实现加法、减法、乘法和除法功能，并导出这些函数。然后在 `index.js` 中导入并使用这些函数。

### 6.2 练习 2：创建一个简单的静态文件服务器

使用 `http` 模块创建一个简单的静态文件服务器，能够提供 HTML、CSS 和 JavaScript 文件。

## 7. 总结

通过本教程，你已经学会了如何创建第一个 Node.js 应用，了解了 Node.js 的基本特性和模块系统，并掌握了使用 npm 和 yarn 管理依赖包的方法。接下来，你可以继续学习 Node.js 的更多高级特性，如事件循环、异步编程、文件系统操作等。