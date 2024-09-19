---
title: 创建第一个 Express.js 应用
date: 2023-10-05
description: 本课程将指导您从零开始创建您的第一个 Express.js 应用，涵盖基础设置、路由配置和简单的API开发。
slug: create-first-expressjs-app
tags:
  - Express.js
  - Node.js
  - Web开发
category: 后端开发
keywords:
  - Express.js 教程
  - Node.js 应用
  - 创建API
---

# 创建第一个 Express.js 应用

## 1. Express.js 简介和特点

Express.js 是一个基于 Node.js 的快速、开放、极简的 Web 框架。它提供了一套强大的功能，用于构建各种类型的 Web 应用程序和 API。Express.js 的特点包括：

- **轻量级**：Express.js 只提供最基本的 Web 开发功能，不强制使用任何特定的模板引擎或数据库。
- **灵活性**：开发者可以根据需求自由选择和集成各种中间件。
- **路由**：Express.js 提供了强大的路由功能，可以轻松处理不同 URL 路径的请求。
- **中间件**：Express.js 支持中间件，可以在请求处理过程中插入自定义逻辑。

## 2. 环境搭建

在开始创建 Express.js 应用之前，我们需要确保已经安装了 Node.js 和 npm（Node Package Manager）。

### 2.1 安装 Node.js 和 npm

1. **下载 Node.js**：访问 [Node.js 官网](https://nodejs.org/)，下载并安装适合你操作系统的 Node.js 版本。
2. **验证安装**：打开终端（命令行工具），输入以下命令来验证 Node.js 和 npm 是否安装成功：

   ```bash
   node -v
   npm -v
   ```

   如果安装成功，你应该会看到 Node.js 和 npm 的版本号。

### 2.2 初始化项目

1. **创建项目目录**：在终端中创建一个新的目录，并进入该目录：

   ```bash
   mkdir my-express-app
   cd my-express-app
   ```

2. **初始化 npm 项目**：在项目目录中运行以下命令来初始化 npm 项目：

   ```bash
   npm init -y
   ```

   这将生成一个 `package.json` 文件，用于管理项目的依赖和脚本。

3. **安装 Express.js**：使用 npm 安装 Express.js：

   ```bash
   npm install express
   ```

## 3. 创建第一个 Express.js 应用

### 3.1 创建主文件

在项目目录中创建一个名为 `app.js` 的文件，这是我们 Express.js 应用的主文件。

### 3.2 编写基本代码

在 `app.js` 文件中编写以下代码：

```javascript
// 引入 Express 模块
const express = require('express');

// 创建 Express 应用实例
const app = express();

// 定义一个简单的路由
app.get('/', (req, res) => {
  res.send('Hello, Express!');
});

// 启动服务器，监听端口 3000
const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`Server is running on http://localhost:${PORT}`);
});
```

### 3.3 运行应用

在终端中运行以下命令来启动应用：

```bash
node app.js
```

如果一切顺利，你应该会看到终端输出 `Server is running on http://localhost:3000`。

### 3.4 访问应用

打开浏览器，访问 `http://localhost:3000`，你应该会看到页面显示 `Hello, Express!`。

## 4. 路由基础

在 Express.js 中，路由用于定义如何响应客户端对特定端点的请求。路由由一个 HTTP 方法、一个路径和一个回调函数组成。

### 4.1 基本路由示例

```javascript
app.get('/about', (req, res) => {
  res.send('This is the about page.');
});
```

### 4.2 处理不同 HTTP 方法

Express.js 支持多种 HTTP 方法，如 `GET`、`POST`、`PUT`、`DELETE` 等。

```javascript
app.post('/submit', (req, res) => {
  res.send('Form submitted successfully.');
});
```

## 5. 中间件概念

中间件是 Express.js 的核心概念之一。它是一个函数，可以访问请求对象（`req`）、响应对象（`res`）以及下一个中间件函数（`next`）。

### 5.1 使用中间件

```javascript
app.use((req, res, next) => {
  console.log('Time:', Date.now());
  next();
});
```

### 5.2 中间件的顺序

中间件的顺序非常重要。中间件会按照它们被定义的顺序依次执行。

```javascript
app.use((req, res, next) => {
  console.log('First middleware');
  next();
});

app.use((req, res, next) => {
  console.log('Second middleware');
  next();
});
```

## 6. 请求和响应对象

在 Express.js 中，`req` 和 `res` 对象分别代表请求和响应。

### 6.1 请求对象 (`req`)

请求对象包含有关客户端请求的信息，如 URL、HTTP 头、请求体等。

```javascript
app.get('/user/:id', (req, res) => {
  res.send(`User ID: ${req.params.id}`);
});
```

### 6.2 响应对象 (`res`)

响应对象用于向客户端发送响应。

```javascript
app.get('/', (req, res) => {
  res.send('Hello, World!');
});
```

## 7. 实践练习

### 7.1 创建一个简单的博客应用

1. **创建路由**：为博客应用创建以下路由：
   - `GET /`：显示博客首页。
   - `GET /posts`：显示所有博客文章。
   - `GET /posts/:id`：显示特定 ID 的博客文章。

2. **使用中间件**：在每个请求中记录请求的时间。

3. **处理错误**：如果用户访问不存在的路由，返回一个 404 错误页面。

### 7.2 代码示例

```javascript
const express = require('express');
const app = express();

// 中间件：记录请求时间
app.use((req, res, next) => {
  console.log('Time:', Date.now());
  next();
});

// 路由：博客首页
app.get('/', (req, res) => {
  res.send('Welcome to the Blog!');
});

// 路由：所有博客文章
app.get('/posts', (req, res) => {
  res.send('List of all blog posts.');
});

// 路由：特定博客文章
app.get('/posts/:id', (req, res) => {
  res.send(`Blog post with ID: ${req.params.id}`);
});

// 处理 404 错误
app.use((req, res) => {
  res.status(404).send('Page not found');
});

// 启动服务器
const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
  console.log(`Server is running on http://localhost:${PORT}`);
});
```

## 8. 总结

通过本教程，你已经学会了如何创建一个简单的 Express.js 应用，并了解了路由、中间件、请求和响应对象的基本概念。接下来，你可以继续深入学习 Express.js 的其他高级功能，如模板引擎、数据库集成、身份认证等。

希望你能继续探索 Express.js 的强大功能，并将其应用于实际项目中！