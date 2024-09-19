---
title: Express.js 框架入门
date: 2023-10-05
description: 本课程将带你从零开始学习Express.js框架，掌握如何使用它构建高效、可扩展的Node.js应用程序。
slug: expressjs-framework-introduction
tags:
  - Node.js
  - Express.js
  - Web开发
category: 后端开发
keywords:
  - Express.js教程
  - Node.js框架
  - Web应用开发
---

# Express.js 框架入门

## 1. 概述

Express.js 是一个基于 Node.js 的快速、开放且极简的 Web 框架。它为 Web 和移动应用提供了强大的功能，是构建 RESTful API 的首选框架之一。本教程将带你从零开始学习 Express.js，包括环境搭建、基本路由、中间件的使用以及如何创建一个简单的 Web 应用。

## 2. 环境搭建

### 2.1 安装 Node.js

首先，确保你已经安装了 Node.js。你可以通过以下命令检查是否已安装：

```bash
node -v
```

如果没有安装，请访问 [Node.js 官网](https://nodejs.org/) 下载并安装最新版本。

### 2.2 初始化项目

创建一个新的项目目录并初始化 npm：

```bash
mkdir my-express-app
cd my-express-app
npm init -y
```

### 2.3 安装 Express.js

使用 npm 安装 Express.js：

```bash
npm install express
```

## 3. 创建第一个 Express 应用

### 3.1 编写基本代码

在项目根目录下创建一个 `index.js` 文件，并编写以下代码：

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

### 3.2 运行应用

在终端中运行以下命令启动应用：

```bash
node index.js
```

打开浏览器并访问 `http://localhost:3000`，你应该会看到 "Hello World!" 的输出。

## 4. 路由和中间件

### 4.1 路由

路由用于确定应用如何响应客户端的请求。Express 提供了多种方法来定义路由，例如 `app.get`, `app.post`, `app.put`, `app.delete` 等。

```javascript
app.get('/about', (req, res) => {
  res.send('About page');
});

app.post('/submit', (req, res) => {
  res.send('Form submitted');
});
```

### 4.2 中间件

中间件是 Express 的核心概念之一。它是一个函数，可以访问请求对象 (`req`)、响应对象 (`res`) 以及下一个中间件函数 (`next`)。

```javascript
app.use((req, res, next) => {
  console.log('Time:', Date.now());
  next();
});
```

### 4.3 使用中间件处理请求

你可以使用中间件来处理请求，例如解析 JSON 数据：

```javascript
app.use(express.json());

app.post('/data', (req, res) => {
  console.log(req.body);
  res.send('Data received');
});
```

## 5. 模板引擎

Express 支持多种模板引擎，如 EJS、Pug 等。这里我们以 EJS 为例。

### 5.1 安装 EJS

```bash
npm install ejs
```

### 5.2 配置模板引擎

在 `index.js` 中配置 EJS：

```javascript
app.set('view engine', 'ejs');
app.set('views', './views');
```

### 5.3 创建视图文件

在项目根目录下创建一个 `views` 文件夹，并在其中创建一个 `index.ejs` 文件：

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Express with EJS</title>
</head>
<body>
  <h1>Hello, <%= name %>!</h1>
</body>
</html>
```

### 5.4 渲染视图

在路由中渲染视图：

```javascript
app.get('/greet/:name', (req, res) => {
  res.render('index', { name: req.params.name });
});
```

访问 `http://localhost:3000/greet/John`，你应该会看到 "Hello, John!" 的输出。

## 6. 实践练习

### 6.1 创建一个简单的博客系统

1. 创建一个 Express 应用。
2. 使用 EJS 作为模板引擎。
3. 创建路由来显示博客文章列表和单篇文章。
4. 使用中间件来处理请求数据。

### 6.2 代码示例

```javascript
const express = require('express');
const app = express();
const port = 3000;

app.set('view engine', 'ejs');
app.set('views', './views');

const posts = [
  { id: 1, title: 'First Post', content: 'This is the first post.' },
  { id: 2, title: 'Second Post', content: 'This is the second post.' }
];

app.get('/', (req, res) => {
  res.render('index', { posts });
});

app.get('/post/:id', (req, res) => {
  const post = posts.find(p => p.id === parseInt(req.params.id));
  res.render('post', { post });
});

app.listen(port, () => {
  console.log(`App listening at http://localhost:${port}`);
});
```

在 `views` 文件夹中创建 `index.ejs` 和 `post.ejs` 文件来渲染博客列表和单篇文章。

## 7. 总结

通过本教程，你已经学会了如何使用 Express.js 创建一个简单的 Web 应用，包括路由、中间件和模板引擎的使用。Express.js 是一个非常强大的框架，适合构建各种规模的 Web 应用。继续深入学习，你将能够构建更复杂的应用，并掌握更多的 Express.js 高级特性。

## 8. 下一步

- 学习如何使用 Express.js 构建 RESTful API。
- 探索更多的中间件和模板引擎。
- 深入了解 Express.js 的安全性和性能优化。

希望本教程对你有所帮助，祝你在 Express.js 的学习旅程中取得成功！