---
title: 深入学习模板引擎：EJS与Pug
date: 2023-10-05
description: 本课程将带你深入了解两种流行的模板引擎——EJS和Pug，学习如何使用它们来构建动态网页和提高开发效率。
slug: template-engines-ejs-pug
tags:
  - 模板引擎
  - EJS
  - Pug
category: 前端开发
keywords:
  - 模板引擎
  - EJS教程
  - Pug教程
  - 前端开发
  - 动态网页
---

# 模板引擎（EJS, Pug）

## 概述

在Web开发中，模板引擎用于将数据和HTML模板结合起来，生成动态的HTML页面。Node.js 生态系统中有多种模板引擎，其中最常用的是 EJS 和 Pug。本教程将详细介绍这两种模板引擎的基本概念、使用方法以及如何在 Express.js 中集成它们。

## EJS 模板引擎

### 什么是 EJS？

EJS（Embedded JavaScript）是一种简单的模板语言，允许你在HTML中嵌入JavaScript代码。EJS 模板文件通常以 `.ejs` 为扩展名。

### 安装 EJS

首先，你需要安装 EJS 模块。你可以通过 npm 来安装：

```bash
npm install ejs
```

### 使用 EJS 渲染模板

在 Express.js 中使用 EJS 非常简单。首先，你需要设置 Express.js 使用 EJS 作为模板引擎，并指定模板文件的存放路径。

```javascript
const express = require('express');
const app = express();

// 设置模板引擎为 EJS
app.set('view engine', 'ejs');

// 设置模板文件的存放路径
app.set('views', './views');

// 示例路由
app.get('/', (req, res) => {
    // 渲染 index.ejs 模板，并传递数据
    res.render('index', { title: 'EJS 示例', message: '欢迎使用 EJS 模板引擎！' });
});

app.listen(3000, () => {
    console.log('服务器已启动，访问 http://localhost:3000');
});
```

### EJS 模板示例

创建一个名为 `index.ejs` 的文件，放在 `views` 目录下：

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title><%= title %></title>
</head>
<body>
    <h1><%= message %></h1>
</body>
</html>
```

在这个模板中，`<%= %>` 用于输出变量的值。

### 实践练习

1. 创建一个新的 Express.js 应用。
2. 安装 EJS 并设置为模板引擎。
3. 创建一个 EJS 模板文件，并在其中显示动态数据。

## Pug 模板引擎

### 什么是 Pug？

Pug（以前称为 Jade）是一种高性能的模板引擎，使用缩进来定义HTML结构。Pug 模板文件通常以 `.pug` 为扩展名。

### 安装 Pug

你可以通过 npm 来安装 Pug：

```bash
npm install pug
```

### 使用 Pug 渲染模板

在 Express.js 中使用 Pug 也非常简单。首先，你需要设置 Express.js 使用 Pug 作为模板引擎，并指定模板文件的存放路径。

```javascript
const express = require('express');
const app = express();

// 设置模板引擎为 Pug
app.set('view engine', 'pug');

// 设置模板文件的存放路径
app.set('views', './views');

// 示例路由
app.get('/', (req, res) => {
    // 渲染 index.pug 模板，并传递数据
    res.render('index', { title: 'Pug 示例', message: '欢迎使用 Pug 模板引擎！' });
});

app.listen(3000, () => {
    console.log('服务器已启动，访问 http://localhost:3000');
});
```

### Pug 模板示例

创建一个名为 `index.pug` 的文件，放在 `views` 目录下：

```pug
doctype html
html(lang="en")
  head
    meta(charset="UTF-8")
    meta(name="viewport" content="width=device-width, initial-scale=1.0")
    title= title
  body
    h1= message
```

在这个模板中，`=` 用于输出变量的值。

### 实践练习

1. 创建一个新的 Express.js 应用。
2. 安装 Pug 并设置为模板引擎。
3. 创建一个 Pug 模板文件，并在其中显示动态数据。

## 比较 EJS 和 Pug

### EJS 的优点

- **简单易学**：EJS 的语法非常接近 HTML，容易上手。
- **灵活性**：你可以在 EJS 模板中直接使用 JavaScript 代码。

### Pug 的优点

- **简洁**：Pug 使用缩进和简洁的语法，减少了代码量。
- **高性能**：Pug 的编译速度较快，适合大型项目。

### 选择合适的模板引擎

选择 EJS 还是 Pug 取决于你的项目需求和个人偏好。如果你更喜欢传统的 HTML 语法，EJS 可能更适合你。如果你喜欢简洁的语法和更高的性能，Pug 可能是更好的选择。

## 总结

本教程介绍了如何在 Node.js 和 Express.js 中使用 EJS 和 Pug 模板引擎。通过学习这两种模板引擎，你可以轻松地创建动态的 HTML 页面。希望你能通过实践练习，进一步掌握这些模板引擎的使用。

## 下一步

接下来，你可以继续学习如何在 Express.js 中实现 RESTful API，或者深入了解数据库操作和 ORM/ODM 的使用。