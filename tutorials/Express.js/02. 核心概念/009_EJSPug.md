---
title: 模板引擎入门：EJS与Pug详解
date: 2023-10-05
description: 本课程详细介绍两种流行的模板引擎——EJS和Pug，帮助你理解如何使用它们来简化前端开发流程。
slug: template-engines-ejs-pug
tags:
  - 前端开发
  - 模板引擎
  - JavaScript
category: 前端开发
keywords:
  - EJS
  - Pug
  - 模板引擎
  - 前端开发
  - JavaScript
---

# 模板引擎 (EJS, Pug)

## 概述

在构建Web应用时，动态生成HTML页面是一个常见的需求。模板引擎允许我们在服务器端生成HTML内容，并将其发送给客户端。Express.js支持多种模板引擎，其中最常用的是EJS和Pug。本教程将详细介绍这两种模板引擎的使用方法。

## EJS (Embedded JavaScript)

### 什么是EJS？

EJS是一种简单的模板语言，允许你在HTML中嵌入JavaScript代码。EJS模板文件的后缀通常是`.ejs`。

### 安装EJS

首先，你需要安装EJS作为Express应用的依赖项：

```bash
npm install ejs
```

### 配置EJS

在Express应用中配置EJS非常简单。你只需要设置`view engine`为`ejs`，并指定模板文件的存放路径。

```javascript
const express = require('express');
const app = express();

// 设置模板引擎为EJS
app.set('view engine', 'ejs');

// 设置模板文件的存放路径
app.set('views', './views');

app.get('/', (req, res) => {
  res.render('index', { title: 'Hello EJS' });
});

app.listen(3000, () => {
  console.log('Server is running on http://localhost:3000');
});
```

### EJS语法

EJS支持多种语法，包括变量插入、条件语句、循环等。

#### 变量插入

```ejs
<h1><%= title %></h1>
```

#### 条件语句

```ejs
<% if (user) { %>
  <p>Welcome, <%= user.name %>!</p>
<% } else { %>
  <p>Please log in.</p>
<% } %>
```

#### 循环

```ejs
<ul>
  <% users.forEach(user => { %>
    <li><%= user.name %></li>
  <% }) %>
</ul>
```

### 实践练习

创建一个简单的Express应用，使用EJS模板引擎渲染一个包含用户列表的页面。

1. 创建一个名为`views`的文件夹，并在其中创建一个名为`index.ejs`的文件。
2. 在`index.ejs`中编写EJS代码，渲染一个包含用户列表的页面。
3. 在Express应用中配置EJS，并渲染`index.ejs`模板。

## Pug

### 什么是Pug？

Pug（原名Jade）是一种高性能的模板引擎，语法简洁，支持缩进和嵌套。Pug模板文件的后缀通常是`.pug`。

### 安装Pug

首先，你需要安装Pug作为Express应用的依赖项：

```bash
npm install pug
```

### 配置Pug

在Express应用中配置Pug与配置EJS类似：

```javascript
const express = require('express');
const app = express();

// 设置模板引擎为Pug
app.set('view engine', 'pug');

// 设置模板文件的存放路径
app.set('views', './views');

app.get('/', (req, res) => {
  res.render('index', { title: 'Hello Pug' });
});

app.listen(3000, () => {
  console.log('Server is running on http://localhost:3000');
});
```

### Pug语法

Pug的语法非常简洁，使用缩进来表示嵌套关系。

#### 变量插入

```pug
h1= title
```

#### 条件语句

```pug
if user
  p Welcome, #{user.name}!
else
  p Please log in.
```

#### 循环

```pug
ul
  each user in users
    li= user.name
```

### 实践练习

创建一个简单的Express应用，使用Pug模板引擎渲染一个包含用户列表的页面。

1. 创建一个名为`views`的文件夹，并在其中创建一个名为`index.pug`的文件。
2. 在`index.pug`中编写Pug代码，渲染一个包含用户列表的页面。
3. 在Express应用中配置Pug，并渲染`index.pug`模板。

## 总结

EJS和Pug都是强大的模板引擎，适用于不同的场景。EJS的语法更接近HTML，适合那些习惯于传统HTML的开发者。而Pug的语法简洁，适合那些喜欢简洁代码的开发者。通过本教程，你应该已经掌握了如何在Express应用中使用这两种模板引擎。

## 下一步

在掌握了模板引擎的基础知识后，你可以进一步学习如何在模板中使用布局、部分视图、以及如何处理表单数据等高级主题。此外，你还可以探索其他模板引擎，如Handlebars，以了解更多的选择。