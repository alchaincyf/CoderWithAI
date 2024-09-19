---
title: 模板引擎简介：从基础到高级
date: 2023-10-05
description: 本课程将带你深入了解模板引擎的基础知识，包括其工作原理、常见用法以及高级技巧，适合所有编程水平的开发者。
slug: introduction-to-template-engines
tags:
  - 模板引擎
  - 编程基础
  - 前端开发
category: 编程教程
keywords:
  - 模板引擎
  - 前端开发
  - 编程教程
---

# 模板引擎简介

## 1. 什么是模板引擎？

模板引擎是一种工具，用于将数据和模板结合起来生成最终的HTML文档。模板引擎允许开发者将页面的结构（HTML）和内容（数据）分离，从而提高代码的可维护性和可重用性。

### 1.1 模板引擎的作用

- **分离关注点**：将数据和视图分离，使代码更清晰。
- **提高可维护性**：修改模板或数据时，不会影响到另一部分。
- **代码复用**：可以在多个地方使用相同的模板。

## 2. 常见的模板引擎

### 2.1 EJS (Embedded JavaScript)

EJS 是一种简单的模板语言，允许你在 HTML 中嵌入 JavaScript 代码。

```html
<html>
<head>
    <title><%= title %></title>
</head>
<body>
    <h1><%= heading %></h1>
    <ul>
        <% items.forEach(function(item) { %>
            <li><%= item %></li>
        <% }); %>
    </ul>
</body>
</html>
```

### 2.2 Handlebars

Handlebars 是一种语法简洁的模板引擎，支持条件语句和循环。

```html
<html>
<head>
    <title>{{title}}</title>
</head>
<body>
    <h1>{{heading}}</h1>
    <ul>
        {{#each items}}
            <li>{{this}}</li>
        {{/each}}
    </ul>
</body>
</html>
```

### 2.3 Pug (原 Jade)

Pug 是一种缩进敏感的模板引擎，语法简洁，适合快速编写 HTML。

```pug
html
  head
    title= title
  body
    h1= heading
    ul
      each item in items
        li= item
```

## 3. 模板引擎的基本语法

### 3.1 变量插入

在模板中插入变量是最基本的操作。

```html
<p>Hello, <%= name %>!</p>
```

### 3.2 条件语句

根据条件显示不同的内容。

```html
<% if (isAdmin) { %>
    <p>Welcome, Admin!</p>
<% } else { %>
    <p>Welcome, User!</p>
<% } %>
```

### 3.3 循环

遍历数组或对象，生成多个元素。

```html
<ul>
    <% items.forEach(function(item) { %>
        <li><%= item %></li>
    <% }); %>
</ul>
```

## 4. 实践练习

### 4.1 创建一个简单的模板

使用 EJS 创建一个简单的模板，显示用户列表。

```html
<!-- users.ejs -->
<html>
<head>
    <title>User List</title>
</head>
<body>
    <h1>User List</h1>
    <ul>
        <% users.forEach(function(user) { %>
            <li><%= user.name %> - <%= user.email %></li>
        <% }); %>
    </ul>
</body>
</html>
```

### 4.2 渲染模板

在服务器端使用 Node.js 和 Express 渲染模板。

```javascript
const express = require('express');
const app = express();

app.set('view engine', 'ejs');

const users = [
    { name: 'Alice', email: 'alice@example.com' },
    { name: 'Bob', email: 'bob@example.com' },
];

app.get('/users', (req, res) => {
    res.render('users', { users: users });
});

app.listen(3000, () => {
    console.log('Server is running on http://localhost:3000');
});
```

## 5. 总结

模板引擎是现代 Web 开发中不可或缺的工具，它帮助我们将数据和视图分离，提高了代码的可维护性和可重用性。通过本教程，你应该已经掌握了模板引擎的基本概念、常见类型、基本语法以及如何使用它们来创建和渲染模板。

## 6. 进一步学习

- **深入学习 EJS**：探索 EJS 的更多高级功能，如自定义分隔符、局部模板等。
- **尝试其他模板引擎**：如 Handlebars、Pug 等，了解它们的独特之处。
- **结合前端框架**：将模板引擎与 React、Vue 等前端框架结合使用，提升开发效率。

通过不断实践和学习，你将能够更加熟练地使用模板引擎，提升你的 Web 开发技能。