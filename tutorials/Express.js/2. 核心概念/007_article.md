---
title: 路由进阶：参数与查询字符串详解
date: 2023-10-05
description: 本课程深入探讨了在Web开发中如何使用路由参数和查询字符串来处理动态URL，提升应用的灵活性和用户体验。
slug: advanced-routing-parameters-query-strings
tags:
  - 路由
  - 参数
  - 查询字符串
category: Web开发
keywords:
  - 路由参数
  - 查询字符串
  - 动态URL
---

# 路由进阶：参数与查询字符串

在Express.js中，路由是处理客户端请求的关键部分。在前面的教程中，我们已经学习了如何使用基本的HTTP方法（如GET、POST）来处理请求。本教程将进一步探讨如何在路由中使用参数和查询字符串来处理更复杂的请求。

## 1. 路由参数

路由参数允许我们在URL中嵌入动态值，这些值可以在处理请求时被提取出来。例如，我们可能有一个博客应用，需要根据文章的ID来获取特定的文章。

### 1.1 定义路由参数

在Express中，我们可以通过在路由路径中使用冒号（`:`）来定义路由参数。例如：

```javascript
const express = require('express');
const app = express();

app.get('/articles/:id', (req, res) => {
    const articleId = req.params.id;
    res.send(`You requested article with ID: ${articleId}`);
});

app.listen(3000, () => {
    console.log('Server is running on port 3000');
});
```

在这个例子中，`/articles/:id`路径中的`:id`是一个路由参数。当客户端请求`/articles/123`时，`id`的值将被设置为`123`，并且可以通过`req.params.id`访问。

### 1.2 多个路由参数

你可以在同一个路由中定义多个参数。例如：

```javascript
app.get('/articles/:category/:id', (req, res) => {
    const category = req.params.category;
    const articleId = req.params.id;
    res.send(`You requested article with ID: ${articleId} in category: ${category}`);
});
```

在这个例子中，`/articles/tech/456`将匹配这个路由，`category`将被设置为`tech`，`id`将被设置为`456`。

## 2. 查询字符串

查询字符串是URL中的一部分，通常用于传递额外的参数。查询字符串以问号（`?`）开头，参数之间用`&`分隔，每个参数的形式为`key=value`。

### 2.1 获取查询字符串参数

在Express中，查询字符串参数可以通过`req.query`对象来访问。例如：

```javascript
app.get('/search', (req, res) => {
    const query = req.query.q;
    const page = req.query.page || 1;
    res.send(`You searched for: ${query} on page: ${page}`);
});
```

在这个例子中，如果客户端请求`/search?q=express&page=2`，`req.query.q`将被设置为`express`，`req.query.page`将被设置为`2`。如果`page`参数没有提供，默认值为`1`。

### 2.2 处理多个查询参数

查询字符串可以包含多个参数，所有参数都可以通过`req.query`对象访问。例如：

```javascript
app.get('/filter', (req, res) => {
    const category = req.query.category;
    const price = req.query.price;
    res.send(`Filtering by category: ${category} and price: ${price}`);
});
```

在这个例子中，如果客户端请求`/filter?category=books&price=100`，`req.query.category`将被设置为`books`，`req.query.price`将被设置为`100`。

## 3. 实践练习

### 3.1 练习1：获取用户信息

创建一个Express应用，该应用有一个路由`/user/:id`，用于获取用户信息。用户信息可以通过查询字符串传递，例如`/user/123?name=John&age=30`。服务器应返回用户ID、姓名和年龄。

```javascript
const express = require('express');
const app = express();

app.get('/user/:id', (req, res) => {
    const userId = req.params.id;
    const name = req.query.name;
    const age = req.query.age;
    res.send(`User ID: ${userId}, Name: ${name}, Age: ${age}`);
});

app.listen(3000, () => {
    console.log('Server is running on port 3000');
});
```

### 3.2 练习2：搜索文章

创建一个Express应用，该应用有一个路由`/search`，用于搜索文章。搜索条件可以通过查询字符串传递，例如`/search?title=Express&author=John`。服务器应返回搜索结果。

```javascript
const express = require('express');
const app = express();

app.get('/search', (req, res) => {
    const title = req.query.title;
    const author = req.query.author;
    res.send(`Searching for articles with title: ${title} and author: ${author}`);
});

app.listen(3000, () => {
    console.log('Server is running on port 3000');
});
```

## 4. 总结

在本教程中，我们学习了如何在Express.js中使用路由参数和查询字符串来处理更复杂的请求。路由参数允许我们在URL中嵌入动态值，而查询字符串则允许我们在URL中传递额外的参数。通过结合使用这两种技术，我们可以构建更加灵活和强大的Web应用。

希望本教程对你有所帮助！如果你有任何问题或需要进一步的帮助，请随时提问。