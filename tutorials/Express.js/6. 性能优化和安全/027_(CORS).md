---
title: 跨域资源共享 (CORS) 详解与实践
date: 2023-10-05
description: 本课程深入探讨跨域资源共享 (CORS) 的概念、原理及其实际应用，帮助开发者理解和解决跨域请求问题。
slug: cors-explained-and-practiced
tags:
  - CORS
  - 网络安全
  - 前端开发
category: 网络安全
keywords:
  - 跨域资源共享
  - CORS 原理
  - 跨域请求
---

# 跨域资源共享 (CORS) 教程

## 1. 什么是跨域资源共享 (CORS)？

跨域资源共享（Cross-Origin Resource Sharing，简称CORS）是一种机制，它使用额外的 HTTP 头来告诉浏览器，允许 Web 应用服务器进行跨域访问。当一个资源从与该资源本身所在的服务器不同的域、协议或端口请求资源时，就会发起一个跨域 HTTP 请求。

### 1.1 为什么需要 CORS？

浏览器出于安全考虑，实施了同源策略（Same-Origin Policy），这意味着一个网页只能请求与它同源的资源。同源指的是协议、域名和端口都相同。CORS 就是为了解决这个问题，允许服务器明确地告诉浏览器哪些跨域请求是被允许的。

## 2. CORS 的工作原理

CORS 通过在 HTTP 请求和响应中添加特定的头信息来工作。以下是 CORS 请求的基本流程：

1. **预检请求（Preflight Request）**：对于一些复杂的跨域请求（如带有自定义头、使用 PUT 或 DELETE 方法的请求），浏览器会先发送一个 OPTIONS 请求，称为预检请求。服务器在响应中返回允许的源、方法和头信息。
2. **简单请求（Simple Request）**：对于一些简单的跨域请求（如 GET、POST 请求，且没有自定义头），浏览器会直接发送请求，并在请求头中添加 `Origin` 字段。
3. **实际请求（Actual Request）**：如果预检请求通过，浏览器会发送实际的请求。

### 2.1 CORS 相关的 HTTP 头

- **请求头**：
  - `Origin`：表示请求的源。
  - `Access-Control-Request-Method`：预检请求中使用，表示实际请求将使用的方法。
  - `Access-Control-Request-Headers`：预检请求中使用，表示实际请求将使用的自定义头。

- **响应头**：
  - `Access-Control-Allow-Origin`：表示允许的源，可以是具体的源（如 `http://example.com`）或 `*`（表示允许所有源）。
  - `Access-Control-Allow-Methods`：表示允许的 HTTP 方法。
  - `Access-Control-Allow-Headers`：表示允许的自定义头。
  - `Access-Control-Allow-Credentials`：表示是否允许发送 Cookie。
  - `Access-Control-Max-Age`：表示预检请求的有效期。

## 3. 在 Express.js 中配置 CORS

在 Express.js 中，我们可以使用 `cors` 中间件来轻松配置 CORS。

### 3.1 安装 `cors` 中间件

首先，你需要安装 `cors` 中间件：

```bash
npm install cors
```

### 3.2 基本用法

在 Express 应用中使用 `cors` 中间件非常简单：

```javascript
const express = require('express');
const cors = require('cors');

const app = express();

// 使用 cors 中间件
app.use(cors());

app.get('/', (req, res) => {
  res.json({ message: 'Hello, CORS!' });
});

app.listen(3000, () => {
  console.log('Server is running on http://localhost:3000');
});
```

在这个例子中，`cors()` 中间件允许所有源访问你的 API。

### 3.3 配置 CORS 选项

你可以通过传递一个配置对象来定制 CORS 行为：

```javascript
const express = require('express');
const cors = require('cors');

const app = express();

const corsOptions = {
  origin: 'http://example.com', // 只允许来自 example.com 的请求
  methods: 'GET,POST', // 只允许 GET 和 POST 请求
  allowedHeaders: 'Content-Type,Authorization', // 只允许特定的头
  credentials: true, // 允许发送 Cookie
};

app.use(cors(corsOptions));

app.get('/', (req, res) => {
  res.json({ message: 'Hello, CORS!' });
});

app.listen(3000, () => {
  console.log('Server is running on http://localhost:3000');
});
```

### 3.4 动态配置 CORS

你还可以根据请求动态配置 CORS：

```javascript
const express = require('express');
const cors = require('cors');

const app = express();

const whitelist = ['http://example1.com', 'http://example2.com'];

const corsOptions = {
  origin: function (origin, callback) {
    if (whitelist.indexOf(origin) !== -1 || !origin) {
      callback(null, true);
    } else {
      callback(new Error('Not allowed by CORS'));
    }
  },
};

app.use(cors(corsOptions));

app.get('/', (req, res) => {
  res.json({ message: 'Hello, CORS!' });
});

app.listen(3000, () => {
  console.log('Server is running on http://localhost:3000');
});
```

在这个例子中，只有来自 `whitelist` 中的源的请求才会被允许。

## 4. 实践练习

### 4.1 创建一个简单的 Express 应用

1. 创建一个新的 Express 应用：

```bash
mkdir cors-demo
cd cors-demo
npm init -y
npm install express cors
```

2. 创建 `index.js` 文件，并添加以下代码：

```javascript
const express = require('express');
const cors = require('cors');

const app = express();

app.use(cors());

app.get('/', (req, res) => {
  res.json({ message: 'Hello, CORS!' });
});

app.listen(3000, () => {
  console.log('Server is running on http://localhost:3000');
});
```

3. 启动服务器：

```bash
node index.js
```

### 4.2 测试 CORS

1. 打开浏览器，访问 `http://localhost:3000`，你应该能看到 JSON 响应。
2. 现在，尝试从不同的域（如 `http://localhost:8080`）访问这个 API。你可以使用一个简单的 HTML 页面来测试：

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>CORS Test</title>
</head>
<body>
  <script>
    fetch('http://localhost:3000')
      .then(response => response.json())
      .then(data => console.log(data))
      .catch(error => console.error('Error:', error));
  </script>
</body>
</html>
```

3. 将这个 HTML 文件放在一个不同的端口（如 `http://localhost:8080`）上，并访问它。你应该能看到控制台输出 `{ message: 'Hello, CORS!' }`，这表明 CORS 配置成功。

## 5. 总结

跨域资源共享（CORS）是现代 Web 开发中不可或缺的一部分。通过使用 `cors` 中间件，你可以轻松地在 Express.js 应用中配置和管理 CORS。理解 CORS 的工作原理和配置选项，将帮助你构建更安全、更灵活的 Web 应用。

希望这篇教程能帮助你更好地理解和应用 CORS。继续探索和实践，你将掌握更多关于 Web 开发的知识！