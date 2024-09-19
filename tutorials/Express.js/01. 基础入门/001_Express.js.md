---
title: Express.js 简介和特点
date: 2023-10-05
description: 本课程将介绍Express.js的基本概念、特点及其在Web开发中的应用，帮助初学者快速上手。
slug: expressjs-introduction-and-features
tags:
  - Express.js
  - Node.js
  - Web开发
category: 后端开发
keywords:
  - Express.js 简介
  - Express.js 特点
  - Node.js框架
---

# Express.js 简介和特点

## 1. 什么是 Express.js？

Express.js 是一个基于 Node.js 的快速、开放且极简的 Web 框架。它为构建 Web 应用和 API 提供了强大的功能和灵活性。Express.js 是 Node.js 生态系统中最流行的框架之一，广泛应用于各种规模的 Web 项目。

### 1.1 Express.js 的特点

- **极简主义**：Express.js 的设计理念是保持简单和灵活，不强制开发者使用特定的结构或模式。
- **中间件**：Express.js 通过中间件机制来处理请求和响应，使得代码组织更加模块化。
- **路由**：Express.js 提供了强大的路由功能，可以轻松处理不同 URL 和 HTTP 方法的请求。
- **模板引擎支持**：Express.js 支持多种模板引擎，如 EJS、Pug 等，方便生成动态 HTML 页面。
- **错误处理**：Express.js 提供了内置的错误处理机制，可以方便地捕获和处理错误。
- **社区支持**：Express.js 拥有庞大的社区和丰富的第三方插件，可以轻松扩展功能。

## 2. 环境搭建

在开始使用 Express.js 之前，我们需要先搭建好开发环境。

### 2.1 安装 Node.js 和 npm

Express.js 是基于 Node.js 的，因此首先需要安装 Node.js。Node.js 自带 npm（Node Package Manager），用于管理项目依赖。

1. **下载 Node.js**：访问 [Node.js 官网](https://nodejs.org/)，下载并安装适合你操作系统的 Node.js 版本。
2. **验证安装**：安装完成后，打开终端（命令行），输入以下命令验证安装是否成功：

   ```bash
   node -v
   npm -v
   ```

   如果安装成功，会显示 Node.js 和 npm 的版本号。

### 2.2 创建项目目录

在终端中创建一个新的项目目录，并进入该目录：

```bash
mkdir my-express-app
cd my-express-app
```

### 2.3 初始化项目

使用 npm 初始化项目，生成 `package.json` 文件：

```bash
npm init -y
```

### 2.4 安装 Express.js

在项目目录中安装 Express.js：

```bash
npm install express
```

## 3. 创建第一个 Express.js 应用

现在我们已经搭建好了开发环境，接下来创建第一个 Express.js 应用。

### 3.1 创建主文件

在项目目录中创建一个名为 `app.js` 的文件，这是我们应用的主文件。

```javascript
// app.js
const express = require('express');
const app = express();
const port = 3000;

app.get('/', (req, res) => {
  res.send('Hello, Express.js!');
});

app.listen(port, () => {
  console.log(`Server is running on http://localhost:${port}`);
});
```

### 3.2 运行应用

在终端中运行以下命令启动应用：

```bash
node app.js
```

打开浏览器，访问 `http://localhost:3000`，你应该会看到页面显示 "Hello, Express.js!"。

## 4. 路由基础

路由是 Express.js 的核心功能之一，用于处理不同 URL 和 HTTP 方法的请求。

### 4.1 基本路由

在 Express.js 中，可以使用 `app.METHOD(path, callback)` 来定义路由，其中 `METHOD` 是 HTTP 方法（如 `get`, `post`, `put`, `delete` 等），`path` 是 URL 路径，`callback` 是处理请求的函数。

```javascript
app.get('/about', (req, res) => {
  res.send('This is the about page.');
});

app.post('/submit', (req, res) => {
  res.send('Form submitted successfully.');
});
```

### 4.2 路由参数

路由参数允许你在 URL 中传递动态值。例如，`/users/:id` 中的 `:id` 就是一个路由参数。

```javascript
app.get('/users/:id', (req, res) => {
  res.send(`User ID: ${req.params.id}`);
});
```

### 4.3 查询字符串

查询字符串是 URL 中 `?` 后面的部分，用于传递额外的参数。

```javascript
app.get('/search', (req, res) => {
  res.send(`Search query: ${req.query.q}`);
});
```

## 5. 中间件概念

中间件是 Express.js 的核心概念之一，用于处理请求和响应之间的逻辑。

### 5.1 什么是中间件？

中间件是一个函数，它可以访问请求对象（`req`）、响应对象（`res`）以及下一个中间件函数（`next`）。中间件可以执行以下任务：

- 执行任何代码。
- 修改请求和响应对象。
- 结束请求-响应循环。
- 调用堆栈中的下一个中间件。

### 5.2 使用中间件

```javascript
app.use((req, res, next) => {
  console.log('Time:', Date.now());
  next();
});

app.get('/', (req, res) => {
  res.send('Hello, Express.js!');
});
```

在这个例子中，`app.use` 定义了一个中间件，它会在每个请求到达时记录当前时间。

## 6. 请求和响应对象

在 Express.js 中，请求对象（`req`）和响应对象（`res`）是处理 HTTP 请求和响应的核心对象。

### 6.1 请求对象（`req`）

请求对象包含客户端发送的请求信息，如 URL、HTTP 头、请求体等。

```javascript
app.get('/user', (req, res) => {
  console.log(req.headers); // 获取请求头
  console.log(req.query);   // 获取查询字符串
  console.log(req.params);  // 获取路由参数
  res.send('User information');
});
```

### 6.2 响应对象（`res`）

响应对象用于向客户端发送响应，可以设置状态码、响应头、响应体等。

```javascript
app.get('/data', (req, res) => {
  res.status(200).json({ message: 'Data fetched successfully' });
});
```

## 7. 静态文件服务

Express.js 提供了内置的静态文件服务功能，可以方便地提供静态资源（如 HTML、CSS、JavaScript、图片等）。

### 7.1 使用静态文件服务

```javascript
app.use(express.static('public'));
```

假设 `public` 目录中有一个 `index.html` 文件，访问 `http://localhost:3000/index.html` 即可访问该文件。

## 8. 模板引擎

模板引擎用于生成动态 HTML 页面。Express.js 支持多种模板引擎，如 EJS、Pug 等。

### 8.1 使用 EJS 模板引擎

1. 安装 EJS：

   ```bash
   npm install ejs
   ```

2. 配置模板引擎：

   ```javascript
   app.set('view engine', 'ejs');
   app.set('views', './views');
   ```

3. 创建视图文件：

   在 `views` 目录下创建一个 `index.ejs` 文件：

   ```html
   <!DOCTYPE html>
   <html>
   <head>
     <title>EJS Example</title>
   </head>
   <body>
     <h1>Hello, <%= name %>!</h1>
   </body>
   </html>
   ```

4. 渲染视图：

   ```javascript
   app.get('/', (req, res) => {
     res.render('index', { name: 'Express' });
   });
   ```

## 9. 错误处理

Express.js 提供了内置的错误处理机制，可以方便地捕获和处理错误。

### 9.1 错误处理中间件

```javascript
app.use((err, req, res, next) => {
  console.error(err.stack);
  res.status(500).send('Something broke!');
});
```

### 9.2 捕获错误

```javascript
app.get('/error', (req, res) => {
  throw new Error('This is a test error');
});
```

## 10. 常用中间件介绍

Express.js 提供了许多内置中间件，同时也支持第三方中间件。

### 10.1 内置中间件

- `express.json()`：解析 JSON 格式的请求体。
- `express.urlencoded()`：解析 URL 编码的请求体。
- `express.static()`：提供静态文件服务。

### 10.2 第三方中间件

- `morgan`：HTTP 请求日志记录中间件。
- `body-parser`：解析请求体。
- `cors`：处理跨域资源共享。

## 11. 自定义中间件开发

你可以编写自定义中间件来处理特定的业务逻辑。

### 11.1 自定义中间件示例

```javascript
const loggerMiddleware = (req, res, next) => {
  console.log(`${req.method} ${req.url}`);
  next();
};

app.use(loggerMiddleware);
```

## 12. 第三方中间件集成

集成第三方中间件可以大大扩展 Express.js 的功能。

### 12.1 使用 `morgan` 记录日志

1. 安装 `morgan`：

   ```bash
   npm install morgan
   ```

2. 使用 `morgan`：

   ```javascript
   const morgan = require('morgan');
   app.use(morgan('dev'));
   ```

## 13. 身份认证中间件

身份认证是 Web 应用中的重要功能，可以使用第三方中间件来实现。

### 13.1 使用 `passport` 进行身份认证

1. 安装 `passport`：

   ```bash
   npm install passport
   ```

2. 配置 `passport`：

   ```javascript
   const passport = require('passport');
   const LocalStrategy = require('passport-local').Strategy;

   passport.use(new LocalStrategy(
     (username, password, done) => {
       // 验证逻辑
       if (username === 'admin' && password === 'password') {
         return done(null, { id: 1, username: 'admin' });
       } else {
         return done(null, false, { message: 'Incorrect credentials.' });
       }
     }
   ));

   app.use(passport.initialize());
   app.use(passport.session());
   ```

## 14. 文件上传处理

文件上传是 Web 应用中常见的功能，可以使用第三方中间件来处理。

### 14.1 使用 `multer` 处理文件上传

1. 安装 `multer`：

   ```bash
   npm install multer
   ```

2. 使用 `multer`：

   ```javascript
   const multer = require('multer');
   const upload = multer({ dest: 'uploads/' });

   app.post('/upload', upload.single('file'), (req, res) => {
     res.send('File uploaded successfully.');
   });
   ```

## 15. MongoDB 与 Mongoose

MongoDB 是一个流行的 NoSQL 数据库，Mongoose 是 MongoDB 的 Node.js 驱动程序。

### 15.1 安装 Mongoose

```bash
npm install mongoose
```

### 15.2 连接 MongoDB

```javascript
const mongoose = require('mongoose');
mongoose.connect('mongodb://localhost:27017/mydatabase', { useNewUrlParser: true, useUnifiedTopology: true });
```

### 15.3 定义模型

```javascript
const userSchema = new mongoose.Schema({
  name: String,
  email: String
});

const User = mongoose.model('User', userSchema);
```

### 15.4 操作数据库

```javascript
app.get('/users', async (req, res) => {
  const users = await User.find();
  res.json(users);
});
```

## 16. SQL 数据库 (MySQL, PostgreSQL)

除了 MongoDB，Express.js 也支持 SQL 数据库，如 MySQL 和 PostgreSQL。

### 16.1 安装 `mysql` 或 `pg`

```bash
npm install mysql
npm install pg
```

### 16.2 连接数据库

```javascript
const mysql = require('mysql');
const connection = mysql.createConnection({
  host: 'localhost',
  user: 'root',
  password: 'password',
  database: 'mydatabase'
});

connection.connect();
```

### 16.3 执行查询

```javascript
app.get('/users', (req, res) => {
  connection.query('SELECT * FROM users', (error, results) => {
    if (error) throw error;
    res.json(results);
  });
});
```

## 17. ORM 工具 (Sequelize)

Sequelize 是一个流行的 ORM 工具，支持多种 SQL 数据库。

### 17.1 安装 Sequelize

```bash
npm install sequelize
npm install mysql2
```

### 17.2 初始化 Sequelize

```javascript
const { Sequelize, DataTypes } = require('sequelize');
const sequelize = new Sequelize('mydatabase', 'root', 'password', {
  host: 'localhost',
  dialect: 'mysql'
});
```

### 17.3 定义模型

```javascript
const User = sequelize.define('User', {
  name: DataTypes.STRING,
  email: DataTypes.STRING
});
```

### 17.4 操作数据库

```javascript
app.get('/users', async (req, res) => {
  const users = await User.findAll();
  res.json(users);
});
```

## 18. 数据验证和安全性

数据验证和安全性是 Web 应用开发中的重要环节。

### 18.1 使用 `express-validator` 进行数据验证

1. 安装 `express-validator`：

   ```bash
   npm install express-validator
   ```

2. 使用 `express-validator`：

   ```javascript
   const { body, validationResult } = require('express-validator');

   app.post('/user', [
     body('name').isLength({ min: 3 }),
     body('email').isEmail()
   ], (req, res) => {
     const errors = validationResult(req);
     if (!errors.isEmpty()) {
       return res.status(400).json({ errors: errors.array() });
     }
     res.send('User created successfully.');
   });
   ```

### 18.2 安全性最佳实践

- 使用 HTTPS 保护数据传输。
- 防止 SQL 注入和 XSS 攻击。
- 使用安全的密码存储（如 bcrypt）。

## 19. RESTful 设计原则

RESTful API 是一种基于 HTTP 协议的 API 设计风格。

### 19.1 RESTful 路由设计

```javascript
app.get('/users', (req, res) => {
  res.json(users);
});

app.get('/users/:id', (req, res) => {
  const user = users.find(u => u.id === parseInt(req.params.id));
  res.json(user);
});

app.post('/users', (req, res) => {
  const newUser = { id: users.length + 1, ...req.body };
  users.push(newUser);
  res.status(201).json(newUser);
});

app.put('/users/:id', (req, res) => {
  const user = users.find(u => u.id === parseInt(req.params.id));
  Object.assign(user, req.body);
  res.json(user);
});

app.delete('/users/:id', (req, res) => {
  const index = users.findIndex(u => u.id === parseInt(req.params.id));
  users.splice(index, 1);
  res.status(204).send();
});
```

## 20. API 版本控制