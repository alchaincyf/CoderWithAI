---
title: API 文档生成 (Swagger) 教程
date: 2023-10-05
description: 本课程将教你如何使用Swagger工具生成和维护API文档，提升开发效率和文档质量。
slug: api-documentation-swagger
tags:
  - Swagger
  - API 文档
  - 开发工具
category: 编程工具
keywords:
  - Swagger
  - API 文档生成
  - 开发效率
---

# API 文档生成 (Swagger)

## 概述

在现代的Web开发中，API文档是不可或缺的一部分。它不仅帮助开发者理解API的功能和使用方法，还为前端开发者和第三方开发者提供了清晰的接口说明。Swagger是一个强大的工具，可以帮助我们自动生成API文档，并且支持实时交互式文档。

本教程将带你了解如何在Express.js应用中集成Swagger，并生成详细的API文档。

## 1. Swagger 简介

Swagger是一个开源的API文档生成工具，它通过解析API的代码注释或配置文件，自动生成API文档。Swagger提供了多种工具，包括Swagger UI、Swagger Editor和Swagger Codegen，帮助开发者更好地管理和使用API。

### 1.1 Swagger UI

Swagger UI是一个可视化的工具，它可以将API文档以网页的形式展示出来，并且支持实时交互。开发者可以通过Swagger UI直接测试API接口，查看请求和响应的详细信息。

### 1.2 Swagger Editor

Swagger Editor是一个在线编辑器，支持编写和预览Swagger文档。开发者可以在编辑器中编写API文档，并实时查看生成的API文档效果。

### 1.3 Swagger Codegen

Swagger Codegen是一个代码生成工具，它可以根据Swagger文档生成客户端SDK和服务端代码。开发者可以使用Swagger Codegen快速生成API的客户端和服务端代码。

## 2. 环境搭建

在开始之前，确保你已经安装了Node.js和npm。如果你还没有安装，可以从[Node.js官网](https://nodejs.org/)下载并安装。

### 2.1 创建Express.js应用

首先，创建一个新的Express.js应用：

```bash
mkdir my-express-app
cd my-express-app
npm init -y
npm install express
```

### 2.2 安装Swagger依赖

接下来，安装Swagger相关的依赖包：

```bash
npm install swagger-jsdoc swagger-ui-express
```

- `swagger-jsdoc`：用于解析代码中的JSDoc注释，并生成Swagger文档。
- `swagger-ui-express`：用于在Express.js应用中集成Swagger UI。

## 3. 配置Swagger

### 3.1 创建Swagger配置文件

在项目根目录下创建一个`swagger.js`文件，用于配置Swagger：

```javascript
const swaggerJsDoc = require('swagger-jsdoc');
const swaggerUi = require('swagger-ui-express');

const options = {
  definition: {
    openapi: '3.0.0',
    info: {
      title: 'My Express API',
      version: '1.0.0',
      description: 'A simple Express API with Swagger documentation',
    },
    servers: [
      {
        url: 'http://localhost:3000',
      },
    ],
  },
  apis: ['./routes/*.js'], // 指定包含API路由的文件
};

const swaggerSpec = swaggerJsDoc(options);

module.exports = (app) => {
  app.use('/api-docs', swaggerUi.serve, swaggerUi.setup(swaggerSpec));
};
```

### 3.2 集成Swagger到Express应用

在`app.js`文件中引入并使用Swagger配置：

```javascript
const express = require('express');
const swaggerConfig = require('./swagger');

const app = express();

// 使用Swagger配置
swaggerConfig(app);

app.listen(3000, () => {
  console.log('Server is running on http://localhost:3000');
});
```

## 4. 编写API路由和文档

### 4.1 创建API路由

在`routes`目录下创建一个`users.js`文件，定义一个简单的API路由：

```javascript
const express = require('express');
const router = express.Router();

/**
 * @swagger
 * /users:
 *   get:
 *     summary: 获取用户列表
 *     description: 返回所有用户的列表
 *     responses:
 *       200:
 *         description: 成功获取用户列表
 *         content:
 *           application/json:
 *             schema:
 *               type: array
 *               items:
 *                 type: object
 *                 properties:
 *                   id:
 *                     type: integer
 *                   name:
 *                     type: string
 */
router.get('/users', (req, res) => {
  const users = [
    { id: 1, name: 'Alice' },
    { id: 2, name: 'Bob' },
  ];
  res.json(users);
});

module.exports = router;
```

### 4.2 在主应用中引入路由

在`app.js`中引入并使用`users.js`路由：

```javascript
const express = require('express');
const swaggerConfig = require('./swagger');
const usersRouter = require('./routes/users');

const app = express();

// 使用Swagger配置
swaggerConfig(app);

// 使用用户路由
app.use('/api', usersRouter);

app.listen(3000, () => {
  console.log('Server is running on http://localhost:3000');
});
```

## 5. 运行应用并查看API文档

启动Express应用：

```bash
node app.js
```

打开浏览器，访问`http://localhost:3000/api-docs`，你将看到生成的Swagger API文档。

## 6. 实践练习

### 6.1 添加更多API路由

尝试在`routes`目录下添加更多的API路由，并为每个路由编写Swagger文档注释。例如，添加一个`POST /users`路由，用于创建新用户。

### 6.2 自定义Swagger UI

Swagger UI提供了丰富的自定义选项。你可以通过修改`swagger.js`文件中的`options`对象，自定义API文档的外观和行为。

## 7. 总结

通过本教程，你学会了如何在Express.js应用中集成Swagger，并生成详细的API文档。Swagger不仅提高了开发效率，还增强了API的可维护性和可读性。希望你能继续探索Swagger的更多功能，并在实际项目中应用这些知识。

## 8. 参考资料

- [Swagger官方文档](https://swagger.io/docs/)
- [swagger-jsdoc GitHub仓库](https://github.com/Surnet/swagger-jsdoc)
- [swagger-ui-express GitHub仓库](https://github.com/scottie1984/swagger-ui-express)

---

通过本教程，你已经掌握了如何在Express.js应用中使用Swagger生成API文档。继续探索和实践，你将能够更好地管理和维护你的API。