---
title: GraphQL 基础教程
date: 2023-10-05
description: 本课程将带你从零开始学习GraphQL，涵盖基础概念、查询、突变和订阅，帮助你掌握这一现代API查询语言。
slug: graphql-basics
tags:
  - GraphQL
  - API
  - 编程基础
category: 编程教程
keywords:
  - GraphQL 基础
  - API 查询语言
  - 现代API
---

# GraphQL 基础

## 1. 什么是 GraphQL？

GraphQL 是一种用于 API 的查询语言，它允许客户端精确地请求所需的数据，而不是服务器返回固定的数据结构。GraphQL 由 Facebook 开发，并于 2015 年开源。与传统的 REST API 相比，GraphQL 提供了更灵活、高效的数据获取方式。

### 1.1 GraphQL 与 REST 的区别

- **REST API**: 服务器定义了多个端点（endpoints），每个端点返回固定的数据结构。客户端必须请求多个端点以获取所需的所有数据。
- **GraphQL**: 客户端通过一个单一的端点发送查询请求，查询中定义了所需的数据结构。服务器根据查询返回精确的数据。

## 2. GraphQL 的核心概念

### 2.1 查询（Query）

查询是客户端向服务器请求数据的指令。查询定义了客户端需要的数据结构。

```graphql
query {
  user(id: "1") {
    name
    email
  }
}
```

### 2.2 变异（Mutation）

变异用于修改服务器上的数据，例如创建、更新或删除数据。

```graphql
mutation {
  createUser(name: "John Doe", email: "john@example.com") {
    id
    name
    email
  }
}
```

### 2.3 订阅（Subscription）

订阅用于实时获取数据更新。当服务器上的数据发生变化时，客户端会自动接收到更新。

```graphql
subscription {
  userCreated {
    id
    name
    email
  }
}
```

## 3. GraphQL 的类型系统

GraphQL 使用强类型系统来定义数据结构。常见的类型包括：

- **标量类型**: `Int`, `Float`, `String`, `Boolean`, `ID`
- **对象类型**: 包含多个字段的对象
- **接口**: 定义一组字段，多个对象类型可以实现该接口
- **联合类型**: 表示多个对象类型的联合

```graphql
type User {
  id: ID!
  name: String!
  email: String!
}

type Query {
  user(id: ID!): User
}
```

## 4. 实践练习：创建一个简单的 GraphQL 服务器

### 4.1 安装依赖

首先，我们需要安装 `express` 和 `express-graphql` 包。

```bash
npm install express express-graphql graphql
```

### 4.2 创建服务器

创建一个简单的 Express 服务器，并使用 `express-graphql` 中间件来处理 GraphQL 请求。

```javascript
const express = require('express');
const { graphqlHTTP } = require('express-graphql');
const { buildSchema } = require('graphql');

// 定义 GraphQL 模式
const schema = buildSchema(`
  type User {
    id: ID!
    name: String!
    email: String!
  }

  type Query {
    user(id: ID!): User
  }
`);

// 模拟用户数据
const users = [
  { id: '1', name: 'John Doe', email: 'john@example.com' },
  { id: '2', name: 'Jane Doe', email: 'jane@example.com' },
];

// 定义解析器
const root = {
  user: ({ id }) => users.find(user => user.id === id),
};

// 创建 Express 服务器
const app = express();
app.use('/graphql', graphqlHTTP({
  schema: schema,
  rootValue: root,
  graphiql: true, // 启用 GraphiQL 工具
}));

app.listen(4000, () => {
  console.log('GraphQL server running at http://localhost:4000/graphql');
});
```

### 4.3 运行服务器

启动服务器：

```bash
node server.js
```

访问 `http://localhost:4000/graphql`，你将看到 GraphiQL 工具，可以在其中编写和测试 GraphQL 查询。

## 5. 实践练习：编写 GraphQL 查询

在 GraphiQL 中编写以下查询：

```graphql
query {
  user(id: "1") {
    name
    email
  }
}
```

你应该会看到以下响应：

```json
{
  "data": {
    "user": {
      "name": "John Doe",
      "email": "john@example.com"
    }
  }
}
```

## 6. 总结

GraphQL 提供了一种更灵活、高效的数据获取方式，允许客户端精确地请求所需的数据。通过本教程，你已经学习了 GraphQL 的核心概念，并创建了一个简单的 GraphQL 服务器。继续探索 GraphQL 的更多功能，如变异和订阅，以及如何在实际项目中应用它。

## 7. 进一步学习资源

- [GraphQL 官方文档](https://graphql.org/learn/)
- [Apollo GraphQL](https://www.apollographql.com/)
- [GraphQL 社区资源](https://graphql.org/community/)

通过这些资源，你可以深入学习 GraphQL 的高级功能和最佳实践。