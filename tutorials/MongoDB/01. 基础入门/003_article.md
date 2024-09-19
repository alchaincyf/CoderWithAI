---
title: 基本概念：数据库、集合、文档
date: 2023-10-05
description: 本课程介绍数据库、集合和文档的基本概念，帮助初学者理解NoSQL数据库的核心组件。
slug: basics-of-databases-collections-documents
tags:
  - 数据库
  - NoSQL
  - 编程基础
category: 编程基础
keywords:
  - 数据库
  - 集合
  - 文档
  - NoSQL
  - 编程基础
---

# 基本概念：数据库、集合、文档

## 1. 引言

在现代软件开发中，数据管理是至关重要的一环。MongoDB 是一种流行的 NoSQL 数据库，它以文档的形式存储数据，提供了灵活的数据模型和高性能的查询能力。本教程将带你深入了解 MongoDB 的基本概念：数据库、集合和文档。

## 2. 数据库（Database）

### 2.1 什么是数据库？

数据库是一个组织化的数据集合，用于存储、管理和检索数据。在 MongoDB 中，数据库是集合的容器。

### 2.2 创建数据库

在 MongoDB 中，你可以使用 `use` 命令来创建或切换到一个数据库。如果数据库不存在，MongoDB 会在你第一次插入数据时自动创建它。

```javascript
use myDatabase
```

### 2.3 查看当前数据库

你可以使用 `db` 命令来查看当前使用的数据库。

```javascript
db
```

### 2.4 查看所有数据库

使用 `show dbs` 命令可以列出所有数据库。

```javascript
show dbs
```

## 3. 集合（Collection）

### 3.1 什么是集合？

集合是 MongoDB 中存储文档的容器。它类似于关系型数据库中的表，但集合没有固定的结构，文档可以有不同的字段。

### 3.2 创建集合

在 MongoDB 中，集合会在你第一次插入文档时自动创建。你也可以使用 `db.createCollection()` 方法显式创建集合。

```javascript
db.createCollection("myCollection")
```

### 3.3 查看所有集合

使用 `show collections` 命令可以列出当前数据库中的所有集合。

```javascript
show collections
```

## 4. 文档（Document）

### 4.1 什么是文档？

文档是 MongoDB 中存储数据的基本单位。文档使用 BSON（Binary JSON）格式存储，这是一种二进制表示的 JSON 数据。

### 4.2 插入文档

你可以使用 `insertOne()` 或 `insertMany()` 方法向集合中插入文档。

```javascript
db.myCollection.insertOne({
    name: "Alice",
    age: 30,
    hobbies: ["reading", "swimming"]
})
```

### 4.3 查询文档

使用 `find()` 方法可以查询集合中的文档。

```javascript
db.myCollection.find({name: "Alice"})
```

### 4.4 更新文档

使用 `updateOne()` 或 `updateMany()` 方法可以更新集合中的文档。

```javascript
db.myCollection.updateOne(
    {name: "Alice"},
    {$set: {age: 31}}
)
```

### 4.5 删除文档

使用 `deleteOne()` 或 `deleteMany()` 方法可以删除集合中的文档。

```javascript
db.myCollection.deleteOne({name: "Alice"})
```

## 5. 实践练习

### 5.1 创建数据库和集合

1. 使用 `use` 命令创建一个名为 `practiceDB` 的数据库。
2. 使用 `createCollection` 方法创建一个名为 `users` 的集合。

### 5.2 插入文档

向 `users` 集合中插入以下文档：

```json
{
    "name": "Bob",
    "age": 25,
    "hobbies": ["coding", "gaming"]
}
```

### 5.3 查询文档

查询 `users` 集合中所有 `age` 大于 20 的文档。

### 5.4 更新文档

将 `users` 集合中 `name` 为 "Bob" 的文档的 `age` 更新为 26。

### 5.5 删除文档

删除 `users` 集合中 `name` 为 "Bob" 的文档。

## 6. 总结

通过本教程，你已经了解了 MongoDB 中的基本概念：数据库、集合和文档。你学会了如何创建数据库和集合，插入、查询、更新和删除文档。这些基本操作是使用 MongoDB 进行数据管理的基础。

在接下来的课程中，我们将深入探讨 MongoDB 的更多高级功能，如索引、聚合框架、事务处理等。继续学习，你将能够构建更复杂和强大的应用程序。