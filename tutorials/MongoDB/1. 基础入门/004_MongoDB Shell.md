---
title: MongoDB Shell 基础操作教程
date: 2023-10-05
description: 本课程详细介绍MongoDB Shell的基础操作，包括数据库和集合的创建、文档的插入、查询、更新和删除等核心功能。
slug: mongodb-shell-basic-operations
tags:
  - MongoDB
  - NoSQL
  - 数据库操作
category: 数据库
keywords:
  - MongoDB Shell
  - 数据库操作
  - NoSQL
---

# MongoDB Shell 基础操作

## 概述

MongoDB Shell（简称 `mongosh`）是 MongoDB 的交互式 JavaScript 接口，允许用户直接与 MongoDB 数据库进行交互。通过 MongoDB Shell，用户可以执行各种数据库操作，如查询、插入、更新和删除数据。本教程将带你了解 MongoDB Shell 的基础操作，适合初学者学习。

## 1. 启动 MongoDB Shell

### 1.1 启动 MongoDB 服务

在启动 MongoDB Shell 之前，确保 MongoDB 服务已经启动。你可以通过以下命令启动 MongoDB 服务：

```bash
mongod
```

### 1.2 启动 MongoDB Shell

在 MongoDB 服务启动后，你可以通过以下命令启动 MongoDB Shell：

```bash
mongosh
```

启动后，你会看到类似以下的提示符：

```bash
test>
```

这表示你已经成功进入 MongoDB Shell，并且默认连接到 `test` 数据库。

## 2. 数据库操作

### 2.1 查看当前数据库

你可以使用 `db` 命令查看当前连接的数据库：

```javascript
test> db
test
```

### 2.2 切换数据库

使用 `use` 命令可以切换到指定的数据库。如果数据库不存在，MongoDB 会在你第一次插入数据时自动创建该数据库。

```javascript
test> use myNewDatabase
switched to db myNewDatabase
```

### 2.3 查看所有数据库

使用 `show dbs` 命令可以查看所有数据库：

```javascript
test> show dbs
admin   0.000GB
config  0.000GB
local   0.000GB
```

## 3. 集合操作

### 3.1 查看当前数据库中的集合

使用 `show collections` 命令可以查看当前数据库中的所有集合：

```javascript
myNewDatabase> show collections
```

### 3.2 创建集合

MongoDB 不会强制要求你显式创建集合。当你第一次插入文档时，MongoDB 会自动创建集合。例如：

```javascript
myNewDatabase> db.myCollection.insertOne({ name: "Alice", age: 30 })
```

### 3.3 删除集合

使用 `drop()` 方法可以删除指定的集合：

```javascript
myNewDatabase> db.myCollection.drop()
true
```

## 4. 文档操作

### 4.1 插入文档

使用 `insertOne()` 方法可以插入单个文档：

```javascript
myNewDatabase> db.myCollection.insertOne({ name: "Bob", age: 25 })
```

使用 `insertMany()` 方法可以插入多个文档：

```javascript
myNewDatabase> db.myCollection.insertMany([
  { name: "Charlie", age: 35 },
  { name: "David", age: 40 }
])
```

### 4.2 查询文档

使用 `find()` 方法可以查询集合中的文档。例如，查询所有文档：

```javascript
myNewDatabase> db.myCollection.find()
```

查询特定条件的文档：

```javascript
myNewDatabase> db.myCollection.find({ age: { $gt: 30 } })
```

### 4.3 更新文档

使用 `updateOne()` 方法可以更新单个文档：

```javascript
myNewDatabase> db.myCollection.updateOne(
  { name: "Alice" },
  { $set: { age: 31 } }
)
```

使用 `updateMany()` 方法可以更新多个文档：

```javascript
myNewDatabase> db.myCollection.updateMany(
  { age: { $lt: 35 } },
  { $inc: { age: 1 } }
)
```

### 4.4 删除文档

使用 `deleteOne()` 方法可以删除单个文档：

```javascript
myNewDatabase> db.myCollection.deleteOne({ name: "Bob" })
```

使用 `deleteMany()` 方法可以删除多个文档：

```javascript
myNewDatabase> db.myCollection.deleteMany({ age: { $gt: 35 } })
```

## 5. 实践练习

### 5.1 创建一个新的数据库 `myBlog`，并在其中创建一个集合 `posts`。

```javascript
test> use myBlog
switched to db myBlog
myBlog> db.posts.insertOne({ title: "First Post", content: "Hello, World!" })
```

### 5.2 插入多个博客文章到 `posts` 集合中。

```javascript
myBlog> db.posts.insertMany([
  { title: "Second Post", content: "This is my second post." },
  { title: "Third Post", content: "This is my third post." }
])
```

### 5.3 查询所有博客文章。

```javascript
myBlog> db.posts.find()
```

### 5.4 更新第一篇博客文章的标题。

```javascript
myBlog> db.posts.updateOne(
  { title: "First Post" },
  { $set: { title: "Updated First Post" } }
)
```

### 5.5 删除标题为 "Third Post" 的博客文章。

```javascript
myBlog> db.posts.deleteOne({ title: "Third Post" })
```

## 6. 总结

通过本教程，你已经学习了 MongoDB Shell 的基础操作，包括数据库、集合和文档的操作。这些基础操作是使用 MongoDB 进行数据管理的基础。接下来，你可以继续学习更高级的 MongoDB 功能，如索引、聚合框架和事务处理等。

## 7. 下一步

- 学习 MongoDB 的查询语言和操作符
- 探索 MongoDB 的索引和性能优化
- 了解 MongoDB 的聚合框架

希望本教程对你有所帮助，祝你在 MongoDB 的学习旅程中取得成功！