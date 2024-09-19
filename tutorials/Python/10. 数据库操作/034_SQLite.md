---
title: SQLite 基础教程
date: 2023-10-05
description: 本课程将带你深入了解SQLite数据库的基础知识，包括安装、创建数据库、执行SQL查询等。
slug: sqlite-basics
tags:
  - 数据库
  - SQLite
  - 编程基础
category: 数据库
keywords:
  - SQLite
  - SQL查询
  - 数据库基础
---

# SQLite 基础

## 概述

SQLite 是一个轻量级的嵌入式关系型数据库管理系统（RDBMS），广泛应用于移动设备、桌面应用和小型网站。它不需要独立的服务器进程，数据存储在一个单一的文件中，非常适合学习和开发小型项目。

## 安装 SQLite

SQLite 通常已经预装在大多数操作系统中，包括 Windows、macOS 和 Linux。你可以通过命令行工具或图形界面工具来访问 SQLite 数据库。

### 命令行工具

在终端或命令提示符中输入以下命令来检查 SQLite 是否已安装：

```bash
sqlite3 --version
```

如果 SQLite 已安装，你会看到版本信息。如果没有安装，你可以从 [SQLite 官方网站](https://www.sqlite.org/download.html) 下载并安装。

### 图形界面工具

推荐使用 [DB Browser for SQLite](https://sqlitebrowser.org/)，这是一个免费的开源工具，提供了图形界面来管理 SQLite 数据库。

## 创建数据库

使用 SQLite 命令行工具创建一个新的数据库文件：

```bash
sqlite3 mydatabase.db
```

这将在当前目录下创建一个名为 `mydatabase.db` 的数据库文件。

## 创建表

在 SQLite 中，表是存储数据的基本结构。你可以使用 SQL 语句来创建表。

```sql
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    age INTEGER
);
```

### 解释

- `id`: 自增的主键。
- `name`: 用户名，不能为空。
- `email`: 用户邮箱，不能为空且必须唯一。
- `age`: 用户年龄，可以为空。

## 插入数据

使用 `INSERT INTO` 语句向表中插入数据。

```sql
INSERT INTO users (name, email, age) VALUES ('Alice', 'alice@example.com', 30);
INSERT INTO users (name, email, age) VALUES ('Bob', 'bob@example.com', 25);
```

## 查询数据

使用 `SELECT` 语句从表中查询数据。

```sql
SELECT * FROM users;
```

这将返回 `users` 表中的所有记录。

### 条件查询

你可以使用 `WHERE` 子句来过滤数据。

```sql
SELECT * FROM users WHERE age > 25;
```

## 更新数据

使用 `UPDATE` 语句更新表中的数据。

```sql
UPDATE users SET age = 31 WHERE name = 'Alice';
```

## 删除数据

使用 `DELETE` 语句删除表中的数据。

```sql
DELETE FROM users WHERE name = 'Bob';
```

## 实践练习

### 练习 1: 创建并查询表

1. 创建一个新的 SQLite 数据库文件 `practice.db`。
2. 创建一个名为 `products` 的表，包含以下字段：
   - `id` (INTEGER, PRIMARY KEY, AUTOINCREMENT)
   - `name` (TEXT, NOT NULL)
   - `price` (REAL, NOT NULL)
   - `quantity` (INTEGER)
3. 向 `products` 表中插入几条记录。
4. 查询并显示所有产品的信息。

### 练习 2: 更新和删除数据

1. 更新 `products` 表中某个产品的价格。
2. 删除 `products` 表中某个产品的记录。
3. 查询并显示更新和删除后的所有产品信息。

## 总结

通过本教程，你已经学习了 SQLite 的基本操作，包括创建数据库、创建表、插入数据、查询数据、更新数据和删除数据。SQLite 是一个简单而强大的工具，适合初学者学习和使用。继续探索 SQLite 的高级功能，如索引、事务和触发器，将帮助你更好地掌握数据库管理。