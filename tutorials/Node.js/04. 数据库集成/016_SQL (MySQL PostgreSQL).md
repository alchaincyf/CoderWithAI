---
title: SQL 数据库入门教程（MySQL, PostgreSQL）
date: 2023-10-05
description: 本课程将带你从零开始学习SQL数据库，涵盖MySQL和PostgreSQL的基本操作、查询优化、数据管理等内容。
slug: sql-database-tutorial
tags:
  - SQL
  - MySQL
  - PostgreSQL
category: 数据库
keywords:
  - SQL 数据库
  - MySQL 教程
  - PostgreSQL 教程
  - 数据库查询
  - 数据管理
---

# SQL 数据库（MySQL, PostgreSQL）

## 1. 概述

SQL（Structured Query Language）是一种用于管理关系型数据库的标准语言。MySQL 和 PostgreSQL 是两种广泛使用的关系型数据库管理系统（RDBMS）。本教程将介绍如何使用 Node.js 与 MySQL 和 PostgreSQL 进行交互。

## 2. 安装和配置

### 2.1 安装 MySQL

1. **下载 MySQL**: 访问 [MySQL 官方网站](https://dev.mysql.com/downloads/mysql/) 下载适合你操作系统的安装包。
2. **安装 MySQL**: 按照安装向导的指示完成安装。
3. **配置 MySQL**: 安装完成后，启动 MySQL 服务并设置 root 用户密码。

### 2.2 安装 PostgreSQL

1. **下载 PostgreSQL**: 访问 [PostgreSQL 官方网站](https://www.postgresql.org/download/) 下载适合你操作系统的安装包。
2. **安装 PostgreSQL**: 按照安装向导的指示完成安装。
3. **配置 PostgreSQL**: 安装完成后，启动 PostgreSQL 服务并设置超级用户（通常是 `postgres`）的密码。

## 3. 连接数据库

### 3.1 使用 Node.js 连接 MySQL

首先，安装 `mysql` 包：

```bash
npm install mysql
```

然后，创建一个连接：

```javascript
const mysql = require('mysql');

const connection = mysql.createConnection({
  host: 'localhost',
  user: 'root',
  password: 'yourpassword',
  database: 'yourdatabase'
});

connection.connect((err) => {
  if (err) throw err;
  console.log('Connected to MySQL database!');
});
```

### 3.2 使用 Node.js 连接 PostgreSQL

首先，安装 `pg` 包：

```bash
npm install pg
```

然后，创建一个连接：

```javascript
const { Client } = require('pg');

const client = new Client({
  user: 'postgres',
  host: 'localhost',
  database: 'yourdatabase',
  password: 'yourpassword',
  port: 5432,
});

client.connect((err) => {
  if (err) throw err;
  console.log('Connected to PostgreSQL database!');
});
```

## 4. 执行 SQL 查询

### 4.1 查询数据

使用 `query` 方法执行 SQL 查询：

```javascript
// MySQL
connection.query('SELECT * FROM users', (err, results) => {
  if (err) throw err;
  console.log(results);
});

// PostgreSQL
client.query('SELECT * FROM users', (err, res) => {
  if (err) throw err;
  console.log(res.rows);
});
```

### 4.2 插入数据

插入数据时，可以使用 `INSERT INTO` 语句：

```javascript
// MySQL
const user = { name: 'John', email: 'john@example.com' };
connection.query('INSERT INTO users SET ?', user, (err, result) => {
  if (err) throw err;
  console.log('User inserted with ID:', result.insertId);
});

// PostgreSQL
const user = { name: 'John', email: 'john@example.com' };
client.query('INSERT INTO users(name, email) VALUES($1, $2)', [user.name, user.email], (err, res) => {
  if (err) throw err;
  console.log('User inserted with ID:', res.rows[0].id);
});
```

### 4.3 更新数据

更新数据时，可以使用 `UPDATE` 语句：

```javascript
// MySQL
connection.query('UPDATE users SET email = ? WHERE name = ?', ['newemail@example.com', 'John'], (err, result) => {
  if (err) throw err;
  console.log('Rows updated:', result.affectedRows);
});

// PostgreSQL
client.query('UPDATE users SET email = $1 WHERE name = $2', ['newemail@example.com', 'John'], (err, res) => {
  if (err) throw err;
  console.log('Rows updated:', res.rowCount);
});
```

### 4.4 删除数据

删除数据时，可以使用 `DELETE` 语句：

```javascript
// MySQL
connection.query('DELETE FROM users WHERE name = ?', ['John'], (err, result) => {
  if (err) throw err;
  console.log('Rows deleted:', result.affectedRows);
});

// PostgreSQL
client.query('DELETE FROM users WHERE name = $1', ['John'], (err, res) => {
  if (err) throw err;
  console.log('Rows deleted:', res.rowCount);
});
```

## 5. 实践练习

### 5.1 创建一个简单的用户管理系统

1. **创建数据库和表**: 使用 MySQL 或 PostgreSQL 创建一个名为 `users` 的表，包含 `id`, `name`, `email` 和 `created_at` 字段。
2. **编写 Node.js 应用**: 创建一个 Node.js 应用，实现以下功能：
   - 添加新用户
   - 列出所有用户
   - 更新用户信息
   - 删除用户

### 5.2 代码示例

```javascript
// MySQL
const mysql = require('mysql');

const connection = mysql.createConnection({
  host: 'localhost',
  user: 'root',
  password: 'yourpassword',
  database: 'yourdatabase'
});

connection.connect((err) => {
  if (err) throw err;
  console.log('Connected to MySQL database!');
});

// 添加用户
const addUser = (user) => {
  connection.query('INSERT INTO users SET ?', user, (err, result) => {
    if (err) throw err;
    console.log('User inserted with ID:', result.insertId);
  });
};

// 列出所有用户
const listUsers = () => {
  connection.query('SELECT * FROM users', (err, results) => {
    if (err) throw err;
    console.log(results);
  });
};

// 更新用户
const updateUser = (id, email) => {
  connection.query('UPDATE users SET email = ? WHERE id = ?', [email, id], (err, result) => {
    if (err) throw err;
    console.log('Rows updated:', result.affectedRows);
  });
};

// 删除用户
const deleteUser = (id) => {
  connection.query('DELETE FROM users WHERE id = ?', [id], (err, result) => {
    if (err) throw err;
    console.log('Rows deleted:', result.affectedRows);
  });
};

// 使用示例
addUser({ name: 'John', email: 'john@example.com' });
listUsers();
updateUser(1, 'newemail@example.com');
deleteUser(1);
```

## 6. 总结

本教程介绍了如何使用 Node.js 与 MySQL 和 PostgreSQL 进行交互。通过学习本教程，你应该能够创建数据库连接、执行基本的 CRUD 操作，并构建一个简单的用户管理系统。继续探索更多高级功能，如事务处理、索引优化和数据库迁移。

## 7. 进一步学习

- **ORM（对象关系映射）**: 学习使用 Sequelize 或 TypeORM 简化数据库操作。
- **数据库迁移**: 使用工具如 `knex.js` 进行数据库模式迁移。
- **性能优化**: 了解如何优化查询性能和数据库设计。

通过这些学习，你将能够更高效地管理和操作 SQL 数据库。