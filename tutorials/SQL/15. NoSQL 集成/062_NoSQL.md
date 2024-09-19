---
title: 关系型数据库与 NoSQL 入门教程
date: 2023-10-05
description: 本课程将深入探讨关系型数据库与NoSQL数据库的基本概念、优缺点以及应用场景，帮助你掌握数据库管理的核心技能。
slug: relational-databases-vs-nosql
tags:
  - 数据库
  - 关系型数据库
  - NoSQL
category: 编程基础
keywords:
  - 关系型数据库
  - NoSQL
  - 数据库管理
  - 数据存储
  - 数据库设计
---

# 关系型数据库与 NoSQL

## 1. 关系型数据库简介

关系型数据库（Relational Database）是一种基于关系模型的数据库，使用结构化查询语言（SQL）进行数据管理。关系型数据库的核心是表（Table），表由行（Row）和列（Column）组成，每个表都有一个唯一的键（Key）来标识每一行数据。

### 1.1 SQL 简介和历史

SQL（Structured Query Language）是一种用于管理关系型数据库的标准语言。SQL 的历史可以追溯到 1970 年代，由 IBM 的研究员 Edgar F. Codd 提出关系模型。SQL 的标准化工作由 ANSI 和 ISO 组织进行，确保不同数据库系统之间的兼容性。

### 1.2 关系数据库概念

关系数据库的核心概念包括：
- **表（Table）**：数据的二维结构，由行和列组成。
- **行（Row）**：表中的一条记录。
- **列（Column）**：表中的一个字段，表示数据的某种属性。
- **主键（Primary Key）**：唯一标识表中每一行的字段。
- **外键（Foreign Key）**：用于建立表之间关系的字段。

## 2. 安装和配置数据库管理系统

### 2.1 安装 MySQL

MySQL 是一种流行的开源关系型数据库管理系统。以下是安装 MySQL 的基本步骤：

1. 下载 MySQL 安装包。
2. 运行安装程序，按照提示完成安装。
3. 配置 MySQL 服务，设置 root 用户密码。

```bash
# 安装 MySQL
sudo apt-get install mysql-server

# 启动 MySQL 服务
sudo systemctl start mysql

# 设置 root 用户密码
sudo mysql_secure_installation
```

### 2.2 安装 PostgreSQL

PostgreSQL 是另一种强大的开源关系型数据库管理系统。以下是安装 PostgreSQL 的基本步骤：

1. 下载 PostgreSQL 安装包。
2. 运行安装程序，按照提示完成安装。
3. 配置 PostgreSQL 服务，设置超级用户密码。

```bash
# 安装 PostgreSQL
sudo apt-get install postgresql

# 启动 PostgreSQL 服务
sudo systemctl start postgresql

# 设置超级用户密码
sudo -u postgres psql
ALTER USER postgres WITH PASSWORD 'yourpassword';
```

## 3. 基本 SQL 语法

### 3.1 创建和管理数据库

```sql
-- 创建数据库
CREATE DATABASE mydatabase;

-- 使用数据库
USE mydatabase;

-- 删除数据库
DROP DATABASE mydatabase;
```

### 3.2 创建表 (CREATE TABLE)

```sql
-- 创建表
CREATE TABLE employees (
    id INT PRIMARY KEY,
    name VARCHAR(100),
    department VARCHAR(50),
    salary DECIMAL(10, 2)
);
```

### 3.3 修改表结构 (ALTER TABLE)

```sql
-- 添加列
ALTER TABLE employees ADD COLUMN email VARCHAR(100);

-- 修改列
ALTER TABLE employees MODIFY COLUMN salary DECIMAL(12, 2);

-- 删除列
ALTER TABLE employees DROP COLUMN email;
```

### 3.4 删除表 (DROP TABLE)

```sql
-- 删除表
DROP TABLE employees;
```

## 4. 数据操作

### 4.1 插入数据 (INSERT)

```sql
-- 插入数据
INSERT INTO employees (id, name, department, salary)
VALUES (1, 'John Doe', 'IT', 5000.00);
```

### 4.2 更新数据 (UPDATE)

```sql
-- 更新数据
UPDATE employees
SET salary = 5500.00
WHERE id = 1;
```

### 4.3 删除数据 (DELETE)

```sql
-- 删除数据
DELETE FROM employees
WHERE id = 1;
```

## 5. 查询数据

### 5.1 基本查询 (SELECT)

```sql
-- 查询所有数据
SELECT * FROM employees;

-- 查询特定列
SELECT name, salary FROM employees;
```

### 5.2 WHERE 子句和条件过滤

```sql
-- 条件过滤
SELECT * FROM employees
WHERE department = 'IT';
```

### 5.3 排序 (ORDER BY)

```sql
-- 按工资排序
SELECT * FROM employees
ORDER BY salary DESC;
```

### 5.4 分组 (GROUP BY)

```sql
-- 按部门分组
SELECT department, COUNT(*) AS num_employees
FROM employees
GROUP BY department;
```

### 5.5 聚合函数 (SUM, AVG, COUNT, MAX, MIN)

```sql
-- 计算总工资
SELECT SUM(salary) AS total_salary
FROM employees;

-- 计算平均工资
SELECT AVG(salary) AS avg_salary
FROM employees;
```

## 6. 高级查询

### 6.1 子查询

```sql
-- 查询工资高于平均工资的员工
SELECT * FROM employees
WHERE salary > (SELECT AVG(salary) FROM employees);
```

### 6.2 表连接 (INNER JOIN, LEFT JOIN, RIGHT JOIN, FULL JOIN)

```sql
-- 内连接
SELECT employees.name, departments.name
FROM employees
INNER JOIN departments ON employees.department_id = departments.id;
```

### 6.3 自连接

```sql
-- 自连接
SELECT e1.name AS employee, e2.name AS manager
FROM employees e1
JOIN employees e2 ON e1.manager_id = e2.id;
```

## 7. 事务管理

### 7.1 事务概念

事务（Transaction）是一组数据库操作，要么全部执行成功，要么全部回滚。事务具有 ACID 属性：
- **原子性（Atomicity）**：事务是一个不可分割的工作单位。
- **一致性（Consistency）**：事务执行前后，数据库必须保持一致状态。
- **隔离性（Isolation）**：多个事务并发执行时，每个事务应该与其他事务隔离。
- **持久性（Durability）**：事务一旦提交，其结果应该永久保存在数据库中。

### 7.2 事务控制语句 (BEGIN, COMMIT, ROLLBACK)

```sql
-- 开始事务
BEGIN;

-- 插入数据
INSERT INTO employees (id, name, department, salary)
VALUES (2, 'Jane Smith', 'HR', 4500.00);

-- 提交事务
COMMIT;

-- 回滚事务
ROLLBACK;
```

## 8. NoSQL 数据库简介

NoSQL（Not Only SQL）数据库是一种非关系型数据库，适用于处理大规模、高并发的数据存储需求。NoSQL 数据库不使用 SQL 作为查询语言，而是使用其他查询方式，如键值对、文档、列族和图形等。

### 8.1 NoSQL 数据库类型

- **键值存储（Key-Value Store）**：如 Redis、Riak。
- **文档存储（Document Store）**：如 MongoDB、CouchDB。
- **列族存储（Column Family Store）**：如 Cassandra、HBase。
- **图形数据库（Graph Database）**：如 Neo4j、OrientDB。

### 8.2 NoSQL 数据库的优势

- **高可扩展性**：NoSQL 数据库通常设计为水平扩展，能够处理大规模数据。
- **灵活的数据模型**：NoSQL 数据库支持多种数据模型，适用于不同的应用场景。
- **高性能**：NoSQL 数据库通常具有较高的读写性能，适用于高并发的应用。

## 9. NoSQL 数据库实践

### 9.1 安装 MongoDB

MongoDB 是一种流行的文档存储 NoSQL 数据库。以下是安装 MongoDB 的基本步骤：

1. 下载 MongoDB 安装包。
2. 运行安装程序，按照提示完成安装。
3. 配置 MongoDB 服务，启动 MongoDB 服务。

```bash
# 安装 MongoDB
sudo apt-get install mongodb

# 启动 MongoDB 服务
sudo systemctl start mongodb
```

### 9.2 基本操作

```javascript
// 连接 MongoDB
const MongoClient = require('mongodb').MongoClient;
const uri = "mongodb://localhost:27017";
const client = new MongoClient(uri, { useNewUrlParser: true });

client.connect(err => {
  const collection = client.db("test").collection("employees");

  // 插入文档
  collection.insertOne({ name: "John Doe", department: "IT", salary: 5000.00 }, function(err, res) {
    console.log("Document inserted");
  });

  // 查询文档
  collection.find({ department: "IT" }).toArray(function(err, docs) {
    console.log("Found documents:", docs);
  });

  // 更新文档
  collection.updateOne({ name: "John Doe" }, { $set: { salary: 5500.00 } }, function(err, res) {
    console.log("Document updated");
  });

  // 删除文档
  collection.deleteOne({ name: "John Doe" }, function(err, res) {
    console.log("Document deleted");
  });

  client.close();
});
```

## 10. 实践练习

### 10.1 关系型数据库练习

1. 创建一个名为 `students` 的表，包含 `id`、`name`、`age`、`grade` 字段。
2. 插入几条学生记录。
3. 查询所有学生的信息，并按年龄排序。
4. 更新某个学生的成绩。
5. 删除某个学生的记录。

### 10.2 NoSQL 数据库练习

1. 使用 MongoDB 创建一个名为 `students` 的集合。
2. 插入几条学生文档。
3. 查询所有学生的信息，并按年龄排序。
4. 更新某个学生的成绩。
5. 删除某个学生的文档。

## 11. 总结

本教程介绍了关系型数据库和 NoSQL 数据库的基本概念、安装配置、基本操作和高级查询。通过实践练习，您可以更好地理解这两种数据库的使用方法和应用场景。关系型数据库适用于结构化数据的管理，而 NoSQL 数据库适用于大规模、高并发的数据存储需求。掌握这两种数据库技术，将有助于您在实际项目中选择合适的数据库解决方案。