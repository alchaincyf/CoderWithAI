---
title: UNION 和 UNION ALL 操作详解
date: 2023-10-05
description: 本课程详细讲解SQL中的UNION和UNION ALL操作，帮助你理解如何合并查询结果集，并区分它们的差异和适用场景。
slug: union-and-union-all-explained
tags:
  - SQL
  - 数据库操作
  - 查询优化
category: 数据库
keywords:
  - UNION
  - UNION ALL
  - SQL查询
  - 数据库合并
---

# UNION 和 UNION ALL

## 1. 概述

在 SQL 中，`UNION` 和 `UNION ALL` 是用于合并两个或多个 `SELECT` 语句结果集的操作符。它们允许你将多个查询的结果组合成一个结果集。`UNION` 和 `UNION ALL` 的主要区别在于 `UNION` 会自动去除重复的行，而 `UNION ALL` 则保留所有行，包括重复的行。

## 2. UNION 和 UNION ALL 的基本语法

```sql
SELECT column1, column2, ...
FROM table1
UNION [ALL]
SELECT column1, column2, ...
FROM table2;
```

- `UNION`：合并两个 `SELECT` 语句的结果集，并去除重复的行。
- `UNION ALL`：合并两个 `SELECT` 语句的结果集，保留所有行，包括重复的行。

## 3. 示例

### 3.1 创建示例表

首先，我们创建两个示例表 `employees1` 和 `employees2`：

```sql
CREATE TABLE employees1 (
    id INT PRIMARY KEY,
    name VARCHAR(50),
    department VARCHAR(50)
);

CREATE TABLE employees2 (
    id INT PRIMARY KEY,
    name VARCHAR(50),
    department VARCHAR(50)
);
```

### 3.2 插入数据

向表中插入一些数据：

```sql
INSERT INTO employees1 (id, name, department) VALUES
(1, 'Alice', 'HR'),
(2, 'Bob', 'IT'),
(3, 'Charlie', 'Finance');

INSERT INTO employees2 (id, name, department) VALUES
(2, 'Bob', 'IT'),
(4, 'David', 'Marketing'),
(5, 'Eve', 'Sales');
```

### 3.3 使用 UNION

使用 `UNION` 合并两个表的数据：

```sql
SELECT name, department
FROM employees1
UNION
SELECT name, department
FROM employees2;
```

**结果：**

| name    | department |
|---------|------------|
| Alice   | HR         |
| Bob     | IT         |
| Charlie | Finance    |
| David   | Marketing  |
| Eve     | Sales      |

### 3.4 使用 UNION ALL

使用 `UNION ALL` 合并两个表的数据：

```sql
SELECT name, department
FROM employees1
UNION ALL
SELECT name, department
FROM employees2;
```

**结果：**

| name    | department |
|---------|------------|
| Alice   | HR         |
| Bob     | IT         |
| Charlie | Finance    |
| Bob     | IT         |
| David   | Marketing  |
| Eve     | Sales      |

## 4. 注意事项

- **列数和数据类型**：`UNION` 和 `UNION ALL` 要求两个 `SELECT` 语句的列数和对应列的数据类型必须匹配。
- **排序**：如果需要对合并后的结果进行排序，可以在 `UNION` 或 `UNION ALL` 之后使用 `ORDER BY` 子句。

## 5. 实践练习

### 5.1 练习 1

创建两个表 `students1` 和 `students2`，分别插入一些学生信息，然后使用 `UNION` 合并两个表的数据，并按学生姓名排序。

### 5.2 练习 2

创建两个表 `orders1` 和 `orders2`，分别插入一些订单信息，然后使用 `UNION ALL` 合并两个表的数据，并按订单金额排序。

## 6. 总结

`UNION` 和 `UNION ALL` 是 SQL 中非常有用的操作符，用于合并多个查询的结果集。`UNION` 去除重复行，而 `UNION ALL` 保留所有行。掌握这两个操作符可以帮助你更灵活地处理和分析数据。

通过本教程的学习，你应该能够理解 `UNION` 和 `UNION ALL` 的基本概念和用法，并能够在实际项目中应用它们。