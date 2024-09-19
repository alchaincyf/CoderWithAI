---
title: WHERE 子句和条件过滤 - SQL 基础教程
date: 2023-10-05
description: 本课程详细讲解SQL中的WHERE子句及其在数据过滤中的应用，帮助你掌握如何使用条件过滤来查询数据库中的特定数据。
slug: where-clause-and-condition-filtering
tags:
  - SQL
  - 数据库
  - 数据过滤
category: 编程基础
keywords:
  - WHERE子句
  - SQL条件过滤
  - 数据库查询
---

# WHERE 子句和条件过滤

## 概述

在 SQL 中，`WHERE` 子句用于过滤数据，只返回满足特定条件的记录。它是查询中非常关键的一部分，能够帮助我们从数据库中提取出所需的数据。本教程将详细介绍 `WHERE` 子句的使用方法，包括其语法、常见的条件操作符以及如何组合多个条件。

## 语法

`WHERE` 子句的基本语法如下：

```sql
SELECT 列名1, 列名2, ...
FROM 表名
WHERE 条件;
```

- `SELECT`：指定要查询的列。
- `FROM`：指定要查询的表。
- `WHERE`：指定过滤条件。

## 常见的条件操作符

### 比较操作符

- `=`：等于
- `<>` 或 `!=`：不等于
- `>`：大于
- `<`：小于
- `>=`：大于等于
- `<=`：小于等于

### 逻辑操作符

- `AND`：逻辑与，多个条件同时满足时返回结果。
- `OR`：逻辑或，多个条件中只要有一个满足就返回结果。
- `NOT`：逻辑非，取反条件。

### 其他操作符

- `IN`：检查某个值是否在指定的集合中。
- `BETWEEN`：检查某个值是否在指定的范围内。
- `LIKE`：模糊匹配，常用于字符串匹配。
- `IS NULL`：检查某个值是否为空。
- `IS NOT NULL`：检查某个值是否不为空。

## 示例

### 示例 1：使用比较操作符

假设我们有一个 `students` 表，包含以下列：`id`, `name`, `age`, `grade`。

```sql
SELECT *
FROM students
WHERE age > 18;
```

这个查询将返回所有年龄大于 18 岁的学生记录。

### 示例 2：使用逻辑操作符

```sql
SELECT *
FROM students
WHERE age > 18 AND grade = 'A';
```

这个查询将返回所有年龄大于 18 岁且成绩为 A 的学生记录。

### 示例 3：使用 `IN` 操作符

```sql
SELECT *
FROM students
WHERE grade IN ('A', 'B');
```

这个查询将返回所有成绩为 A 或 B 的学生记录。

### 示例 4：使用 `BETWEEN` 操作符

```sql
SELECT *
FROM students
WHERE age BETWEEN 18 AND 25;
```

这个查询将返回所有年龄在 18 到 25 岁之间的学生记录。

### 示例 5：使用 `LIKE` 操作符

```sql
SELECT *
FROM students
WHERE name LIKE 'J%';
```

这个查询将返回所有名字以 J 开头的学生记录。

### 示例 6：使用 `IS NULL` 和 `IS NOT NULL`

```sql
SELECT *
FROM students
WHERE grade IS NULL;
```

这个查询将返回所有成绩为空的学生记录。

```sql
SELECT *
FROM students
WHERE grade IS NOT NULL;
```

这个查询将返回所有成绩不为空的学生记录。

## 实践练习

### 练习 1：基本条件过滤

假设我们有一个 `employees` 表，包含以下列：`id`, `name`, `department`, `salary`。

1. 查询所有 `salary` 大于 5000 的员工。
2. 查询所有 `department` 为 'Sales' 的员工。

### 练习 2：组合条件

1. 查询所有 `salary` 大于 5000 且 `department` 为 'Sales' 的员工。
2. 查询所有 `salary` 大于 5000 或 `department` 为 'Sales' 的员工。

### 练习 3：使用 `IN` 和 `BETWEEN`

1. 查询所有 `department` 为 'Sales' 或 'Marketing' 的员工。
2. 查询所有 `salary` 在 3000 到 7000 之间的员工。

### 练习 4：使用 `LIKE`

1. 查询所有名字以 'A' 开头的员工。
2. 查询所有名字包含 'son' 的员工。

### 练习 5：使用 `IS NULL` 和 `IS NOT NULL`

1. 查询所有 `department` 为空的员工。
2. 查询所有 `department` 不为空的员工。

## 总结

`WHERE` 子句是 SQL 查询中非常重要的一个部分，它允许我们根据特定的条件过滤数据。通过掌握 `WHERE` 子句的使用方法，我们可以更精确地从数据库中提取所需的数据。希望本教程能够帮助你更好地理解和使用 `WHERE` 子句。