---
title: 插入数据 (INSERT) - SQL 基础教程
date: 2023-10-05
description: 本课程详细讲解如何在SQL数据库中使用INSERT语句插入数据，适合初学者学习SQL基础操作。
slug: sql-insert-data
tags:
  - SQL
  - 数据库
  - 编程基础
category: 数据库操作
keywords:
  - SQL INSERT
  - 数据库插入
  - SQL基础
---

# 插入数据 (INSERT)

## 1. 概述

在关系数据库中，`INSERT` 语句用于向表中添加新记录。这是数据库操作中最基本的操作之一，通常在创建表之后进行。`INSERT` 语句允许你指定要插入的列和对应的值。

## 2. 基本语法

`INSERT` 语句的基本语法如下：

```sql
INSERT INTO table_name (column1, column2, column3, ...)
VALUES (value1, value2, value3, ...);
```

- `table_name`：你要插入数据的表的名称。
- `column1, column2, ...`：你要插入数据的列名。
- `value1, value2, ...`：对应列的值。

### 2.1 插入所有列

如果你要插入所有列的数据，可以省略列名：

```sql
INSERT INTO table_name
VALUES (value1, value2, value3, ...);
```

**注意**：在这种情况下，值的顺序必须与表中列的顺序一致。

### 2.2 插入部分列

你也可以选择只插入部分列的数据：

```sql
INSERT INTO table_name (column1, column3)
VALUES (value1, value3);
```

在这种情况下，未指定的列将使用默认值（如果定义了默认值）或 `NULL`。

## 3. 示例

假设我们有一个名为 `employees` 的表，结构如下：

```sql
CREATE TABLE employees (
    id INT PRIMARY KEY,
    name VARCHAR(100),
    department VARCHAR(50),
    salary DECIMAL(10, 2)
);
```

### 3.1 插入单条记录

插入一条新记录：

```sql
INSERT INTO employees (id, name, department, salary)
VALUES (1, 'John Doe', 'IT', 5000.00);
```

### 3.2 插入多条记录

你可以一次性插入多条记录：

```sql
INSERT INTO employees (id, name, department, salary)
VALUES 
    (2, 'Jane Smith', 'HR', 4500.00),
    (3, 'Alice Johnson', 'Finance', 6000.00);
```

### 3.3 插入部分列

只插入 `id` 和 `name` 列：

```sql
INSERT INTO employees (id, name)
VALUES (4, 'Bob Brown');
```

在这种情况下，`department` 和 `salary` 列将使用默认值或 `NULL`。

## 4. 实践练习

### 4.1 创建表

首先，创建一个名为 `students` 的表：

```sql
CREATE TABLE students (
    student_id INT PRIMARY KEY,
    first_name VARCHAR(50),
    last_name VARCHAR(50),
    age INT,
    major VARCHAR(50)
);
```

### 4.2 插入数据

插入以下学生记录：

1. `student_id`: 1, `first_name`: 'Alice', `last_name`: 'Johnson', `age`: 22, `major`: 'Computer Science'
2. `student_id`: 2, `first_name`: 'Bob', `last_name`: 'Smith', `age`: 21, `major`: 'Mathematics'
3. `student_id`: 3, `first_name`: 'Charlie', `last_name`: 'Brown', `age`: 23, `major`: 'Physics'

使用 `INSERT` 语句完成上述操作。

### 4.3 验证插入

使用 `SELECT` 语句验证数据是否正确插入：

```sql
SELECT * FROM students;
```

## 5. 常见问题

### 5.1 主键冲突

如果你尝试插入一条记录，而该记录的主键已经存在，数据库会抛出错误。例如：

```sql
INSERT INTO employees (id, name, department, salary)
VALUES (1, 'John Doe', 'IT', 5000.00);
```

如果 `id` 为 1 的记录已经存在，数据库会报错。

### 5.2 数据类型不匹配

确保插入的值与列的数据类型匹配。例如，不能将字符串插入到 `INT` 类型的列中。

## 6. 总结

`INSERT` 语句是向数据库表中添加新记录的基本操作。通过本教程，你应该已经掌握了如何使用 `INSERT` 语句插入单条和多条记录，以及如何处理常见的插入问题。

## 7. 下一步

接下来，你可以学习如何使用 `UPDATE` 语句更新表中的数据。