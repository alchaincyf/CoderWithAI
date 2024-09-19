---
title: 创建表 (CREATE TABLE) - SQL基础教程
date: 2023-10-05
description: 本课程将详细介绍如何在SQL中使用CREATE TABLE语句创建数据库表，包括表结构定义、数据类型选择和约束条件设置。
slug: create-table-sql-tutorial
tags:
  - SQL
  - 数据库
  - 基础教程
category: 数据库管理
keywords:
  - CREATE TABLE
  - SQL语句
  - 数据库表
---

# 创建表 (CREATE TABLE)

## 概述

在关系数据库中，表是存储数据的基本单位。每个表由行和列组成，行代表记录，列代表记录的属性。`CREATE TABLE` 语句用于在数据库中创建一个新的表。本教程将详细介绍如何使用 `CREATE TABLE` 语句创建表，包括表的结构定义、数据类型选择、主键和外键的设置等。

## 1. 基本语法

`CREATE TABLE` 语句的基本语法如下：

```sql
CREATE TABLE table_name (
    column1 datatype constraints,
    column2 datatype constraints,
    ...
    columnN datatype constraints
);
```

- `table_name`：要创建的表的名称。
- `column1, column2, ..., columnN`：表中的列名。
- `datatype`：列的数据类型，如 `INT`, `VARCHAR`, `DATE` 等。
- `constraints`：列的约束条件，如 `NOT NULL`, `UNIQUE`, `PRIMARY KEY` 等。

## 2. 数据类型

在创建表时，选择合适的数据类型非常重要。以下是一些常见的数据类型：

- **整数类型**：`INT`, `BIGINT`, `SMALLINT`, `TINYINT`
- **浮点数类型**：`FLOAT`, `DOUBLE`, `DECIMAL(p, s)`
- **字符串类型**：`VARCHAR(n)`, `CHAR(n)`, `TEXT`
- **日期和时间类型**：`DATE`, `TIME`, `DATETIME`, `TIMESTAMP`
- **布尔类型**：`BOOLEAN` 或 `BOOL`

## 3. 约束条件

约束条件用于确保数据的完整性和一致性。常见的约束条件包括：

- **NOT NULL**：列不能为空。
- **UNIQUE**：列中的值必须是唯一的。
- **PRIMARY KEY**：主键，唯一标识表中的每一行。
- **FOREIGN KEY**：外键，用于建立表与表之间的关系。
- **CHECK**：检查约束，确保列中的值满足特定条件。
- **DEFAULT**：默认值，如果插入数据时未指定值，则使用默认值。

## 4. 创建表的示例

### 示例 1：创建一个简单的用户表

```sql
CREATE TABLE users (
    user_id INT PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(100) NOT NULL UNIQUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

在这个示例中：

- `user_id` 是主键，用于唯一标识每个用户。
- `username` 和 `email` 不能为空且必须唯一。
- `created_at` 列的默认值为当前时间戳。

### 示例 2：创建一个带有外键的订单表

```sql
CREATE TABLE orders (
    order_id INT PRIMARY KEY,
    user_id INT,
    order_date DATE NOT NULL,
    total_amount DECIMAL(10, 2) NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(user_id)
);
```

在这个示例中：

- `order_id` 是主键，用于唯一标识每个订单。
- `user_id` 是外键，引用 `users` 表中的 `user_id` 列。
- `order_date` 和 `total_amount` 不能为空。

## 5. 实践练习

### 练习 1：创建一个产品表

创建一个名为 `products` 的表，包含以下列：

- `product_id`：主键，整数类型。
- `product_name`：产品名称，字符串类型，最大长度为 100，不能为空。
- `price`：产品价格，浮点数类型，不能为空。
- `stock_quantity`：库存数量，整数类型，不能为空。

### 练习 2：创建一个订单详情表

创建一个名为 `order_details` 的表，包含以下列：

- `detail_id`：主键，整数类型。
- `order_id`：外键，引用 `orders` 表中的 `order_id` 列。
- `product_id`：外键，引用 `products` 表中的 `product_id` 列。
- `quantity`：购买数量，整数类型，不能为空。

## 6. 总结

`CREATE TABLE` 语句是关系数据库中创建表的基本操作。通过本教程，您学习了如何定义表的结构、选择合适的数据类型以及设置约束条件。创建表是数据库设计的基础，掌握这一技能对于后续的数据库操作至关重要。

## 7. 下一步

在掌握了 `CREATE TABLE` 语句后，您可以继续学习如何修改表结构 (`ALTER TABLE`) 和删除表 (`DROP TABLE`)。这些操作将帮助您更好地管理和维护数据库中的表。

---

通过本教程，您应该能够理解并掌握 `CREATE TABLE` 语句的基本用法。希望您在实践中不断巩固和提升这一技能，为后续的数据库操作打下坚实的基础。