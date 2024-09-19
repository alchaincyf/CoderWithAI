---
title: 深入理解SQL中的GROUP BY语句
date: 2023-10-05
description: 本课程详细讲解SQL中的GROUP BY语句，帮助你掌握如何对数据进行分组和聚合操作，提升数据分析能力。
slug: sql-group-by-tutorial
tags:
  - SQL
  - 数据库
  - 数据分析
category: 数据库技术
keywords:
  - GROUP BY
  - SQL分组
  - 数据聚合
---

# 分组 (GROUP BY)

## 1. 概述

在 SQL 中，`GROUP BY` 子句用于将数据按照一个或多个列进行分组，通常与聚合函数（如 `SUM`, `AVG`, `COUNT`, `MAX`, `MIN`）一起使用，以便对每个组进行汇总计算。`GROUP BY` 是数据分析和报表生成中的重要工具。

## 2. 理论解释

### 2.1 基本概念

- **分组**：将数据按照指定的列进行分组，每个组内的数据具有相同的列值。
- **聚合函数**：对每个组进行计算的函数，如 `SUM`（求和）、`AVG`（平均值）、`COUNT`（计数）、`MAX`（最大值）、`MIN`（最小值）。

### 2.2 语法结构

```sql
SELECT 列1, 列2, ..., 聚合函数(列)
FROM 表名
WHERE 条件
GROUP BY 列1, 列2, ...;
```

- `SELECT`：选择需要显示的列和聚合函数。
- `FROM`：指定数据来源的表。
- `WHERE`：过滤数据的条件。
- `GROUP BY`：指定分组的列。

## 3. 代码示例

### 3.1 示例表结构

假设我们有一个名为 `orders` 的表，结构如下：

```sql
CREATE TABLE orders (
    order_id INT PRIMARY KEY,
    customer_id INT,
    order_date DATE,
    amount DECIMAL(10, 2)
);
```

### 3.2 插入示例数据

```sql
INSERT INTO orders (order_id, customer_id, order_date, amount) VALUES
(1, 101, '2023-01-01', 100.00),
(2, 102, '2023-01-02', 150.00),
(3, 101, '2023-01-03', 200.00),
(4, 103, '2023-01-04', 50.00),
(5, 102, '2023-01-05', 300.00);
```

### 3.3 基本分组查询

查询每个客户的订单总金额：

```sql
SELECT customer_id, SUM(amount) AS total_amount
FROM orders
GROUP BY customer_id;
```

**输出结果**：

| customer_id | total_amount |
|-------------|--------------|
| 101         | 300.00       |
| 102         | 450.00       |
| 103         | 50.00        |

### 3.4 使用多个列进行分组

查询每个客户在每个月的订单总金额：

```sql
SELECT customer_id, DATE_FORMAT(order_date, '%Y-%m') AS month, SUM(amount) AS total_amount
FROM orders
GROUP BY customer_id, DATE_FORMAT(order_date, '%Y-%m');
```

**输出结果**：

| customer_id | month   | total_amount |
|-------------|---------|--------------|
| 101         | 2023-01 | 300.00       |
| 102         | 2023-01 | 450.00       |
| 103         | 2023-01 | 50.00        |

## 4. 实践练习

### 4.1 练习1：计算每个客户的订单数量

编写一个 SQL 查询，计算每个客户的订单数量。

```sql
SELECT customer_id, COUNT(*) AS order_count
FROM orders
GROUP BY customer_id;
```

### 4.2 练习2：计算每个月的总订单金额

编写一个 SQL 查询，计算每个月的总订单金额。

```sql
SELECT DATE_FORMAT(order_date, '%Y-%m') AS month, SUM(amount) AS total_amount
FROM orders
GROUP BY DATE_FORMAT(order_date, '%Y-%m');
```

## 5. 总结

`GROUP BY` 是 SQL 中非常重要的一个子句，它允许我们按照指定的列对数据进行分组，并使用聚合函数对每个组进行汇总计算。通过本教程的学习，你应该能够理解 `GROUP BY` 的基本概念和使用方法，并能够在实际项目中应用它。

## 6. 进阶学习

在掌握了 `GROUP BY` 的基本用法后，你可以进一步学习 `HAVING` 子句、子查询、表连接等高级 SQL 技术，以便更灵活地处理和分析数据。