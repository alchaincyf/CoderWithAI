---
title: 聚合函数详解：SUM, AVG, COUNT, MAX, MIN
date: 2023-10-05
description: 本课程详细讲解SQL中的聚合函数，包括SUM, AVG, COUNT, MAX, MIN的使用方法和实际应用场景，帮助你掌握数据分析的核心技能。
slug: aggregate-functions-sql
tags:
  - SQL
  - 数据分析
  - 数据库
category: 编程基础
keywords:
  - 聚合函数
  - SUM
  - AVG
  - COUNT
  - MAX
  - MIN
---

# 聚合函数 (SUM, AVG, COUNT, MAX, MIN)

## 概述

在SQL中，聚合函数用于对一组值执行计算并返回单个值。常见的聚合函数包括 `SUM`, `AVG`, `COUNT`, `MAX`, 和 `MIN`。这些函数在数据分析和报表生成中非常有用。

## 聚合函数详解

### 1. SUM

`SUM` 函数用于计算某一列的总和。它只适用于数值类型的列。

**语法:**
```sql
SELECT SUM(column_name) FROM table_name;
```

**示例:**
```sql
SELECT SUM(price) AS total_price FROM products;
```

### 2. AVG

`AVG` 函数用于计算某一列的平均值。它也只适用于数值类型的列。

**语法:**
```sql
SELECT AVG(column_name) FROM table_name;
```

**示例:**
```sql
SELECT AVG(price) AS average_price FROM products;
```

### 3. COUNT

`COUNT` 函数用于计算某一列中非空值的数量。它可以用于任何数据类型的列。

**语法:**
```sql
SELECT COUNT(column_name) FROM table_name;
```

**示例:**
```sql
SELECT COUNT(product_id) AS total_products FROM products;
```

### 4. MAX

`MAX` 函数用于返回某一列中的最大值。它可以用于数值、字符串和日期类型的列。

**语法:**
```sql
SELECT MAX(column_name) FROM table_name;
```

**示例:**
```sql
SELECT MAX(price) AS max_price FROM products;
```

### 5. MIN

`MIN` 函数用于返回某一列中的最小值。它同样适用于数值、字符串和日期类型的列。

**语法:**
```sql
SELECT MIN(column_name) FROM table_name;
```

**示例:**
```sql
SELECT MIN(price) AS min_price FROM products;
```

## 实践练习

### 练习1: 计算总销售额

假设你有一个名为 `orders` 的表，其中包含 `order_id`, `product_id`, 和 `quantity` 列。请编写一个查询来计算所有订单的总销售额。

**提示:** 你需要先计算每个订单的销售额，然后再求和。

```sql
SELECT SUM(quantity * price) AS total_sales
FROM orders
JOIN products ON orders.product_id = products.product_id;
```

### 练习2: 计算平均订单量

假设你有一个名为 `orders` 的表，其中包含 `order_id`, `product_id`, 和 `quantity` 列。请编写一个查询来计算所有订单的平均订单量。

```sql
SELECT AVG(quantity) AS average_order_quantity
FROM orders;
```

### 练习3: 查找最贵和最便宜的产品

假设你有一个名为 `products` 的表，其中包含 `product_id`, `product_name`, 和 `price` 列。请编写一个查询来查找最贵和最便宜的产品。

```sql
SELECT MAX(price) AS max_price, MIN(price) AS min_price
FROM products;
```

## 总结

聚合函数是SQL中非常强大的工具，能够帮助你快速进行数据分析和报表生成。通过掌握 `SUM`, `AVG`, `COUNT`, `MAX`, 和 `MIN` 这些基本函数，你可以在实际项目中更高效地处理数据。

## 下一步

接下来，你可以学习如何使用 `GROUP BY` 子句与聚合函数结合，进一步分析数据。此外，`HAVING` 子句也是一个重要的概念，它允许你对分组后的数据进行过滤。