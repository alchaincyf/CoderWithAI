---
title: 表连接详解：INNER JOIN, LEFT JOIN, RIGHT JOIN, FULL JOIN
date: 2023-10-05
description: 本课程详细讲解SQL中的表连接操作，包括INNER JOIN、LEFT JOIN、RIGHT JOIN和FULL JOIN，帮助你掌握数据库查询中的关键技能。
slug: sql-table-joins
tags:
  - SQL
  - 数据库
  - 表连接
category: 数据库操作
keywords:
  - INNER JOIN
  - LEFT JOIN
  - RIGHT JOIN
  - FULL JOIN
  - SQL查询
---

# 表连接 (INNER JOIN, LEFT JOIN, RIGHT JOIN, FULL JOIN)

## 1. 概述

在关系数据库中，表连接（Join）是一种将两个或多个表中的数据组合在一起的操作。通过连接操作，我们可以根据表之间的关联字段（通常是主键和外键）来获取更复杂的数据视图。SQL 提供了多种连接类型，包括 `INNER JOIN`、`LEFT JOIN`、`RIGHT JOIN` 和 `FULL JOIN`。每种连接类型都有其特定的用途和结果集。

## 2. 连接类型详解

### 2.1 INNER JOIN

`INNER JOIN` 是最常用的连接类型，它返回两个表中满足连接条件的所有行。换句话说，只有当两个表中的关联字段匹配时，才会返回结果。

#### 语法

```sql
SELECT 列名
FROM 表1
INNER JOIN 表2
ON 表1.列名 = 表2.列名;
```

#### 示例

假设我们有两个表：`Customers` 和 `Orders`。我们希望找出所有有订单的客户信息。

```sql
SELECT Customers.CustomerID, Customers.CustomerName, Orders.OrderID
FROM Customers
INNER JOIN Orders
ON Customers.CustomerID = Orders.CustomerID;
```

### 2.2 LEFT JOIN

`LEFT JOIN` 返回左表中的所有行，即使右表中没有匹配的行。如果右表中没有匹配的行，则结果集中右表的列将包含 `NULL` 值。

#### 语法

```sql
SELECT 列名
FROM 表1
LEFT JOIN 表2
ON 表1.列名 = 表2.列名;
```

#### 示例

我们希望找出所有客户及其订单信息，包括那些没有订单的客户。

```sql
SELECT Customers.CustomerID, Customers.CustomerName, Orders.OrderID
FROM Customers
LEFT JOIN Orders
ON Customers.CustomerID = Orders.CustomerID;
```

### 2.3 RIGHT JOIN

`RIGHT JOIN` 与 `LEFT JOIN` 相反，它返回右表中的所有行，即使左表中没有匹配的行。如果左表中没有匹配的行，则结果集中左表的列将包含 `NULL` 值。

#### 语法

```sql
SELECT 列名
FROM 表1
RIGHT JOIN 表2
ON 表1.列名 = 表2.列名;
```

#### 示例

我们希望找出所有订单及其对应的客户信息，包括那些没有客户的订单。

```sql
SELECT Customers.CustomerID, Customers.CustomerName, Orders.OrderID
FROM Customers
RIGHT JOIN Orders
ON Customers.CustomerID = Orders.CustomerID;
```

### 2.4 FULL JOIN

`FULL JOIN` 返回两个表中的所有行，无论是否匹配。如果某个表中没有匹配的行，则结果集中该表的列将包含 `NULL` 值。

#### 语法

```sql
SELECT 列名
FROM 表1
FULL JOIN 表2
ON 表1.列名 = 表2.列名;
```

#### 示例

我们希望找出所有客户和订单的组合，包括那些没有订单的客户和没有客户的订单。

```sql
SELECT Customers.CustomerID, Customers.CustomerName, Orders.OrderID
FROM Customers
FULL JOIN Orders
ON Customers.CustomerID = Orders.CustomerID;
```

## 3. 实践练习

### 3.1 创建表

首先，我们创建两个表 `Customers` 和 `Orders`，并插入一些示例数据。

```sql
CREATE TABLE Customers (
    CustomerID INT PRIMARY KEY,
    CustomerName VARCHAR(50)
);

CREATE TABLE Orders (
    OrderID INT PRIMARY KEY,
    CustomerID INT,
    OrderDate DATE
);

INSERT INTO Customers (CustomerID, CustomerName) VALUES
(1, 'Alice'),
(2, 'Bob'),
(3, 'Charlie');

INSERT INTO Orders (OrderID, CustomerID, OrderDate) VALUES
(101, 1, '2023-01-01'),
(102, 2, '2023-02-01'),
(103, 2, '2023-03-01');
```

### 3.2 执行连接查询

现在，我们使用上述连接类型对这两个表进行查询。

#### INNER JOIN

```sql
SELECT Customers.CustomerID, Customers.CustomerName, Orders.OrderID
FROM Customers
INNER JOIN Orders
ON Customers.CustomerID = Orders.CustomerID;
```

#### LEFT JOIN

```sql
SELECT Customers.CustomerID, Customers.CustomerName, Orders.OrderID
FROM Customers
LEFT JOIN Orders
ON Customers.CustomerID = Orders.CustomerID;
```

#### RIGHT JOIN

```sql
SELECT Customers.CustomerID, Customers.CustomerName, Orders.OrderID
FROM Customers
RIGHT JOIN Orders
ON Customers.CustomerID = Orders.CustomerID;
```

#### FULL JOIN

```sql
SELECT Customers.CustomerID, Customers.CustomerName, Orders.OrderID
FROM Customers
FULL JOIN Orders
ON Customers.CustomerID = Orders.CustomerID;
```

## 4. 总结

通过本教程，我们学习了 SQL 中的四种主要连接类型：`INNER JOIN`、`LEFT JOIN`、`RIGHT JOIN` 和 `FULL JOIN`。每种连接类型都有其独特的用途和结果集。理解这些连接类型对于从多个表中获取所需数据至关重要。通过实践练习，我们进一步巩固了这些概念。

## 5. 下一步

在掌握了表连接的基本概念后，你可以继续学习更高级的 SQL 主题，如自连接、UNION 和 UNION ALL、子查询等。这些主题将进一步增强你处理复杂数据查询的能力。