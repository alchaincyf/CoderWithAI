---
title: PIVOT 和 UNPIVOT 操作详解
date: 2023-10-05
description: 本课程详细讲解SQL中的PIVOT和UNPIVOT操作，帮助你掌握数据透视和逆透视的技巧，提升数据处理能力。
slug: pivot-unpivot-operations
tags:
  - SQL
  - 数据处理
  - 数据透视
category: 数据库操作
keywords:
  - PIVOT
  - UNPIVOT
  - SQL操作
---

# PIVOT 和 UNPIVOT 操作

## 概述

在SQL中，`PIVOT`和`UNPIVOT`是两个强大的操作，用于数据透视和逆透视。`PIVOT`操作可以将行数据转换为列数据，而`UNPIVOT`则相反，将列数据转换为行数据。这两个操作在数据分析和报表生成中非常有用。

## PIVOT 操作

### 理论解释

`PIVOT`操作允许你将行数据转换为列数据。例如，假设你有一个销售数据表，其中包含产品、月份和销售额。通过`PIVOT`操作，你可以将月份作为列标题，产品作为行标题，销售额作为值。

### 代码示例

假设我们有一个名为`sales`的表，结构如下：

```sql
CREATE TABLE sales (
    product VARCHAR(50),
    month VARCHAR(20),
    amount INT
);
```

插入一些示例数据：

```sql
INSERT INTO sales (product, month, amount) VALUES
('ProductA', 'January', 100),
('ProductA', 'February', 150),
('ProductB', 'January', 200),
('ProductB', 'February', 250);
```

使用`PIVOT`操作将数据透视：

```sql
SELECT product, January, February
FROM (
    SELECT product, month, amount
    FROM sales
) AS source
PIVOT (
    SUM(amount)
    FOR month IN ('January' AS January, 'February' AS February)
) AS pivot_table;
```

### 实践练习

1. 创建一个包含更多月份和产品的销售表。
2. 使用`PIVOT`操作将数据透视，使每个产品对应不同月份的销售额。

## UNPIVOT 操作

### 理论解释

`UNPIVOT`操作与`PIVOT`相反，它将列数据转换为行数据。例如，如果你有一个表，其中每行包含多个列（如不同月份的销售额），通过`UNPIVOT`操作，你可以将这些列转换为行数据。

### 代码示例

假设我们有一个名为`sales_pivot`的表，结构如下：

```sql
CREATE TABLE sales_pivot (
    product VARCHAR(50),
    January INT,
    February INT
);
```

插入一些示例数据：

```sql
INSERT INTO sales_pivot (product, January, February) VALUES
('ProductA', 100, 150),
('ProductB', 200, 250);
```

使用`UNPIVOT`操作将数据逆透视：

```sql
SELECT product, month, amount
FROM sales_pivot
UNPIVOT (
    amount FOR month IN (January, February)
) AS unpivot_table;
```

### 实践练习

1. 创建一个包含更多列（如不同月份）的销售表。
2. 使用`UNPIVOT`操作将数据逆透视，使每个产品对应不同月份的销售额。

## 总结

`PIVOT`和`UNPIVOT`操作是SQL中非常强大的工具，用于数据透视和逆透视。通过这些操作，你可以轻松地将行数据转换为列数据，或将列数据转换为行数据，从而满足不同的数据分析和报表需求。

### 进一步学习

1. 探索如何在不同的数据库管理系统（如MySQL、PostgreSQL、SQL Server）中使用`PIVOT`和`UNPIVOT`。
2. 研究如何在实际项目中应用这些操作，以优化数据处理和报表生成。

通过实践和不断学习，你将能够更熟练地使用`PIVOT`和`UNPIVOT`操作，提升你的SQL技能。