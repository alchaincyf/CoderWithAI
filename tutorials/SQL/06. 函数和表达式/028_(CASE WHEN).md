---
title: 条件表达式 (CASE WHEN) 详解
date: 2023-10-05
description: 本课程详细讲解SQL中的条件表达式CASE WHEN的用法，包括基本语法、嵌套使用、以及在实际数据库查询中的应用。
slug: case-when-expression-tutorial
tags:
  - SQL
  - 数据库
  - 条件表达式
category: 数据库编程
keywords:
  - CASE WHEN
  - SQL条件表达式
  - 数据库查询
---

# 条件表达式 (CASE WHEN)

## 概述

在 SQL 中，`CASE WHEN` 是一种强大的条件表达式，允许你在查询中根据不同的条件返回不同的值。它类似于编程语言中的 `if-else` 语句，但专门用于 SQL 查询中。`CASE WHEN` 可以用于各种场景，如数据转换、条件过滤和生成报告。

## 基本语法

`CASE WHEN` 的基本语法如下：

```sql
CASE
    WHEN condition1 THEN result1
    WHEN condition2 THEN result2
    ...
    ELSE default_result
END
```

- `condition1`, `condition2`, ...: 这些是布尔表达式，用于判断是否满足某个条件。
- `result1`, `result2`, ...: 如果对应的 `condition` 为真，则返回该结果。
- `default_result`: 如果所有条件都不满足，则返回默认结果。

## 示例

### 示例 1: 简单的条件判断

假设我们有一个 `employees` 表，其中包含员工的 `salary` 和 `department` 信息。我们想要根据员工的薪水等级来标记他们。

```sql
SELECT 
    employee_id,
    salary,
    CASE
        WHEN salary > 100000 THEN 'High'
        WHEN salary BETWEEN 50000 AND 100000 THEN 'Medium'
        ELSE 'Low'
    END AS salary_level
FROM 
    employees;
```

在这个例子中，我们根据 `salary` 的不同范围，将员工分为 `High`, `Medium`, 和 `Low` 三个等级。

### 示例 2: 在 `WHERE` 子句中使用 `CASE WHEN`

我们也可以在 `WHERE` 子句中使用 `CASE WHEN` 来过滤数据。例如，我们只想查询薪水等级为 `High` 的员工。

```sql
SELECT 
    employee_id,
    salary
FROM 
    employees
WHERE
    CASE
        WHEN salary > 100000 THEN 'High'
        ELSE NULL
    END = 'High';
```

### 示例 3: 在 `ORDER BY` 子句中使用 `CASE WHEN`

我们还可以在 `ORDER BY` 子句中使用 `CASE WHEN` 来根据条件对结果进行排序。例如，我们希望首先按 `department` 排序，然后在每个部门内按 `salary` 排序。

```sql
SELECT 
    employee_id,
    department,
    salary
FROM 
    employees
ORDER BY 
    department,
    CASE
        WHEN salary > 100000 THEN 1
        ELSE 2
    END;
```

在这个例子中，薪水高于 100000 的员工会排在前面。

## 实践练习

### 练习 1: 薪水等级报告

假设你有一个 `employees` 表，其中包含 `employee_id`, `name`, `salary`, 和 `department` 字段。请编写一个查询，生成一个报告，显示每个员工的 `name`, `salary`, 和 `salary_level`（`High`, `Medium`, `Low`）。

```sql
SELECT 
    name,
    salary,
    CASE
        WHEN salary > 100000 THEN 'High'
        WHEN salary BETWEEN 50000 AND 100000 THEN 'Medium'
        ELSE 'Low'
    END AS salary_level
FROM 
    employees;
```

### 练习 2: 部门薪水统计

编写一个查询，统计每个部门的平均薪水，并根据平均薪水的高低标记部门为 `High`, `Medium`, 或 `Low`。

```sql
SELECT 
    department,
    AVG(salary) AS avg_salary,
    CASE
        WHEN AVG(salary) > 100000 THEN 'High'
        WHEN AVG(salary) BETWEEN 50000 AND 100000 THEN 'Medium'
        ELSE 'Low'
    END AS salary_level
FROM 
    employees
GROUP BY 
    department;
```

## 总结

`CASE WHEN` 是 SQL 中非常有用的条件表达式，允许你在查询中根据不同的条件返回不同的值。它可以在 `SELECT`, `WHERE`, `ORDER BY` 等子句中使用，帮助你实现复杂的数据处理和报告生成。通过本教程的学习，你应该能够理解并应用 `CASE WHEN` 来解决实际的数据库查询问题。