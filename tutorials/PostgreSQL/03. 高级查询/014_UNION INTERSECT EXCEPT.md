---
title: 集合操作详解：UNION, INTERSECT, EXCEPT
date: 2023-10-05
description: 本课程详细讲解SQL中的集合操作，包括UNION、INTERSECT和EXCEPT的使用方法及其在数据库查询中的应用。
slug: set-operations-union-intersect-except
tags:
  - SQL
  - 数据库
  - 查询操作
category: 数据库操作
keywords:
  - UNION
  - INTERSECT
  - EXCEPT
  - 集合操作
  - SQL查询
---

# 集合操作（UNION, INTERSECT, EXCEPT）

在SQL中，集合操作是一种强大的工具，用于处理多个查询的结果集。集合操作允许你将多个查询的结果合并、交集或差集。常见的集合操作包括`UNION`、`INTERSECT`和`EXCEPT`。本教程将详细介绍这些操作的用法、语法和实际应用。

## 1. UNION 操作

`UNION`操作用于将两个或多个`SELECT`语句的结果集合并成一个结果集。`UNION`会自动去除重复的行，如果你想保留重复的行，可以使用`UNION ALL`。

### 1.1 语法

```sql
SELECT column1, column2, ...
FROM table1
UNION [ALL]
SELECT column1, column2, ...
FROM table2;
```

### 1.2 示例

假设我们有两个表`employees1`和`employees2`，它们分别存储了不同部门的员工信息。

```sql
-- 表 employees1
CREATE TABLE employees1 (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100),
    department VARCHAR(50)
);

-- 表 employees2
CREATE TABLE employees2 (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100),
    department VARCHAR(50)
);

-- 插入数据
INSERT INTO employees1 (name, department) VALUES ('Alice', 'HR'), ('Bob', 'IT');
INSERT INTO employees2 (name, department) VALUES ('Charlie', 'Sales'), ('Alice', 'HR');
```

现在，我们使用`UNION`来合并这两个表的数据：

```sql
SELECT name, department
FROM employees1
UNION
SELECT name, department
FROM employees2;
```

结果将是：

```
  name   | department
---------+-----------
 Alice   | HR
 Bob     | IT
 Charlie | Sales
```

### 1.3 实践练习

1. 创建两个表`students1`和`students2`，分别存储不同班级的学生信息。
2. 使用`UNION`操作将两个表的学生信息合并，并查看结果。

## 2. INTERSECT 操作

`INTERSECT`操作用于返回两个`SELECT`语句的结果集中相同的行。换句话说，它返回两个结果集的交集。

### 2.1 语法

```sql
SELECT column1, column2, ...
FROM table1
INTERSECT
SELECT column1, column2, ...
FROM table2;
```

### 2.2 示例

继续使用上面的`employees1`和`employees2`表，我们使用`INTERSECT`来查找两个表中相同的员工信息：

```sql
SELECT name, department
FROM employees1
INTERSECT
SELECT name, department
FROM employees2;
```

结果将是：

```
  name  | department
--------+-----------
 Alice  | HR
```

### 2.3 实践练习

1. 创建两个表`courses1`和`courses2`，分别存储不同学期的课程信息。
2. 使用`INTERSECT`操作查找两个学期都开设的课程。

## 3. EXCEPT 操作

`EXCEPT`操作用于返回第一个`SELECT`语句的结果集中存在，但不在第二个`SELECT`语句结果集中的行。换句话说，它返回两个结果集的差集。

### 3.1 语法

```sql
SELECT column1, column2, ...
FROM table1
EXCEPT
SELECT column1, column2, ...
FROM table2;
```

### 3.2 示例

继续使用`employees1`和`employees2`表，我们使用`EXCEPT`来查找只在`employees1`中存在的员工信息：

```sql
SELECT name, department
FROM employees1
EXCEPT
SELECT name, department
FROM employees2;
```

结果将是：

```
  name  | department
--------+-----------
 Bob    | IT
```

### 3.3 实践练习

1. 创建两个表`products1`和`products2`，分别存储不同供应商的产品信息。
2. 使用`EXCEPT`操作查找只在第一个供应商中存在的产品。

## 4. 注意事项

1. **列数和数据类型**：在进行集合操作时，两个`SELECT`语句的列数和数据类型必须匹配。
2. **排序**：如果你希望结果集按特定顺序排列，可以在最后一个`SELECT`语句后使用`ORDER BY`。

## 5. 总结

集合操作是SQL中非常强大的工具，能够帮助你轻松处理多个查询的结果集。通过`UNION`、`INTERSECT`和`EXCEPT`，你可以实现结果集的合并、交集和差集操作。希望本教程能帮助你更好地理解和应用这些操作。

## 6. 下一步

接下来，你可以继续学习SQL中的其他高级主题，如窗口函数、公共表表达式（CTE）等。这些主题将进一步增强你对SQL的理解和应用能力。

---

通过本教程，你应该已经掌握了`UNION`、`INTERSECT`和`EXCEPT`的基本用法和实际应用。继续练习和探索，你将能够更熟练地使用这些强大的SQL工具。