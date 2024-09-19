---
title: JSON和JSONB支持 - PostgreSQL数据库编程教程
date: 2023-10-05
description: 本课程详细介绍如何在PostgreSQL数据库中使用JSON和JSONB数据类型，包括创建、查询和操作JSON数据。
slug: json-jsonb-support-postgresql
tags:
  - PostgreSQL
  - JSON
  - JSONB
category: 数据库编程
keywords:
  - PostgreSQL JSON
  - JSONB数据类型
  - 数据库编程
---

# JSON和JSONB支持

## 1. 概述

在现代数据库应用中，处理非结构化数据变得越来越重要。PostgreSQL 提供了对 JSON（JavaScript Object Notation）数据类型的支持，使得存储和查询非结构化数据变得更加容易。PostgreSQL 支持两种 JSON 数据类型：`JSON` 和 `JSONB`。`JSON` 是原始的 JSON 数据类型，而 `JSONB` 是二进制格式的 JSON，它在性能和功能上有所增强。

## 2. JSON 和 JSONB 的区别

### 2.1 JSON

- **存储格式**：`JSON` 类型存储的是原始的 JSON 文本。
- **性能**：在查询时需要解析 JSON 文本，因此性能较低。
- **功能**：支持基本的 JSON 操作，但功能有限。

### 2.2 JSONB

- **存储格式**：`JSONB` 类型存储的是二进制格式的 JSON，去除了冗余的空白和重复的键。
- **性能**：由于存储的是二进制格式，查询时不需要解析，因此性能较高。
- **功能**：支持更多的操作，如索引、键值对的直接访问等。

## 3. 创建包含 JSON 和 JSONB 列的表

### 3.1 创建表

```sql
CREATE TABLE orders (
    id SERIAL PRIMARY KEY,
    info JSON,
    details JSONB
);
```

### 3.2 插入数据

```sql
INSERT INTO orders (info, details)
VALUES (
    '{"customer": "John Doe", "items": {"product": "Beer", "qty": 6}}',
    '{"customer": "Jane Doe", "items": {"product": "Wine", "qty": 2}}'
);
```

## 4. 查询 JSON 和 JSONB 数据

### 4.1 基本查询

```sql
SELECT info, details FROM orders;
```

### 4.2 使用 `->` 和 `->>` 操作符

- `->` 返回 JSON 对象的键值对，结果是 JSON 类型。
- `->>` 返回 JSON 对象的键值对，结果是文本类型。

```sql
SELECT info->'customer' AS customer_json, details->>'customer' AS customer_text FROM orders;
```

### 4.3 使用 `#>` 和 `#>>` 操作符

- `#>` 返回 JSON 对象的路径值，结果是 JSON 类型。
- `#>>` 返回 JSON 对象的路径值，结果是文本类型。

```sql
SELECT info#>'{items, product}' AS product_json, details#>>'{items, product}' AS product_text FROM orders;
```

## 5. 索引 JSONB 数据

### 5.1 创建 GIN 索引

```sql
CREATE INDEX idx_details ON orders USING GIN (details);
```

### 5.2 使用索引查询

```sql
EXPLAIN ANALYZE SELECT * FROM orders WHERE details @> '{"customer": "Jane Doe"}';
```

## 6. 实践练习

### 6.1 创建一个包含 JSONB 列的表

```sql
CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name TEXT,
    attributes JSONB
);
```

### 6.2 插入数据

```sql
INSERT INTO products (name, attributes)
VALUES (
    'Laptop',
    '{"brand": "Dell", "model": "XPS 13", "specs": {"cpu": "i7", "ram": "16GB", "storage": "512GB SSD"}}'
);
```

### 6.3 查询数据

```sql
SELECT name, attributes->>'brand' AS brand, attributes#>>'{specs, cpu}' AS cpu FROM products;
```

### 6.4 创建索引并优化查询

```sql
CREATE INDEX idx_attributes ON products USING GIN (attributes);

EXPLAIN ANALYZE SELECT * FROM products WHERE attributes @> '{"brand": "Dell"}';
```

## 7. 总结

PostgreSQL 的 `JSON` 和 `JSONB` 数据类型为处理非结构化数据提供了强大的支持。`JSONB` 由于其二进制存储格式和丰富的功能，通常是首选。通过索引和高效的查询操作符，可以显著提高查询性能。通过本教程的学习，你应该能够理解并应用 JSON 和 JSONB 数据类型在实际项目中。

## 8. 进一步学习

- 探索更多 JSONB 的高级功能，如 `jsonb_set`、`jsonb_each` 等。
- 研究如何在实际应用中结合其他 PostgreSQL 功能（如触发器、存储过程）来处理 JSONB 数据。
- 了解如何在分布式环境中使用 JSONB 数据类型，如在 PostgreSQL 的复制和流复制中。

通过这些深入的学习，你将能够更全面地掌握 PostgreSQL 中 JSON 和 JSONB 数据类型的应用。