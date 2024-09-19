---
title: 数据库连接教程：从基础到高级
date: 2023-10-05
description: 本课程详细讲解如何进行数据库连接，包括基础概念、常用数据库连接方法、高级技巧及常见问题解决。
slug: database-connection-tutorial
tags:
  - 数据库
  - 编程
  - SQL
category: 编程教程
keywords:
  - 数据库连接
  - SQL连接
  - 数据库编程
---

# 数据库连接

## 1. 概述

在现代软件开发中，数据库是存储和管理数据的核心组件。Java 提供了多种方式来连接和操作数据库。本教程将详细介绍如何使用 Java 连接数据库，执行 SQL 语句，以及管理数据库事务。

## 2. JDBC 简介

JDBC（Java Database Connectivity）是 Java 提供的一组 API，用于连接和操作各种关系型数据库。JDBC 提供了一个标准的接口，使得开发者可以使用相同的代码来连接不同的数据库，如 MySQL、Oracle、PostgreSQL 等。

### 2.1 JDBC 核心接口

- **DriverManager**: 用于管理数据库驱动程序。
- **Connection**: 表示与数据库的连接。
- **Statement**: 用于执行静态 SQL 语句。
- **PreparedStatement**: 用于执行预编译的 SQL 语句。
- **ResultSet**: 表示查询结果集。

## 3. 数据库连接步骤

### 3.1 加载数据库驱动

首先，需要加载数据库驱动程序。不同的数据库有不同的驱动程序。例如，连接 MySQL 数据库时，可以使用 `com.mysql.cj.jdbc.Driver`。

```java
Class.forName("com.mysql.cj.jdbc.Driver");
```

### 3.2 建立数据库连接

使用 `DriverManager.getConnection()` 方法建立与数据库的连接。需要提供数据库的 URL、用户名和密码。

```java
String url = "jdbc:mysql://localhost:3306/mydatabase";
String username = "root";
String password = "password";
Connection connection = DriverManager.getConnection(url, username, password);
```

### 3.3 执行 SQL 语句

#### 3.3.1 使用 Statement

`Statement` 接口用于执行静态 SQL 语句。

```java
Statement statement = connection.createStatement();
String sql = "SELECT * FROM users";
ResultSet resultSet = statement.executeQuery(sql);
```

#### 3.3.2 使用 PreparedStatement

`PreparedStatement` 接口用于执行预编译的 SQL 语句，可以防止 SQL 注入攻击。

```java
String sql = "INSERT INTO users (name, email) VALUES (?, ?)";
PreparedStatement preparedStatement = connection.prepareStatement(sql);
preparedStatement.setString(1, "John Doe");
preparedStatement.setString(2, "john.doe@example.com");
preparedStatement.executeUpdate();
```

### 3.4 处理结果集

使用 `ResultSet` 接口遍历查询结果。

```java
while (resultSet.next()) {
    int id = resultSet.getInt("id");
    String name = resultSet.getString("name");
    String email = resultSet.getString("email");
    System.out.println("ID: " + id + ", Name: " + name + ", Email: " + email);
}
```

### 3.5 关闭资源

在操作完成后，务必关闭 `ResultSet`、`Statement` 和 `Connection` 以释放资源。

```java
resultSet.close();
statement.close();
connection.close();
```

## 4. 事务管理

事务是一组数据库操作，要么全部成功执行，要么全部回滚。JDBC 提供了事务管理功能。

### 4.1 开启事务

```java
connection.setAutoCommit(false);
```

### 4.2 提交事务

```java
connection.commit();
```

### 4.3 回滚事务

```java
connection.rollback();
```

## 5. 实践练习

### 5.1 创建数据库表

创建一个名为 `users` 的表，包含 `id`、`name` 和 `email` 字段。

```sql
CREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100),
    email VARCHAR(100)
);
```

### 5.2 编写 Java 程序

编写一个 Java 程序，连接到数据库，插入一条记录，并查询所有记录。

```java
import java.sql.*;

public class DatabaseConnectionExample {
    public static void main(String[] args) {
        String url = "jdbc:mysql://localhost:3306/mydatabase";
        String username = "root";
        String password = "password";

        try {
            Class.forName("com.mysql.cj.jdbc.Driver");
            Connection connection = DriverManager.getConnection(url, username, password);

            // 插入记录
            String insertSql = "INSERT INTO users (name, email) VALUES (?, ?)";
            PreparedStatement insertStatement = connection.prepareStatement(insertSql);
            insertStatement.setString(1, "John Doe");
            insertStatement.setString(2, "john.doe@example.com");
            insertStatement.executeUpdate();

            // 查询记录
            String selectSql = "SELECT * FROM users";
            Statement selectStatement = connection.createStatement();
            ResultSet resultSet = selectStatement.executeQuery(selectSql);

            while (resultSet.next()) {
                int id = resultSet.getInt("id");
                String name = resultSet.getString("name");
                String email = resultSet.getString("email");
                System.out.println("ID: " + id + ", Name: " + name + ", Email: " + email);
            }

            // 关闭资源
            resultSet.close();
            selectStatement.close();
            insertStatement.close();
            connection.close();
        } catch (ClassNotFoundException | SQLException e) {
            e.printStackTrace();
        }
    }
}
```

## 6. 总结

本教程介绍了如何使用 Java 连接数据库，执行 SQL 语句，以及管理数据库事务。通过实践练习，你可以更好地理解这些概念，并能够在实际项目中应用它们。

## 7. 进一步学习

- **连接池**: 学习如何使用连接池（如 HikariCP）来优化数据库连接管理。
- **ORM 框架**: 探索如 Hibernate 这样的 ORM 框架，简化数据库操作。
- **高级事务管理**: 学习如何使用分布式事务和两阶段提交。

通过不断实践和学习，你将能够更加熟练地使用 Java 进行数据库编程。