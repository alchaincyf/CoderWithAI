---
title: 持久化与 Exposed 框架入门教程
date: 2023-10-05
description: 本课程将深入探讨如何在 Kotlin 中使用 Exposed 框架进行数据库持久化操作，包括创建表、插入数据、查询和更新记录等。
slug: persistence-with-exposed
tags:
  - Kotlin
  - Exposed
  - 数据库
category: 编程教程
keywords:
  - Kotlin 持久化
  - Exposed 框架
  - 数据库操作
---

# 持久化与 Exposed

## 概述

在现代应用程序开发中，数据持久化是一个至关重要的部分。持久化是指将数据存储在持久存储介质（如数据库）中，以便在应用程序关闭或重启后数据仍然可用。Kotlin 提供了多种持久化解决方案，其中 Exposed 是一个流行的轻量级 ORM（对象关系映射）框架，专门为 Kotlin 设计。

本教程将介绍如何使用 Exposed 进行数据持久化，包括数据库连接、表定义、数据操作等。

## 1. 安装 Exposed

首先，我们需要在项目中添加 Exposed 依赖。如果你使用的是 Gradle，可以在 `build.gradle.kts` 文件中添加以下依赖：

```kotlin
dependencies {
    implementation("org.jetbrains.exposed:exposed-core:0.38.2")
    implementation("org.jetbrains.exposed:exposed-dao:0.38.2")
    implementation("org.jetbrains.exposed:exposed-jdbc:0.38.2")
    implementation("org.jetbrains.exposed:exposed-java-time:0.38.2")
    implementation("org.postgresql:postgresql:42.2.23") // 根据你的数据库选择相应的驱动
}
```

## 2. 连接数据库

在使用 Exposed 之前，我们需要连接到数据库。Exposed 支持多种数据库，如 PostgreSQL、MySQL、SQLite 等。以下是连接到 PostgreSQL 数据库的示例：

```kotlin
import org.jetbrains.exposed.sql.Database
import org.jetbrains.exposed.sql.transactions.transaction

fun main() {
    // 连接到 PostgreSQL 数据库
    Database.connect(
        url = "jdbc:postgresql://localhost:5432/mydatabase",
        driver = "org.postgresql.Driver",
        user = "myuser",
        password = "mypassword"
    )

    // 执行事务
    transaction {
        // 在这里执行数据库操作
    }
}
```

## 3. 定义表结构

在 Exposed 中，表结构通过 `Table` 类来定义。每个表对应一个 `Table` 类的子类。以下是一个简单的用户表定义示例：

```kotlin
import org.jetbrains.exposed.sql.Table

object Users : Table("users") {
    val id = integer("id").autoIncrement()
    val name = varchar("name", length = 50)
    val email = varchar("email", length = 50).uniqueIndex()

    override val primaryKey = PrimaryKey(id, name = "PK_User_ID")
}
```

## 4. 创建表

定义表结构后，我们需要在数据库中创建相应的表。可以使用 `SchemaUtils.create` 方法来创建表：

```kotlin
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.exposed.sql.transactions.transaction

fun main() {
    Database.connect(
        url = "jdbc:postgresql://localhost:5432/mydatabase",
        driver = "org.postgresql.Driver",
        user = "myuser",
        password = "mypassword"
    )

    transaction {
        SchemaUtils.create(Users)
    }
}
```

## 5. 插入数据

使用 Exposed 插入数据非常简单。可以通过 `insert` 方法将数据插入到表中：

```kotlin
import org.jetbrains.exposed.sql.insert
import org.jetbrains.exposed.sql.transactions.transaction

fun main() {
    Database.connect(
        url = "jdbc:postgresql://localhost:5432/mydatabase",
        driver = "org.postgresql.Driver",
        user = "myuser",
        password = "mypassword"
    )

    transaction {
        Users.insert {
            it[name] = "John Doe"
            it[email] = "john.doe@example.com"
        }
    }
}
```

## 6. 查询数据

Exposed 提供了强大的查询功能。可以使用 `select` 方法查询数据：

```kotlin
import org.jetbrains.exposed.sql.selectAll
import org.jetbrains.exposed.sql.transactions.transaction

fun main() {
    Database.connect(
        url = "jdbc:postgresql://localhost:5432/mydatabase",
        driver = "org.postgresql.Driver",
        user = "myuser",
        password = "mypassword"
    )

    transaction {
        val users = Users.selectAll().map {
            User(
                id = it[Users.id],
                name = it[Users.name],
                email = it[Users.email]
            )
        }

        users.forEach { println(it) }
    }
}

data class User(val id: Int, val name: String, val email: String)
```

## 7. 更新数据

更新数据同样简单。可以使用 `update` 方法来更新表中的数据：

```kotlin
import org.jetbrains.exposed.sql.update
import org.jetbrains.exposed.sql.transactions.transaction

fun main() {
    Database.connect(
        url = "jdbc:postgresql://localhost:5432/mydatabase",
        driver = "org.postgresql.Driver",
        user = "myuser",
        password = "mypassword"
    )

    transaction {
        Users.update({ Users.id eq 1 }) {
            it[name] = "Jane Doe"
        }
    }
}
```

## 8. 删除数据

删除数据可以使用 `deleteWhere` 方法：

```kotlin
import org.jetbrains.exposed.sql.deleteWhere
import org.jetbrains.exposed.sql.transactions.transaction

fun main() {
    Database.connect(
        url = "jdbc:postgresql://localhost:5432/mydatabase",
        driver = "org.postgresql.Driver",
        user = "myuser",
        password = "mypassword"
    )

    transaction {
        Users.deleteWhere { Users.id eq 1 }
    }
}
```

## 9. 实践练习

### 练习 1: 创建一个博客系统

1. 创建一个名为 `Posts` 的表，包含以下字段：
   - `id` (主键，自增)
   - `title` (标题，最大长度 100)
   - `content` (内容，最大长度 1000)
   - `createdAt` (创建时间，使用 `java.time.LocalDateTime`)

2. 插入几条博客文章数据。

3. 查询所有博客文章并打印出来。

4. 更新某篇博客文章的标题。

5. 删除某篇博客文章。

### 练习 2: 创建一个用户管理系统

1. 创建一个名为 `Users` 的表，包含以下字段：
   - `id` (主键，自增)
   - `username` (用户名，最大长度 50)
   - `password` (密码，最大长度 100)
   - `email` (邮箱，最大长度 100)

2. 插入几个用户数据。

3. 查询所有用户并打印出来。

4. 更新某个用户的邮箱。

5. 删除某个用户。

## 总结

通过本教程，我们学习了如何使用 Exposed 进行数据持久化。Exposed 提供了简洁的 API，使得数据库操作变得简单易懂。通过实践练习，你可以进一步巩固所学知识，并将其应用到实际项目中。

希望本教程对你有所帮助，祝你在 Kotlin 编程的道路上越走越远！