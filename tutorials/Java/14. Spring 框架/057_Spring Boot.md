---
title: Spring Boot 入门教程
date: 2023-10-05
description: 本课程将带你从零开始学习Spring Boot，掌握如何快速构建高效、可扩展的Java Web应用程序。
slug: spring-boot-introduction
tags:
  - Spring Boot
  - Java
  - Web开发
category: 编程教程
keywords:
  - Spring Boot 入门
  - Java Web开发
  - Spring Boot教程
---

# Spring Boot 入门

## 1. 概述

Spring Boot 是一个用于简化 Spring 应用程序开发的框架。它通过提供默认配置和自动配置功能，使得开发者能够快速搭建和运行 Spring 应用程序。本教程将带你从零开始，逐步掌握 Spring Boot 的基本概念和使用方法。

## 2. 环境准备

在开始之前，确保你已经安装了以下工具：

- **Java Development Kit (JDK)**: 确保你已经安装了 JDK 8 或更高版本。
- **Maven**: 用于管理项目依赖和构建项目。
- **IDE**: 推荐使用 IntelliJ IDEA 或 Eclipse。

## 3. 创建第一个 Spring Boot 项目

### 3.1 使用 Spring Initializr 创建项目

Spring Initializr 是一个在线工具，可以帮助你快速生成 Spring Boot 项目的基本结构。

1. 访问 [Spring Initializr](https://start.spring.io/)。
2. 选择项目的基本配置：
   - **Project**: Maven Project
   - **Language**: Java
   - **Spring Boot**: 选择最新的稳定版本
   - **Group**: 通常是你的域名反写，例如 `com.example`
   - **Artifact**: 项目的名称，例如 `demo`
   - **Name**: 项目的名称，例如 `demo`
   - **Package Name**: 项目的包名，例如 `com.example.demo`
   - **Packaging**: Jar
   - **Java**: 选择你使用的 Java 版本
3. 在 "Dependencies" 部分，添加以下依赖：
   - **Spring Web**: 用于构建 Web 应用程序
   - **Spring Boot DevTools**: 用于开发时的热部署
4. 点击 "Generate" 按钮，下载生成的项目压缩包。
5. 解压下载的压缩包，并在你的 IDE 中导入项目。

### 3.2 项目结构

导入项目后，你将看到以下目录结构：

```
demo
├── src
│   ├── main
│   │   ├── java
│   │   │   └── com
│   │   │       └── example
│   │   │           └── demo
│   │   │               └── DemoApplication.java
│   │   └── resources
│   │       ├── application.properties
│   │       ├── static
│   │       └── templates
│   └── test
│       └── java
│           └── com
│               └── example
│                   └── demo
│                       └── DemoApplicationTests.java
├── pom.xml
```

- `DemoApplication.java`: 这是 Spring Boot 应用程序的入口类。
- `application.properties`: 用于配置应用程序的属性。
- `pom.xml`: Maven 项目的配置文件，定义了项目的依赖和构建配置。

### 3.3 运行项目

在 IDE 中，右键点击 `DemoApplication.java` 文件，选择 "Run" 或 "Debug" 来启动应用程序。

启动后，你可以在浏览器中访问 `http://localhost:8080`，虽然此时页面会显示 404 错误，因为还没有定义任何控制器。

## 4. 创建一个简单的 Web 控制器

### 4.1 创建控制器类

在 `com.example.demo` 包下创建一个新的 Java 类 `HelloController.java`：

```java
package com.example.demo;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class HelloController {

    @GetMapping("/hello")
    public String sayHello() {
        return "Hello, Spring Boot!";
    }
}
```

### 4.2 解释代码

- `@RestController`: 这是一个组合注解，包含了 `@Controller` 和 `@ResponseBody`，用于标记这是一个 RESTful 控制器。
- `@GetMapping("/hello")`: 用于映射 HTTP GET 请求到指定的方法。
- `sayHello()`: 该方法返回一个字符串，表示响应内容。

### 4.3 测试控制器

重新启动应用程序，然后在浏览器中访问 `http://localhost:8080/hello`，你将看到页面显示 "Hello, Spring Boot!"。

## 5. 配置应用程序

### 5.1 修改端口号

默认情况下，Spring Boot 应用程序运行在 8080 端口。你可以通过修改 `application.properties` 文件来更改端口号：

```properties
server.port=8081
```

重新启动应用程序，访问 `http://localhost:8081/hello`，你将看到相同的响应。

### 5.2 添加自定义属性

你可以在 `application.properties` 中添加自定义属性，并在代码中使用：

```properties
welcome.message=Welcome to Spring Boot!
```

在 `HelloController.java` 中使用该属性：

```java
import org.springframework.beans.factory.annotation.Value;

@RestController
public class HelloController {

    @Value("${welcome.message}")
    private String welcomeMessage;

    @GetMapping("/hello")
    public String sayHello() {
        return welcomeMessage;
    }
}
```

重新启动应用程序，访问 `http://localhost:8081/hello`，你将看到页面显示 "Welcome to Spring Boot!"。

## 6. 实践练习

### 6.1 练习目标

创建一个简单的 Spring Boot 应用程序，包含以下功能：

- 一个控制器，处理 `/greet` 路径的 GET 请求，返回 "Hello, World!"。
- 在 `application.properties` 中定义一个属性 `greeting.message`，并在控制器中使用该属性。

### 6.2 提示

- 创建一个新的控制器类 `GreetController.java`。
- 在 `application.properties` 中添加 `greeting.message` 属性。
- 使用 `@Value` 注解在控制器中注入属性值。

### 6.3 参考答案

```java
package com.example.demo;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GreetController {

    @Value("${greeting.message}")
    private String greetingMessage;

    @GetMapping("/greet")
    public String greet() {
        return greetingMessage;
    }
}
```

在 `application.properties` 中添加：

```properties
greeting.message=Hello, World!
```

重新启动应用程序，访问 `http://localhost:8081/greet`，你将看到页面显示 "Hello, World!"。

## 7. 总结

通过本教程，你已经学会了如何创建一个简单的 Spring Boot 应用程序，并了解了基本的配置和控制器使用方法。Spring Boot 提供了丰富的功能和强大的扩展性，后续你可以继续学习 Spring Boot 的高级特性，如数据库集成、安全配置、RESTful API 开发等。

希望本教程能帮助你快速入门 Spring Boot，并为你的 Java 开发之旅打下坚实的基础。