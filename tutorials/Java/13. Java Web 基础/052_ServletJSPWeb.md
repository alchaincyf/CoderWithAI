---
title: 深入理解Servlet与JSP：构建动态Web应用
date: 2023-10-05
description: 本课程详细讲解Servlet和JSP的基础知识与高级应用，帮助你掌握如何使用Java构建动态Web应用程序。
slug: servlet-jsp-tutorial
tags:
  - Java
  - Web开发
  - Servlet
  - JSP
category: 编程教程
keywords:
  - Servlet
  - JSP
  - Java Web开发
  - 动态Web应用
---

# Servlet 和 JSP 教程

## 1. 概述

### 1.1 什么是 Servlet？
Servlet 是 Java 编写的服务器端程序，用于处理客户端请求并生成动态内容。Servlet 运行在 Web 服务器上，通常与 HTTP 协议一起使用。

### 1.2 什么是 JSP？
JSP（JavaServer Pages）是一种用于创建动态 Web 内容的 Java 技术。JSP 页面在服务器端被编译成 Servlet，然后执行。

## 2. 环境设置

### 2.1 安装 Tomcat
Tomcat 是一个开源的 Web 容器，支持 Servlet 和 JSP。你可以从 [Apache Tomcat 官网](https://tomcat.apache.org/) 下载并安装。

### 2.2 配置环境变量
确保你的 `JAVA_HOME` 和 `CATALINA_HOME` 环境变量已正确配置。

## 3. 第一个 Servlet 程序

### 3.1 创建 Servlet 类
```java
import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@WebServlet("/HelloServlet")
public class HelloServlet extends HttpServlet {
    protected void doGet(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        response.setContentType("text/html");
        PrintWriter out = response.getWriter();
        out.println("<h1>Hello, Servlet!</h1>");
    }
}
```

### 3.2 部署和运行
1. 将编译后的 `.class` 文件放入 `WEB-INF/classes` 目录。
2. 启动 Tomcat 服务器。
3. 访问 `http://localhost:8080/yourapp/HelloServlet`。

## 4. 第一个 JSP 页面

### 4.1 创建 JSP 文件
```jsp
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<!DOCTYPE html>
<html>
<head>
    <title>Hello JSP</title>
</head>
<body>
    <h1>Hello, JSP!</h1>
</body>
</html>
```

### 4.2 部署和运行
1. 将 JSP 文件放入 Web 应用的根目录。
2. 启动 Tomcat 服务器。
3. 访问 `http://localhost:8080/yourapp/hello.jsp`。

## 5. Servlet 生命周期

### 5.1 生命周期方法
- `init()`: 初始化 Servlet。
- `service()`: 处理客户端请求。
- `destroy()`: 销毁 Servlet。

### 5.2 示例代码
```java
public class LifeCycleServlet extends HttpServlet {
    public void init() {
        System.out.println("Servlet initialized.");
    }

    protected void service(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        System.out.println("Service method called.");
    }

    public void destroy() {
        System.out.println("Servlet destroyed.");
    }
}
```

## 6. JSP 内置对象

### 6.1 常用内置对象
- `request`: 客户端请求信息。
- `response`: 服务器响应信息。
- `out`: 输出流。
- `session`: 会话信息。
- `application`: 应用上下文。

### 6.2 示例代码
```jsp
<%
    String user = request.getParameter("user");
    out.println("Hello, " + user);
    session.setAttribute("user", user);
%>
```

## 7. 实践练习

### 7.1 练习 1: 创建一个简单的登录页面
1. 创建一个 JSP 页面用于输入用户名和密码。
2. 创建一个 Servlet 处理登录请求，验证用户名和密码。
3. 如果验证成功，重定向到欢迎页面；否则，显示错误信息。

### 7.2 练习 2: 实现一个简单的购物车
1. 创建一个 JSP 页面显示商品列表。
2. 创建一个 Servlet 处理添加商品到购物车的请求。
3. 使用 `session` 对象存储购物车信息。

## 8. 总结

通过本教程，你已经学习了 Servlet 和 JSP 的基本概念、生命周期、内置对象以及如何创建简单的 Web 应用。继续深入学习，你将能够构建更复杂的 Web 应用。

## 9. 参考资料

- [Apache Tomcat 官方文档](https://tomcat.apache.org/tomcat-9.0-doc/index.html)
- [Java Servlet 教程](https://www.javatpoint.com/servlet-tutorial)
- [JSP 教程](https://www.tutorialspoint.com/jsp/index.htm)

希望这篇教程对你有所帮助，祝你在 Servlet 和 JSP 的学习中取得进步！