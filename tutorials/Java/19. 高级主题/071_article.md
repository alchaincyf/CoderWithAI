---
title: 网络编程基础教程
date: 2023-10-05
description: 本课程将带你深入了解网络编程的基础知识，包括TCP/IP协议、HTTP协议、Socket编程等内容，适合初学者和有一定编程基础的开发者。
slug: network-programming-basics
tags:
  - 网络编程
  - TCP/IP
  - HTTP
category: 编程教程
keywords:
  - 网络编程
  - TCP/IP协议
  - HTTP协议
---

# 网络编程

## 概述

网络编程是指编写程序以通过网络进行通信。在Java中，网络编程通常涉及使用`java.net`包中的类和接口来创建客户端和服务器应用程序。通过网络编程，我们可以实现客户端与服务器之间的数据交换，构建分布式系统，以及开发各种网络应用。

## 基本概念

### IP地址和端口号

- **IP地址**：用于唯一标识网络中的设备。常见的IP地址有IPv4（如`192.168.1.1`）和IPv6（如`2001:0db8:85a3:0000:0000:8a2e:0370:7334`）。
- **端口号**：用于标识设备上的特定进程或服务。端口号范围从0到65535，其中0到1023为系统保留端口。

### 套接字（Socket）

套接字是网络编程中的基本概念，它代表一个通信端点。套接字允许两个不同的进程在网络上进行通信。在Java中，`Socket`类用于客户端，`ServerSocket`类用于服务器端。

## 创建简单的客户端-服务器应用程序

### 服务器端代码

```java
import java.io.*;
import java.net.*;

public class SimpleServer {
    public static void main(String[] args) {
        try (ServerSocket serverSocket = new ServerSocket(6666)) {
            System.out.println("Server is listening on port 6666");
            Socket socket = serverSocket.accept();
            System.out.println("New client connected");

            InputStream input = socket.getInputStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(input));

            String message = reader.readLine();
            System.out.println("Message from client: " + message);

            OutputStream output = socket.getOutputStream();
            PrintWriter writer = new PrintWriter(output, true);
            writer.println("Hello Client, Server received your message: " + message);

        } catch (IOException ex) {
            System.out.println("Server exception: " + ex.getMessage());
            ex.printStackTrace();
        }
    }
}
```

### 客户端代码

```java
import java.io.*;
import java.net.*;

public class SimpleClient {
    public static void main(String[] args) {
        try (Socket socket = new Socket("localhost", 6666)) {
            OutputStream output = socket.getOutputStream();
            PrintWriter writer = new PrintWriter(output, true);
            writer.println("Hello Server");

            InputStream input = socket.getInputStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(input));

            String response = reader.readLine();
            System.out.println("Server response: " + response);

        } catch (UnknownHostException ex) {
            System.out.println("Server not found: " + ex.getMessage());
        } catch (IOException ex) {
            System.out.println("I/O error: " + ex.getMessage());
        }
    }
}
```

### 运行步骤

1. 首先运行服务器端代码。服务器将开始监听端口6666。
2. 然后运行客户端代码。客户端将连接到服务器并发送消息。
3. 服务器接收到消息后，会发送响应给客户端。

## 实践练习

### 练习1：扩展服务器功能

扩展服务器代码，使其能够处理多个客户端连接。提示：使用多线程。

### 练习2：文件传输

编写一个客户端-服务器应用程序，客户端可以上传文件到服务器，服务器接收并保存文件。

## 总结

网络编程是构建分布式系统和网络应用的基础。通过Java的`java.net`包，我们可以轻松地创建客户端和服务器应用程序。理解IP地址、端口号和套接字是网络编程的关键。通过实践练习，你可以进一步掌握网络编程的技巧。