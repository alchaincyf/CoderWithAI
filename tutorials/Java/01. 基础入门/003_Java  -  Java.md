---
title: 第一个 Java 程序 - 从零开始学习 Java
date: 2023-10-05
description: 本课程将指导你如何编写并运行你的第一个 Java 程序，从设置开发环境到编写简单的 "Hello, World!" 程序。
slug: first-java-program
tags:
  - Java
  - 编程入门
  - 初学者
category: 编程基础
keywords:
  - Java 程序
  - Hello World
  - Java 入门
---

# 第一个 Java 程序

## 概述

在本教程中，我们将编写并运行我们的第一个 Java 程序。这个程序将非常简单，但它将帮助你理解 Java 程序的基本结构和如何编译、运行 Java 程序。

## 1. Java 程序的基本结构

一个典型的 Java 程序包含以下几个部分：

1. **包声明**（可选）：用于组织类和接口。
2. **导入语句**（可选）：用于引入其他包中的类和接口。
3. **类定义**：Java 程序的基本单位是类。每个 Java 程序至少包含一个类。
4. **主方法**（`main` 方法）：程序的入口点。

## 2. 编写第一个 Java 程序

### 2.1 创建一个简单的 Java 类

让我们创建一个名为 `HelloWorld` 的类，并在其中编写一个简单的 `main` 方法。

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

### 2.2 代码解释

- `public class HelloWorld`：定义了一个名为 `HelloWorld` 的公共类。
- `public static void main(String[] args)`：这是 Java 程序的入口点。`main` 方法是每个 Java 应用程序必须包含的方法。
- `System.out.println("Hello, World!");`：这行代码会在控制台输出 `Hello, World!`。

## 3. 编译和运行 Java 程序

### 3.1 编译 Java 程序

要编译 Java 程序，你需要使用 `javac` 命令。假设你已经将上面的代码保存为 `HelloWorld.java` 文件，你可以使用以下命令来编译它：

```bash
javac HelloWorld.java
```

编译成功后，会生成一个名为 `HelloWorld.class` 的文件，这是 Java 字节码文件。

### 3.2 运行 Java 程序

要运行编译后的 Java 程序，你需要使用 `java` 命令。运行以下命令：

```bash
java HelloWorld
```

你应该会在控制台看到输出：

```
Hello, World!
```

## 4. 实践练习

### 4.1 练习 1：修改输出内容

修改 `HelloWorld` 类中的 `System.out.println` 语句，使其输出你自己的名字。例如：

```java
System.out.println("Hello, Alice!");
```

然后重新编译并运行程序，看看输出是否正确。

### 4.2 练习 2：添加更多输出

在 `main` 方法中添加更多的 `System.out.println` 语句，输出多行文本。例如：

```java
System.out.println("Hello, Alice!");
System.out.println("Welcome to Java Programming!");
```

重新编译并运行程序，观察输出结果。

## 5. 总结

通过本教程，你已经成功编写并运行了你的第一个 Java 程序。你学习了 Java 程序的基本结构，如何编写 `main` 方法，以及如何使用 `javac` 和 `java` 命令来编译和运行 Java 程序。

在接下来的教程中，我们将深入探讨 Java 的基本语法、数据类型、运算符、条件语句、循环等内容。继续学习，你将逐步掌握 Java 编程的更多知识和技能。

## 6. 下一步

- 学习 Java 的基本语法和数据类型。
- 探索 Java 的运算符和表达式。
- 了解 Java 的条件语句和循环结构。

希望你喜欢这个教程，并继续探索 Java 编程的世界！