---
title: Java 简介和特性
date: 2023-10-05
description: 本课程介绍Java编程语言的基础知识和主要特性，包括面向对象编程、平台无关性、多线程支持等。
slug: java-introduction-features
tags:
  - Java
  - 编程基础
  - 面向对象编程
category: 编程语言
keywords:
  - Java 简介
  - Java 特性
  - 面向对象编程
---

# Java 简介和特性

## 1. Java 简介

Java 是一种广泛使用的编程语言，最初由 Sun Microsystems 在 1995 年发布。Java 的设计目标是“一次编写，到处运行”（Write Once, Run Anywhere），这意味着 Java 程序可以在任何支持 Java 的平台上运行，而无需重新编译。

### 1.1 Java 的历史

- **1991**: Sun Microsystems 的 James Gosling 领导了一个团队，开始开发一种名为 Oak 的语言，后来改名为 Java。
- **1995**: Java 1.0 发布，标志着 Java 的正式诞生。
- **2009**: Oracle 收购 Sun Microsystems，Java 成为 Oracle 的产品。
- **2014**: Java 8 发布，引入了 Lambda 表达式和 Stream API 等新特性。
- **2018**: Java 11 发布，成为长期支持版本（LTS）。

### 1.2 Java 的应用领域

Java 广泛应用于以下领域：
- **企业级应用**: 如大型企业管理系统、电子商务平台。
- **移动应用**: Android 应用开发。
- **桌面应用**: 如 Eclipse、IntelliJ IDEA 等开发工具。
- **Web 应用**: 如 Spring Boot、Servlet 和 JSP。
- **嵌入式系统**: 如智能卡、传感器网络。

## 2. Java 的特性

### 2.1 面向对象

Java 是一种面向对象的编程语言，支持封装、继承和多态等面向对象的特性。

```java
class Animal {
    void sound() {
        System.out.println("Animal makes a sound");
    }
}

class Dog extends Animal {
    void sound() {
        System.out.println("Dog barks");
    }
}

public class Main {
    public static void main(String[] args) {
        Animal myDog = new Dog();
        myDog.sound(); // 输出: Dog barks
    }
}
```

### 2.2 平台无关性

Java 程序编译成字节码，可以在任何支持 Java 虚拟机（JVM）的平台上运行。

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

### 2.3 自动内存管理

Java 通过垃圾回收机制自动管理内存，开发者无需手动释放内存。

```java
public class MemoryManagement {
    public static void main(String[] args) {
        String str = new String("Hello");
        str = null; // 不再使用的对象会被垃圾回收
    }
}
```

### 2.4 多线程支持

Java 内置多线程支持，可以轻松创建和管理线程。

```java
public class MyThread extends Thread {
    public void run() {
        System.out.println("Thread is running");
    }

    public static void main(String[] args) {
        MyThread thread = new MyThread();
        thread.start();
    }
}
```

### 2.5 丰富的标准库

Java 提供了丰富的标准库，涵盖了从基本数据结构到网络编程的各个方面。

```java
import java.util.ArrayList;

public class StandardLibrary {
    public static void main(String[] args) {
        ArrayList<String> list = new ArrayList<>();
        list.add("Java");
        list.add("Python");
        System.out.println(list); // 输出: [Java, Python]
    }
}
```

## 3. 实践练习

### 3.1 编写一个简单的 Java 程序

编写一个 Java 程序，输出“Hello, Java!”。

```java
public class HelloJava {
    public static void main(String[] args) {
        System.out.println("Hello, Java!");
    }
}
```

### 3.2 创建一个类和对象

创建一个 `Person` 类，包含姓名和年龄属性，并创建一个对象。

```java
class Person {
    String name;
    int age;

    Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    void display() {
        System.out.println("Name: " + name + ", Age: " + age);
    }
}

public class Main {
    public static void main(String[] args) {
        Person person = new Person("Alice", 30);
        person.display(); // 输出: Name: Alice, Age: 30
    }
}
```

## 4. 总结

Java 是一种功能强大且广泛使用的编程语言，具有面向对象、平台无关性、自动内存管理、多线程支持和丰富的标准库等特性。通过本教程，您应该对 Java 有了初步的了解，并能够编写简单的 Java 程序。

在接下来的课程中，我们将深入学习 Java 的各个方面，包括 JDK 安装和环境配置、基本语法和数据类型、运算符和表达式、条件语句、循环、类和对象、方法和构造器等。