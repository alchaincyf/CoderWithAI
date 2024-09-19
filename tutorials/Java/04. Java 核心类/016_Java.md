---
title: Java包装类详解
date: 2023-10-05
description: 本课程详细讲解Java中的包装类，包括其定义、使用场景以及与基本数据类型的转换。
slug: java-wrapper-classes
tags:
  - Java
  - 包装类
  - 数据类型
category: 编程基础
keywords:
  - Java包装类
  - 基本数据类型
  - 自动装箱与拆箱
---

# 包装类

## 概述

在 Java 中，基本数据类型（如 `int`, `char`, `boolean` 等）不是对象，因此它们不能直接参与面向对象的操作，例如存储在集合中或作为方法的参数传递。为了解决这个问题，Java 提供了包装类（Wrapper Classes），它们是基本数据类型的对象表示形式。每个基本数据类型都有一个对应的包装类。

## 包装类列表

| 基本数据类型 | 包装类       |
|--------------|--------------|
| `byte`       | `Byte`       |
| `short`      | `Short`      |
| `int`        | `Integer`    |
| `long`       | `Long`       |
| `float`      | `Float`      |
| `double`     | `Double`     |
| `char`       | `Character`  |
| `boolean`    | `Boolean`    |

## 自动装箱和拆箱

Java 提供了自动装箱（Autoboxing）和拆箱（Unboxing）的功能，使得基本数据类型和其对应的包装类之间的转换变得非常方便。

### 自动装箱

自动装箱是指将基本数据类型自动转换为其对应的包装类对象。

```java
int primitiveInt = 42;
Integer wrappedInt = primitiveInt; // 自动装箱
```

### 自动拆箱

自动拆箱是指将包装类对象自动转换为其对应的基本数据类型。

```java
Integer wrappedInt = 42;
int primitiveInt = wrappedInt; // 自动拆箱
```

## 常用方法

包装类提供了许多有用的方法来操作基本数据类型。以下是一些常用的方法：

### `Integer` 类

- `intValue()`: 返回 `Integer` 对象的 `int` 值。
- `parseInt(String s)`: 将字符串解析为 `int` 值。
- `toString()`: 返回 `Integer` 对象的字符串表示。

```java
Integer num = 100;
int intValue = num.intValue(); // 100
String strValue = num.toString(); // "100"
int parsedInt = Integer.parseInt("200"); // 200
```

### `Double` 类

- `doubleValue()`: 返回 `Double` 对象的 `double` 值。
- `parseDouble(String s)`: 将字符串解析为 `double` 值。
- `toString()`: 返回 `Double` 对象的字符串表示。

```java
Double dbl = 3.14;
double doubleValue = dbl.doubleValue(); // 3.14
String strValue = dbl.toString(); // "3.14"
double parsedDouble = Double.parseDouble("2.718"); // 2.718
```

## 实践练习

### 练习 1: 自动装箱和拆箱

编写一个程序，展示自动装箱和拆箱的过程。

```java
public class AutoboxingUnboxing {
    public static void main(String[] args) {
        // 自动装箱
        int primitiveInt = 42;
        Integer wrappedInt = primitiveInt;
        System.out.println("自动装箱: " + wrappedInt);

        // 自动拆箱
        Integer anotherWrappedInt = 99;
        int anotherPrimitiveInt = anotherWrappedInt;
        System.out.println("自动拆箱: " + anotherPrimitiveInt);
    }
}
```

### 练习 2: 使用包装类方法

编写一个程序，使用 `Integer` 和 `Double` 类的常用方法。

```java
public class WrapperMethods {
    public static void main(String[] args) {
        // 使用 Integer 类的方法
        Integer num = 100;
        int intValue = num.intValue();
        String strValue = num.toString();
        int parsedInt = Integer.parseInt("200");

        System.out.println("intValue: " + intValue);
        System.out.println("strValue: " + strValue);
        System.out.println("parsedInt: " + parsedInt);

        // 使用 Double 类的方法
        Double dbl = 3.14;
        double doubleValue = dbl.doubleValue();
        String dblStrValue = dbl.toString();
        double parsedDouble = Double.parseDouble("2.718");

        System.out.println("doubleValue: " + doubleValue);
        System.out.println("dblStrValue: " + dblStrValue);
        System.out.println("parsedDouble: " + parsedDouble);
    }
}
```

## 总结

包装类在 Java 中扮演着重要的角色，它们允许基本数据类型以对象的形式存在，从而可以参与更多的面向对象操作。通过自动装箱和拆箱，Java 使得基本数据类型和包装类之间的转换变得非常简单。掌握包装类的使用，对于理解和编写高效的 Java 代码至关重要。

希望这篇教程能够帮助你更好地理解 Java 中的包装类。继续练习和探索，你将能够更加熟练地使用这些强大的工具。