---
title: Java 集合框架：List, Set, Map 接口详解
date: 2023-10-05
description: 本课程详细讲解Java中的List, Set, Map接口及其常用实现类，帮助你掌握集合框架的核心概念和使用方法。
slug: java-collections-list-set-map
tags:
  - Java
  - 集合框架
  - 数据结构
category: 编程语言
keywords:
  - Java List
  - Java Set
  - Java Map
  - 集合框架
  - 数据结构
---

# List, Set, Map 接口

## 概述

在 Java 中，`List`、`Set` 和 `Map` 是集合框架中的三个主要接口。它们分别用于不同的数据存储需求：

- **List**: 有序集合，允许重复元素。
- **Set**: 无序集合，不允许重复元素。
- **Map**: 键值对集合，键不允许重复。

## List 接口

### 理论解释

`List` 接口继承自 `Collection` 接口，表示一个有序的元素集合。`List` 允许重复元素，并且可以通过索引访问元素。

### 代码示例

```java
import java.util.ArrayList;
import java.util.List;

public class ListExample {
    public static void main(String[] args) {
        // 创建一个 ArrayList 实例
        List<String> fruits = new ArrayList<>();

        // 添加元素
        fruits.add("Apple");
        fruits.add("Banana");
        fruits.add("Cherry");

        // 访问元素
        System.out.println("First fruit: " + fruits.get(0));

        // 遍历 List
        for (String fruit : fruits) {
            System.out.println(fruit);
        }
    }
}
```

### 实践练习

1. 创建一个 `List` 存储整数，并尝试添加、删除和访问元素。
2. 使用 `for` 循环和 `Iterator` 遍历 `List`。

## Set 接口

### 理论解释

`Set` 接口也继承自 `Collection` 接口，表示一个无序的元素集合。`Set` 不允许重复元素。

### 代码示例

```java
import java.util.HashSet;
import java.util.Set;

public class SetExample {
    public static void main(String[] args) {
        // 创建一个 HashSet 实例
        Set<String> uniqueFruits = new HashSet<>();

        // 添加元素
        uniqueFruits.add("Apple");
        uniqueFruits.add("Banana");
        uniqueFruits.add("Cherry");
        uniqueFruits.add("Apple"); // 重复元素不会被添加

        // 遍历 Set
        for (String fruit : uniqueFruits) {
            System.out.println(fruit);
        }
    }
}
```

### 实践练习

1. 创建一个 `Set` 存储字符串，并尝试添加、删除和检查元素是否存在。
2. 使用 `for` 循环和 `Iterator` 遍历 `Set`。

## Map 接口

### 理论解释

`Map` 接口表示键值对的集合，每个键对应一个值。`Map` 不允许重复键，但允许键对应的值重复。

### 代码示例

```java
import java.util.HashMap;
import java.util.Map;

public class MapExample {
    public static void main(String[] args) {
        // 创建一个 HashMap 实例
        Map<String, Integer> fruitCount = new HashMap<>();

        // 添加键值对
        fruitCount.put("Apple", 5);
        fruitCount.put("Banana", 3);
        fruitCount.put("Cherry", 7);

        // 访问值
        System.out.println("Number of Apples: " + fruitCount.get("Apple"));

        // 遍历 Map
        for (Map.Entry<String, Integer> entry : fruitCount.entrySet()) {
            System.out.println(entry.getKey() + ": " + entry.getValue());
        }
    }
}
```

### 实践练习

1. 创建一个 `Map` 存储学生姓名和对应的成绩，并尝试添加、删除和访问键值对。
2. 使用 `for` 循环和 `Iterator` 遍历 `Map`。

## 总结

`List`、`Set` 和 `Map` 是 Java 集合框架中的三个核心接口，分别适用于不同的数据存储需求。通过本教程，你应该能够理解它们的区别，并能够在实际编程中使用它们。

## 下一步

接下来，我们将深入学习 `ArrayList` 和 `LinkedList`、`HashSet` 和 `TreeSet`、`HashMap` 和 `TreeMap` 等具体实现类，以及集合的排序和查找操作。