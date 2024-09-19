---
title: 深入理解通配符：编程中的强大工具
date: 2023-10-05
description: 本课程将深入探讨通配符在编程中的应用，包括其在文件操作、正则表达式和命令行中的使用，帮助你掌握这一强大工具。
slug: wildcard-programming-tutorial
tags:
  - 编程基础
  - 通配符
  - 文件操作
category: 编程技巧
keywords:
  - 通配符
  - 编程
  - 文件匹配
---

# 通配符

## 1. 概述

在 Java 编程中，通配符（Wildcard）是泛型编程的一个重要概念。通配符允许我们在泛型代码中编写更灵活和通用的代码。通配符主要有三种类型：无界通配符、上界通配符和下界通配符。

## 2. 无界通配符

### 2.1 理论解释

无界通配符使用 `?` 表示，它表示任何类型的参数。无界通配符通常用于当我们不关心泛型参数的具体类型时。

### 2.2 代码示例

```java
import java.util.List;

public class WildcardExample {
    public static void printList(List<?> list) {
        for (Object elem : list) {
            System.out.println(elem);
        }
    }

    public static void main(String[] args) {
        List<Integer> intList = List.of(1, 2, 3);
        List<String> strList = List.of("A", "B", "C");

        printList(intList); // 输出: 1 2 3
        printList(strList); // 输出: A B C
    }
}
```

### 2.3 实践练习

编写一个方法 `sumOfList`，该方法接受一个 `List<?>` 参数，并计算列表中所有元素的和。注意，由于通配符的限制，你可能需要使用反射或其他技术来实现。

## 3. 上界通配符

### 3.1 理论解释

上界通配符使用 `? extends T` 表示，它表示类型参数必须是 `T` 或 `T` 的子类。上界通配符通常用于当我们需要确保泛型参数是某个特定类型的子类型时。

### 3.2 代码示例

```java
import java.util.List;

public class UpperBoundedWildcardExample {
    public static double sumOfList(List<? extends Number> list) {
        double sum = 0.0;
        for (Number num : list) {
            sum += num.doubleValue();
        }
        return sum;
    }

    public static void main(String[] args) {
        List<Integer> intList = List.of(1, 2, 3);
        List<Double> doubleList = List.of(1.5, 2.5, 3.5);

        System.out.println(sumOfList(intList)); // 输出: 6.0
        System.out.println(sumOfList(doubleList)); // 输出: 7.5
    }
}
```

### 3.3 实践练习

编写一个方法 `averageOfList`，该方法接受一个 `List<? extends Number>` 参数，并计算列表中所有元素的平均值。

## 4. 下界通配符

### 4.1 理论解释

下界通配符使用 `? super T` 表示，它表示类型参数必须是 `T` 或 `T` 的父类。下界通配符通常用于当我们需要确保泛型参数是某个特定类型的父类型时。

### 4.2 代码示例

```java
import java.util.List;

public class LowerBoundedWildcardExample {
    public static void addNumbers(List<? super Integer> list) {
        for (int i = 1; i <= 10; i++) {
            list.add(i);
        }
    }

    public static void main(String[] args) {
        List<Number> numberList = new ArrayList<>();
        List<Object> objectList = new ArrayList<>();

        addNumbers(numberList);
        addNumbers(objectList);

        System.out.println(numberList); // 输出: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        System.out.println(objectList); // 输出: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    }
}
```

### 4.3 实践练习

编写一个方法 `addAll`，该方法接受一个 `List<? super T>` 参数和一个 `T` 类型的元素，并将该元素添加到列表中。

## 5. 通配符捕获

### 5.1 理论解释

通配符捕获是指在泛型方法中使用通配符时，编译器会自动推断出通配符的具体类型。通配符捕获通常用于当我们需要对通配符类型的参数进行操作时。

### 5.2 代码示例

```java
import java.util.List;

public class WildcardCaptureExample {
    public static void reverse(List<?> list) {
        reverseHelper(list);
    }

    private static <T> void reverseHelper(List<T> list) {
        List<T> temp = new ArrayList<>(list);
        list.clear();
        for (int i = temp.size() - 1; i >= 0; i--) {
            list.add(temp.get(i));
        }
    }

    public static void main(String[] args) {
        List<Integer> intList = List.of(1, 2, 3);
        reverse(intList);
        System.out.println(intList); // 输出: [3, 2, 1]
    }
}
```

### 5.3 实践练习

编写一个方法 `swap`，该方法接受一个 `List<?>` 参数和两个索引，并交换列表中这两个索引处的元素。

## 6. 总结

通配符是 Java 泛型编程中的一个强大工具，它允许我们编写更灵活和通用的代码。通过使用无界通配符、上界通配符和下界通配符，我们可以更好地控制泛型参数的类型，从而提高代码的可读性和可维护性。

## 7. 进一步学习

- 深入学习 Java 泛型的高级特性，如类型推断和类型擦除。
- 探索 Java 集合框架中的泛型使用，了解如何使用通配符来处理集合。
- 研究 Java 8 引入的 `Stream API` 和 `Optional` 类，了解它们如何利用泛型和通配符来简化代码。

通过不断实践和学习，你将能够更好地掌握通配符的使用，并在实际项目中灵活应用。