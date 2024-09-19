---
title: 深入理解Java Stream API
date: 2023-10-05
description: 本课程将深入探讨Java Stream API的使用，包括流的操作、中间操作和终端操作，以及如何高效地处理集合数据。
slug: java-stream-api-tutorial
tags:
  - Java
  - Stream API
  - 函数式编程
category: 编程教程
keywords:
  - Java Stream API
  - 流操作
  - 函数式编程
---

# Stream API 教程

## 概述

Stream API 是 Java 8 引入的一个强大的工具，用于处理集合数据。它提供了一种声明式的方式来处理数据，使得代码更加简洁和易读。Stream API 可以与 Lambda 表达式结合使用，进一步提高代码的可读性和简洁性。

## 1. Stream API 的基本概念

### 1.1 什么是 Stream？

Stream 是一个来自数据源的元素队列，并支持聚合操作。数据源可以是集合、数组或其他数据结构。Stream 操作可以是中间操作（返回另一个 Stream）或终端操作（返回结果或副作用）。

### 1.2 Stream 的特点

- **声明式**：Stream API 允许你以声明式的方式描述你想要做什么，而不是如何做。
- **惰性求值**：许多 Stream 操作是惰性的，只有在终端操作被调用时才会执行。
- **并行处理**：Stream 可以轻松地并行处理数据，提高处理速度。

## 2. Stream 的创建

### 2.1 从集合创建 Stream

你可以从 `Collection` 接口的实现类（如 `List`、`Set`）中创建 Stream：

```java
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

public class StreamExample {
    public static void main(String[] args) {
        List<String> list = Arrays.asList("a", "b", "c");
        Stream<String> stream = list.stream();
    }
}
```

### 2.2 从数组创建 Stream

你可以使用 `Arrays.stream()` 方法从数组创建 Stream：

```java
import java.util.stream.Stream;

public class StreamExample {
    public static void main(String[] args) {
        String[] array = {"a", "b", "c"};
        Stream<String> stream = Arrays.stream(array);
    }
}
```

### 2.3 使用 `Stream.of()` 创建 Stream

你可以使用 `Stream.of()` 方法创建一个包含指定元素的 Stream：

```java
import java.util.stream.Stream;

public class StreamExample {
    public static void main(String[] args) {
        Stream<String> stream = Stream.of("a", "b", "c");
    }
}
```

## 3. Stream 的中间操作

### 3.1 `filter()`

`filter()` 方法用于过滤 Stream 中的元素，返回一个包含符合条件的元素的 Stream：

```java
import java.util.Arrays;
import java.util.List;

public class StreamExample {
    public static void main(String[] args) {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        numbers.stream()
               .filter(n -> n % 2 == 0)
               .forEach(System.out::println); // 输出 2, 4
    }
}
```

### 3.2 `map()`

`map()` 方法用于将 Stream 中的每个元素映射为另一个元素，返回一个包含映射后元素的 Stream：

```java
import java.util.Arrays;
import java.util.List;

public class StreamExample {
    public static void main(String[] args) {
        List<String> words = Arrays.asList("hello", "world");
        words.stream()
             .map(String::toUpperCase)
             .forEach(System.out::println); // 输出 HELLO, WORLD
    }
}
```

### 3.3 `sorted()`

`sorted()` 方法用于对 Stream 中的元素进行排序，返回一个排序后的 Stream：

```java
import java.util.Arrays;
import java.util.List;

public class StreamExample {
    public static void main(String[] args) {
        List<Integer> numbers = Arrays.asList(5, 3, 1, 4, 2);
        numbers.stream()
               .sorted()
               .forEach(System.out::println); // 输出 1, 2, 3, 4, 5
    }
}
```

## 4. Stream 的终端操作

### 4.1 `forEach()`

`forEach()` 方法用于对 Stream 中的每个元素执行指定的操作：

```java
import java.util.Arrays;
import java.util.List;

public class StreamExample {
    public static void main(String[] args) {
        List<String> words = Arrays.asList("hello", "world");
        words.stream()
             .forEach(System.out::println); // 输出 hello, world
    }
}
```

### 4.2 `collect()`

`collect()` 方法用于将 Stream 中的元素收集到一个集合中：

```java
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class StreamExample {
    public static void main(String[] args) {
        List<String> words = Arrays.asList("hello", "world");
        List<String> upperCaseWords = words.stream()
                                           .map(String::toUpperCase)
                                           .collect(Collectors.toList());
        System.out.println(upperCaseWords); // 输出 [HELLO, WORLD]
    }
}
```

### 4.3 `reduce()`

`reduce()` 方法用于将 Stream 中的元素归约为一个值：

```java
import java.util.Arrays;
import java.util.List;

public class StreamExample {
    public static void main(String[] args) {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        int sum = numbers.stream()
                         .reduce(0, (a, b) -> a + b);
        System.out.println(sum); // 输出 15
    }
}
```

## 5. 实践练习

### 练习 1：过滤和映射

编写一个程序，从一个包含整数的列表中过滤出偶数，并将这些偶数平方后输出。

```java
import java.util.Arrays;
import java.util.List;

public class StreamExercise {
    public static void main(String[] args) {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6);
        numbers.stream()
               .filter(n -> n % 2 == 0)
               .map(n -> n * n)
               .forEach(System.out::println); // 输出 4, 16, 36
    }
}
```

### 练习 2：排序和收集

编写一个程序，从一个包含字符串的列表中按字符串长度排序，并将排序后的结果收集到一个新的列表中。

```java
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class StreamExercise {
    public static void main(String[] args) {
        List<String> words = Arrays.asList("apple", "banana", "cherry", "date");
        List<String> sortedWords = words.stream()
                                        .sorted((a, b) -> Integer.compare(a.length(), b.length()))
                                        .collect(Collectors.toList());
        System.out.println(sortedWords); // 输出 [date, apple, banana, cherry]
    }
}
```

## 6. 总结

Stream API 是 Java 中处理集合数据的一个强大工具。通过 Stream API，你可以以声明式的方式处理数据，使得代码更加简洁和易读。Stream API 还支持惰性求值和并行处理，进一步提高了代码的效率。

通过本教程的学习，你应该已经掌握了 Stream API 的基本概念、创建方式、中间操作和终端操作，并能够编写简单的 Stream 处理程序。继续练习和探索，你将能够更加熟练地使用 Stream API 来处理各种数据处理任务。