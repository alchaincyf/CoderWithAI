---
title: 深入理解循环结构：for, while, do-while
date: 2023-10-05
description: 本课程详细讲解编程中的循环结构，包括for循环、while循环和do-while循环的使用方法和应用场景。
slug: understanding-loops-for-while-do-while
tags:
  - 循环
  - for循环
  - while循环
  - do-while循环
category: 编程基础
keywords:
  - 循环结构
  - for循环语法
  - while循环示例
  - do-while循环应用
---

# 循环 (for, while, do-while)

## 1. 概述

循环是编程中非常重要的概念，它允许我们重复执行一段代码，直到满足某个条件为止。Java 提供了三种主要的循环结构：`for` 循环、`while` 循环和 `do-while` 循环。每种循环都有其特定的使用场景和优势。

## 2. for 循环

### 2.1 理论解释

`for` 循环通常用于已知循环次数的情况。它的语法结构如下：

```java
for (初始化; 条件; 更新) {
    // 循环体
}
```

- **初始化**：在循环开始前执行一次，通常用于初始化循环变量。
- **条件**：每次循环开始前都会检查这个条件，如果为 `true`，则继续执行循环体；如果为 `false`，则退出循环。
- **更新**：每次循环结束后执行，通常用于更新循环变量。

### 2.2 代码示例

```java
public class ForLoopExample {
    public static void main(String[] args) {
        for (int i = 0; i < 5; i++) {
            System.out.println("当前值: " + i);
        }
    }
}
```

### 2.3 实践练习

编写一个 `for` 循环，计算并输出 1 到 10 的和。

## 3. while 循环

### 3.1 理论解释

`while` 循环用于在条件为 `true` 时重复执行代码块。它的语法结构如下：

```java
while (条件) {
    // 循环体
}
```

- **条件**：在每次循环开始前检查，如果为 `true`，则执行循环体；如果为 `false`，则退出循环。

### 3.2 代码示例

```java
public class WhileLoopExample {
    public static void main(String[] args) {
        int i = 0;
        while (i < 5) {
            System.out.println("当前值: " + i);
            i++;
        }
    }
}
```

### 3.3 实践练习

编写一个 `while` 循环，输出 1 到 10 之间的所有偶数。

## 4. do-while 循环

### 4.1 理论解释

`do-while` 循环与 `while` 循环类似，但它的循环体至少会执行一次，因为条件是在循环体执行后检查的。它的语法结构如下：

```java
do {
    // 循环体
} while (条件);
```

### 4.2 代码示例

```java
public class DoWhileLoopExample {
    public static void main(String[] args) {
        int i = 0;
        do {
            System.out.println("当前值: " + i);
            i++;
        } while (i < 5);
    }
}
```

### 4.3 实践练习

编写一个 `do-while` 循环，要求用户输入一个数字，直到输入的数字为 0 为止。

## 5. 循环控制语句

### 5.1 break 语句

`break` 语句用于立即退出循环，不再执行循环体内剩余的代码。

```java
public class BreakExample {
    public static void main(String[] args) {
        for (int i = 0; i < 10; i++) {
            if (i == 5) {
                break;
            }
            System.out.println("当前值: " + i);
        }
    }
}
```

### 5.2 continue 语句

`continue` 语句用于跳过当前循环的剩余部分，直接进入下一次循环。

```java
public class ContinueExample {
    public static void main(String[] args) {
        for (int i = 0; i < 10; i++) {
            if (i % 2 == 0) {
                continue;
            }
            System.out.println("当前值: " + i);
        }
    }
}
```

### 5.3 实践练习

编写一个程序，使用 `break` 和 `continue` 语句，输出 1 到 10 之间的所有奇数，但跳过数字 7。

## 6. 总结

循环是编程中非常强大的工具，能够帮助我们高效地处理重复性任务。通过本教程，你应该已经掌握了 `for`、`while` 和 `do-while` 循环的基本用法，并能够使用 `break` 和 `continue` 语句来控制循环的执行流程。

## 7. 下一步

接下来，你将学习跳转语句 (`break`, `continue`, `return`)，这些语句将进一步增强你对循环和条件语句的理解和应用能力。