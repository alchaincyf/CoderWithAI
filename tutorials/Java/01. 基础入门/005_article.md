---
title: 运算符和表达式详解
date: 2023-10-05
description: 本课程详细讲解编程中的运算符和表达式，包括算术运算符、比较运算符、逻辑运算符等，帮助你掌握编程基础。
slug: operators-and-expressions
tags:
  - 编程基础
  - 运算符
  - 表达式
category: 编程入门
keywords:
  - 运算符
  - 表达式
  - 编程基础
---

# 运算符和表达式

## 概述

在编程中，运算符和表达式是构建程序逻辑的基础。运算符用于执行各种操作，如算术运算、比较运算、逻辑运算等。表达式则是由运算符和操作数组成的，用于计算出某个值。理解运算符和表达式是掌握编程语言的关键一步。

## 运算符的类型

Java 提供了多种类型的运算符，主要包括以下几类：

### 1. 算术运算符

算术运算符用于执行基本的数学运算。

- `+`：加法
- `-`：减法
- `*`：乘法
- `/`：除法
- `%`：取模（求余数）

**代码示例：**

```java
public class ArithmeticOperators {
    public static void main(String[] args) {
        int a = 10;
        int b = 3;

        System.out.println("a + b = " + (a + b)); // 输出：a + b = 13
        System.out.println("a - b = " + (a - b)); // 输出：a - b = 7
        System.out.println("a * b = " + (a * b)); // 输出：a * b = 30
        System.out.println("a / b = " + (a / b)); // 输出：a / b = 3
        System.out.println("a % b = " + (a % b)); // 输出：a % b = 1
    }
}
```

### 2. 关系运算符

关系运算符用于比较两个值，返回布尔值 `true` 或 `false`。

- `==`：等于
- `!=`：不等于
- `>`：大于
- `<`：小于
- `>=`：大于等于
- `<=`：小于等于

**代码示例：**

```java
public class RelationalOperators {
    public static void main(String[] args) {
        int x = 5;
        int y = 8;

        System.out.println("x == y: " + (x == y)); // 输出：x == y: false
        System.out.println("x != y: " + (x != y)); // 输出：x != y: true
        System.out.println("x > y: " + (x > y));   // 输出：x > y: false
        System.out.println("x < y: " + (x < y));   // 输出：x < y: true
        System.out.println("x >= y: " + (x >= y)); // 输出：x >= y: false
        System.out.println("x <= y: " + (x <= y)); // 输出：x <= y: true
    }
}
```

### 3. 逻辑运算符

逻辑运算符用于组合多个布尔表达式。

- `&&`：逻辑与（AND）
- `||`：逻辑或（OR）
- `!`：逻辑非（NOT）

**代码示例：**

```java
public class LogicalOperators {
    public static void main(String[] args) {
        boolean p = true;
        boolean q = false;

        System.out.println("p && q: " + (p && q)); // 输出：p && q: false
        System.out.println("p || q: " + (p || q)); // 输出：p || q: true
        System.out.println("!p: " + (!p));         // 输出：!p: false
    }
}
```

### 4. 赋值运算符

赋值运算符用于将值赋给变量。

- `=`：简单赋值
- `+=`：加法赋值
- `-=`：减法赋值
- `*=`：乘法赋值
- `/=`：除法赋值
- `%=`：取模赋值

**代码示例：**

```java
public class AssignmentOperators {
    public static void main(String[] args) {
        int a = 10;

        a += 5; // 等价于 a = a + 5
        System.out.println("a += 5: " + a); // 输出：a += 5: 15

        a -= 3; // 等价于 a = a - 3
        System.out.println("a -= 3: " + a); // 输出：a -= 3: 12

        a *= 2; // 等价于 a = a * 2
        System.out.println("a *= 2: " + a); // 输出：a *= 2: 24

        a /= 4; // 等价于 a = a / 4
        System.out.println("a /= 4: " + a); // 输出：a /= 4: 6

        a %= 5; // 等价于 a = a % 5
        System.out.println("a %= 5: " + a); // 输出：a %= 5: 1
    }
}
```

### 5. 位运算符

位运算符用于对整数的二进制位进行操作。

- `&`：按位与
- `|`：按位或
- `^`：按位异或
- `~`：按位取反
- `<<`：左移
- `>>`：右移
- `>>>`：无符号右移

**代码示例：**

```java
public class BitwiseOperators {
    public static void main(String[] args) {
        int a = 5;  // 二进制：0101
        int b = 3;  // 二进制：0011

        System.out.println("a & b: " + (a & b)); // 输出：a & b: 1 (二进制：0001)
        System.out.println("a | b: " + (a | b)); // 输出：a | b: 7 (二进制：0111)
        System.out.println("a ^ b: " + (a ^ b)); // 输出：a ^ b: 6 (二进制：0110)
        System.out.println("~a: " + (~a));       // 输出：~a: -6 (二进制：1010)
        System.out.println("a << 1: " + (a << 1)); // 输出：a << 1: 10 (二进制：1010)
        System.out.println("a >> 1: " + (a >> 1)); // 输出：a >> 1: 2 (二进制：0010)
        System.out.println("a >>> 1: " + (a >>> 1)); // 输出：a >>> 1: 2 (二进制：0010)
    }
}
```

### 6. 条件运算符（三元运算符）

条件运算符用于根据条件选择不同的值。

- `? :`：条件运算符

**代码示例：**

```java
public class ConditionalOperator {
    public static void main(String[] args) {
        int a = 10;
        int b = 20;

        int max = (a > b) ? a : b;
        System.out.println("Max value: " + max); // 输出：Max value: 20
    }
}
```

## 表达式

表达式是由运算符和操作数组成的，用于计算出某个值。表达式可以是简单的，也可以是复杂的。

**代码示例：**

```java
public class Expressions {
    public static void main(String[] args) {
        int x = 5;
        int y = 3;

        int result = x + y * 2; // 表达式：x + y * 2
        System.out.println("Result: " + result); // 输出：Result: 11
    }
}
```

## 实践练习

1. **计算圆的面积**：编写一个程序，使用用户输入的半径计算圆的面积。使用 `Math.PI` 和 `Math.pow` 函数。

2. **温度转换**：编写一个程序，将用户输入的摄氏温度转换为华氏温度。公式为：`F = C * 9/5 + 32`。

3. **判断闰年**：编写一个程序，判断用户输入的年份是否为闰年。闰年的条件是：能被4整除但不能被100整除，或者能被400整除。

## 总结

运算符和表达式是编程中的基础概念，掌握它们对于编写复杂的程序逻辑至关重要。通过本教程的学习，你应该能够理解并使用各种运算符，并能够编写简单的表达式来解决实际问题。继续练习和探索，你将能够更深入地理解这些概念，并在编程中灵活运用它们。