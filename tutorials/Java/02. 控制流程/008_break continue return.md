---
title: 跳转语句详解：break, continue, return
date: 2023-10-05
description: 本课程详细讲解编程中的跳转语句，包括break、continue和return的使用方法及其在不同编程语言中的应用。
slug: jump-statements-break-continue-return
tags:
  - 编程基础
  - 控制结构
  - 语句
category: 编程基础
keywords:
  - break语句
  - continue语句
  - return语句
  - 跳转语句
  - 编程控制结构
---

# 跳转语句 (break, continue, return)

在编程中，跳转语句用于控制程序的执行流程。Java 提供了三种主要的跳转语句：`break`、`continue` 和 `return`。这些语句可以帮助我们在特定条件下改变程序的执行路径，从而实现更复杂的逻辑控制。

## 1. `break` 语句

`break` 语句用于立即终止当前的循环（`for`、`while`、`do-while`）或 `switch` 语句。一旦执行到 `break` 语句，程序将跳出当前的循环或 `switch` 块，继续执行后续的代码。

### 1.1 在循环中使用 `break`

在循环中使用 `break` 语句可以提前终止循环，即使循环条件仍然为真。

```java
public class BreakExample {
    public static void main(String[] args) {
        for (int i = 1; i <= 10; i++) {
            if (i == 5) {
                break; // 当 i 等于 5 时，终止循环
            }
            System.out.println("i = " + i);
        }
        System.out.println("循环结束");
    }
}
```

**输出：**
```
i = 1
i = 2
i = 3
i = 4
循环结束
```

### 1.2 在 `switch` 语句中使用 `break`

在 `switch` 语句中，`break` 用于终止当前的 `case` 块，防止代码继续执行到下一个 `case`。

```java
public class SwitchBreakExample {
    public static void main(String[] args) {
        int day = 3;
        switch (day) {
            case 1:
                System.out.println("星期一");
                break;
            case 2:
                System.out.println("星期二");
                break;
            case 3:
                System.out.println("星期三");
                break;
            default:
                System.out.println("其他天");
                break;
        }
    }
}
```

**输出：**
```
星期三
```

## 2. `continue` 语句

`continue` 语句用于跳过当前循环的剩余部分，并立即开始下一次循环的迭代。它通常用于在某些条件下跳过特定的循环迭代。

### 2.1 在循环中使用 `continue`

在循环中使用 `continue` 语句可以跳过当前迭代，直接进入下一次迭代。

```java
public class ContinueExample {
    public static void main(String[] args) {
        for (int i = 1; i <= 10; i++) {
            if (i % 2 == 0) {
                continue; // 跳过偶数
            }
            System.out.println("i = " + i);
        }
    }
}
```

**输出：**
```
i = 1
i = 3
i = 5
i = 7
i = 9
```

## 3. `return` 语句

`return` 语句用于从方法中返回一个值，并终止方法的执行。如果方法的返回类型是 `void`，则可以使用不带值的 `return` 语句来提前结束方法。

### 3.1 在方法中使用 `return`

在方法中使用 `return` 语句可以返回一个值，并立即终止方法的执行。

```java
public class ReturnExample {
    public static void main(String[] args) {
        int result = add(5, 10);
        System.out.println("结果是: " + result);
    }

    public static int add(int a, int b) {
        return a + b; // 返回 a 和 b 的和
    }
}
```

**输出：**
```
结果是: 15
```

### 3.2 提前结束方法

在某些情况下，我们可能希望在满足某个条件时提前结束方法的执行。

```java
public class EarlyReturnExample {
    public static void main(String[] args) {
        int result = divide(10, 0);
        System.out.println("结果是: " + result);
    }

    public static int divide(int a, int b) {
        if (b == 0) {
            System.out.println("除数不能为零");
            return 0; // 提前结束方法
        }
        return a / b;
    }
}
```

**输出：**
```
除数不能为零
结果是: 0
```

## 4. 实践练习

### 练习 1: 使用 `break` 语句

编写一个程序，使用 `for` 循环从 1 到 100 进行迭代，当找到第一个能被 7 整除的数时，使用 `break` 语句终止循环。

### 练习 2: 使用 `continue` 语句

编写一个程序，使用 `while` 循环从 1 到 20 进行迭代，跳过所有能被 3 整除的数。

### 练习 3: 使用 `return` 语句

编写一个方法，接受两个整数参数，如果第一个参数大于第二个参数，则返回 `true`，否则返回 `false`。在 `main` 方法中调用该方法并打印结果。

## 5. 总结

`break`、`continue` 和 `return` 是 Java 中常用的跳转语句，它们可以帮助我们控制程序的执行流程。`break` 用于终止循环或 `switch` 语句，`continue` 用于跳过当前循环迭代，`return` 用于从方法中返回值并终止方法的执行。通过合理使用这些语句，我们可以编写出更加灵活和高效的代码。

希望这篇教程能帮助你更好地理解和使用 Java 中的跳转语句。继续练习和实践，你将能够熟练掌握这些重要的编程概念。