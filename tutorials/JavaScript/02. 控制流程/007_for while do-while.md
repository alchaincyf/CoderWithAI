---
title: 深入理解循环结构：for, while, do-while
date: 2023-10-05
description: 本课程详细讲解编程中的循环结构，包括for循环、while循环和do-while循环的使用方法和应用场景，帮助你掌握循环控制的基本技巧。
slug: understanding-loops-for-while-do-while
tags:
  - 编程基础
  - 循环结构
  - 控制流
category: 编程基础
keywords:
  - for循环
  - while循环
  - do-while循环
  - 循环控制
  - 编程教程
---

# 循环 (for, while, do-while)

## 1. 引言

在编程中，循环是一种重复执行代码块的结构。循环可以帮助我们处理需要多次执行相同操作的情况，从而减少代码的重复性。JavaScript 提供了三种主要的循环结构：`for` 循环、`while` 循环和 `do-while` 循环。

## 2. for 循环

### 2.1 基本语法

`for` 循环是最常用的循环结构之一。它的基本语法如下：

```javascript
for (初始化; 条件; 更新) {
    // 循环体
}
```

- **初始化**：在循环开始前执行一次，通常用于设置循环变量的初始值。
- **条件**：每次循环开始前都会检查这个条件，如果条件为 `true`，则执行循环体；如果为 `false`，则退出循环。
- **更新**：每次循环结束后执行，通常用于更新循环变量的值。

### 2.2 示例

```javascript
for (let i = 0; i < 5; i++) {
    console.log("当前的 i 值是: " + i);
}
```

**输出：**

```
当前的 i 值是: 0
当前的 i 值是: 1
当前的 i 值是: 2
当前的 i 值是: 3
当前的 i 值是: 4
```

### 2.3 实践练习

编写一个 `for` 循环，计算从 1 到 10 的所有整数的和，并输出结果。

```javascript
let sum = 0;
for (let i = 1; i <= 10; i++) {
    sum += i;
}
console.log("1 到 10 的和是: " + sum);
```

## 3. while 循环

### 3.1 基本语法

`while` 循环在条件为 `true` 时重复执行代码块。它的基本语法如下：

```javascript
while (条件) {
    // 循环体
}
```

### 3.2 示例

```javascript
let i = 0;
while (i < 5) {
    console.log("当前的 i 值是: " + i);
    i++;
}
```

**输出：**

```
当前的 i 值是: 0
当前的 i 值是: 1
当前的 i 值是: 2
当前的 i 值是: 3
当前的 i 值是: 4
```

### 3.3 实践练习

编写一个 `while` 循环，计算从 1 到 10 的所有整数的和，并输出结果。

```javascript
let sum = 0;
let i = 1;
while (i <= 10) {
    sum += i;
    i++;
}
console.log("1 到 10 的和是: " + sum);
```

## 4. do-while 循环

### 4.1 基本语法

`do-while` 循环与 `while` 循环类似，但它的循环体至少会执行一次，即使条件一开始就为 `false`。它的基本语法如下：

```javascript
do {
    // 循环体
} while (条件);
```

### 4.2 示例

```javascript
let i = 0;
do {
    console.log("当前的 i 值是: " + i);
    i++;
} while (i < 5);
```

**输出：**

```
当前的 i 值是: 0
当前的 i 值是: 1
当前的 i 值是: 2
当前的 i 值是: 3
当前的 i 值是: 4
```

### 4.3 实践练习

编写一个 `do-while` 循环，计算从 1 到 10 的所有整数的和，并输出结果。

```javascript
let sum = 0;
let i = 1;
do {
    sum += i;
    i++;
} while (i <= 10);
console.log("1 到 10 的和是: " + sum);
```

## 5. 循环控制语句

### 5.1 `break` 语句

`break` 语句用于立即退出循环。

```javascript
for (let i = 0; i < 10; i++) {
    if (i === 5) {
        break;
    }
    console.log("当前的 i 值是: " + i);
}
```

**输出：**

```
当前的 i 值是: 0
当前的 i 值是: 1
当前的 i 值是: 2
当前的 i 值是: 3
当前的 i 值是: 4
```

### 5.2 `continue` 语句

`continue` 语句用于跳过当前循环的剩余部分，直接进入下一次循环。

```javascript
for (let i = 0; i < 5; i++) {
    if (i === 2) {
        continue;
    }
    console.log("当前的 i 值是: " + i);
}
```

**输出：**

```
当前的 i 值是: 0
当前的 i 值是: 1
当前的 i 值是: 3
当前的 i 值是: 4
```

## 6. 总结

循环是编程中非常重要的概念，它可以帮助我们高效地处理重复性任务。JavaScript 提供了多种循环结构，包括 `for` 循环、`while` 循环和 `do-while` 循环。通过合理使用这些循环结构，我们可以编写出更加简洁和高效的代码。

## 7. 进一步练习

1. 编写一个程序，使用 `for` 循环输出 1 到 100 之间的所有偶数。
2. 编写一个程序，使用 `while` 循环计算从 1 到 100 的所有奇数的和。
3. 编写一个程序，使用 `do-while` 循环输出 1 到 100 之间的所有能被 7 整除的数。

通过这些练习，你将更好地掌握循环的使用方法，并能够在实际编程中灵活应用。