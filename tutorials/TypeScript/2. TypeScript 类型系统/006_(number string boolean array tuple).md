---
title: 基本类型详解 (number, string, boolean, array, tuple)
date: 2023-10-05
description: 本课程详细讲解编程中的基本数据类型，包括数字、字符串、布尔值、数组和元组，帮助初学者掌握数据类型的基本概念和使用方法。
slug: basic-types-explained
tags:
  - 数据类型
  - 编程基础
  - 初学者指南
category: 编程基础
keywords:
  - number
  - string
  - boolean
  - array
  - tuple
---

# 基本类型详解 (number, string, boolean, array, tuple)

在 TypeScript 中，基本类型是构建复杂数据结构的基础。理解这些基本类型对于编写类型安全的代码至关重要。本教程将详细介绍 `number`、`string`、`boolean`、`array` 和 `tuple` 这五种基本类型。

## 1. `number` 类型

### 1.1 理论解释

`number` 类型用于表示数字。在 TypeScript 中，所有的数字，无论是整数还是浮点数，都被视为 `number` 类型。TypeScript 支持二进制、八进制、十进制和十六进制表示法。

### 1.2 代码示例

```typescript
let decimal: number = 6;
let hex: number = 0xf00d;
let binary: number = 0b1010;
let octal: number = 0o744;
let float: number = 3.14;

console.log(decimal); // 输出: 6
console.log(hex);     // 输出: 61453
console.log(binary);  // 输出: 10
console.log(octal);   // 输出: 484
console.log(float);   // 输出: 3.14
```

### 1.3 实践练习

编写一个 TypeScript 程序，定义并输出不同进制的数字。

## 2. `string` 类型

### 2.1 理论解释

`string` 类型用于表示文本数据。字符串可以用单引号 `'`、双引号 `"` 或反引号 `` ` `` 来表示。反引号 `` ` `` 可以用来创建模板字符串，支持嵌入表达式。

### 2.2 代码示例

```typescript
let name: string = "Alice";
let greeting: string = `Hello, ${name}!`;

console.log(name);     // 输出: Alice
console.log(greeting); // 输出: Hello, Alice!
```

### 2.3 实践练习

编写一个 TypeScript 程序，使用模板字符串创建一个问候语，并输出。

## 3. `boolean` 类型

### 3.1 理论解释

`boolean` 类型只有两个值：`true` 和 `false`。它通常用于条件判断和逻辑运算。

### 3.2 代码示例

```typescript
let isDone: boolean = false;

if (isDone) {
    console.log("工作已完成");
} else {
    console.log("工作未完成");
}
```

### 3.3 实践练习

编写一个 TypeScript 程序，根据布尔值输出不同的消息。

## 4. `array` 类型

### 4.1 理论解释

`array` 类型用于表示一组相同类型的元素。数组可以包含任意数量的元素，并且可以通过索引访问这些元素。

### 4.2 代码示例

```typescript
let numbers: number[] = [1, 2, 3, 4, 5];
let fruits: Array<string> = ["apple", "banana", "cherry"];

console.log(numbers[0]); // 输出: 1
console.log(fruits[1]);  // 输出: banana
```

### 4.3 实践练习

编写一个 TypeScript 程序，定义一个包含不同类型元素的数组，并输出数组中的元素。

## 5. `tuple` 类型

### 5.1 理论解释

`tuple` 类型允许你表示一个已知元素数量和类型的数组。与数组不同，元组的每个元素可以是不同的类型。

### 5.2 代码示例

```typescript
let person: [string, number] = ["Alice", 30];

console.log(person[0]); // 输出: Alice
console.log(person[1]); // 输出: 30
```

### 5.3 实践练习

编写一个 TypeScript 程序，定义一个包含姓名和年龄的元组，并输出元组中的元素。

## 总结

本教程详细介绍了 TypeScript 中的五种基本类型：`number`、`string`、`boolean`、`array` 和 `tuple`。通过理论解释、代码示例和实践练习，你应该能够理解并掌握这些基本类型的使用。在接下来的课程中，我们将继续深入探讨 TypeScript 的其他高级类型和特性。