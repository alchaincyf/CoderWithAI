---
title: 深入理解TypeScript中的联合类型
date: 2023-10-05
description: 本课程将详细介绍TypeScript中的联合类型，帮助你理解如何使用联合类型来增强代码的灵活性和类型安全性。
slug: typescript-union-types
tags:
  - TypeScript
  - 联合类型
  - 类型系统
category: 编程语言
keywords:
  - TypeScript联合类型
  - 类型安全
  - 编程教程
---

# 联合类型

## 概述

在 TypeScript 中，联合类型（Union Types）是一种允许变量或函数参数具有多种可能类型的机制。通过使用联合类型，我们可以更灵活地定义数据结构，使其能够适应多种数据类型。

## 理论解释

联合类型通过使用竖线符号 `|` 来定义，表示一个变量可以是多种类型中的一种。例如，`string | number` 表示一个变量可以是字符串或数字类型。

### 语法

```typescript
let variable: Type1 | Type2 | Type3;
```

### 示例

```typescript
let id: string | number;

id = "12345"; // 有效
id = 12345;   // 有效
id = true;    // 错误：类型 'boolean' 不能赋值给类型 'string | number'
```

## 代码示例

### 基本联合类型

```typescript
let value: string | number;

value = "Hello, TypeScript";
console.log(value); // 输出: Hello, TypeScript

value = 42;
console.log(value); // 输出: 42
```

### 函数参数的联合类型

```typescript
function printId(id: string | number) {
    console.log(`ID: ${id}`);
}

printId("12345"); // 输出: ID: 12345
printId(12345);   // 输出: ID: 12345
```

### 类型保护

在使用联合类型时，有时需要根据变量的实际类型执行不同的操作。这时可以使用类型保护（Type Guards）来区分类型。

```typescript
function printId(id: string | number) {
    if (typeof id === "string") {
        console.log(`String ID: ${id.toUpperCase()}`);
    } else {
        console.log(`Number ID: ${id}`);
    }
}

printId("12345"); // 输出: String ID: 12345
printId(12345);   // 输出: Number ID: 12345
```

## 实践练习

### 练习 1：联合类型与函数

编写一个函数 `formatValue`，该函数接受一个参数 `value`，可以是 `string` 或 `number`。如果 `value` 是字符串，则返回其大写形式；如果是数字，则返回其字符串表示。

```typescript
function formatValue(value: string | number): string {
    if (typeof value === "string") {
        return value.toUpperCase();
    } else {
        return value.toString();
    }
}

console.log(formatValue("hello")); // 输出: HELLO
console.log(formatValue(42));      // 输出: 42
```

### 练习 2：联合类型与数组

定义一个数组 `values`，其中每个元素可以是 `string` 或 `number`。编写一个函数 `sumOrConcat`，该函数接受这个数组并返回结果。如果数组中所有元素都是数字，则返回它们的和；否则，返回所有字符串的连接结果。

```typescript
function sumOrConcat(values: (string | number)[]): string | number {
    let isAllNumbers = values.every(value => typeof value === "number");

    if (isAllNumbers) {
        return values.reduce((sum, value) => sum + value, 0);
    } else {
        return values.filter(value => typeof value === "string").join("");
    }
}

console.log(sumOrConcat([1, 2, 3])); // 输出: 6
console.log(sumOrConcat(["a", "b", "c"])); // 输出: abc
console.log(sumOrConcat([1, "a", 2, "b"])); // 输出: ab
```

## 总结

联合类型是 TypeScript 中一个非常强大的特性，它允许我们定义更加灵活的数据结构。通过结合类型保护，我们可以在运行时根据变量的实际类型执行不同的操作。掌握联合类型将使你在编写 TypeScript 代码时更加得心应手。

## 下一步

接下来，我们将探讨 TypeScript 中的交叉类型（Intersection Types），它允许我们将多个类型组合成一个新类型。