---
title: 函数声明和表达式详解
date: 2023-10-05
description: 本课程详细讲解了JavaScript中的函数声明和函数表达式的区别、用法及其在实际编程中的应用。
slug: function-declarations-and-expressions
tags:
  - JavaScript
  - 函数编程
  - 基础教程
category: 编程基础
keywords:
  - 函数声明
  - 函数表达式
  - JavaScript函数
---

# 函数声明和表达式

在 TypeScript 中，函数是构建程序的基本模块之一。函数可以被声明为独立的代码块，也可以作为表达式嵌入到其他代码中。理解函数声明和表达式的区别以及如何使用它们是掌握 TypeScript 编程的关键。

## 1. 函数声明

函数声明是一种定义函数的方式，它使用 `function` 关键字来声明一个函数。函数声明会在代码执行之前被提升（hoisted），这意味着你可以在声明函数之前调用它。

### 1.1 基本语法

```typescript
function 函数名(参数1: 类型, 参数2: 类型): 返回类型 {
    // 函数体
    return 返回值;
}
```

### 1.2 示例

```typescript
function greet(name: string): string {
    return `Hello, ${name}!`;
}

console.log(greet("Alice")); // 输出: Hello, Alice!
```

在这个例子中，`greet` 函数接受一个字符串类型的参数 `name`，并返回一个字符串。

### 1.3 函数声明的提升

```typescript
console.log(greet("Bob")); // 输出: Hello, Bob!

function greet(name: string): string {
    return `Hello, ${name}!`;
}
```

即使函数在调用之后声明，TypeScript 仍然能够正确执行，因为函数声明会被提升到作用域的顶部。

## 2. 函数表达式

函数表达式是将函数赋值给变量的一种方式。与函数声明不同，函数表达式不会被提升，因此你必须在调用之前定义它。

### 2.1 基本语法

```typescript
const 变量名 = function(参数1: 类型, 参数2: 类型): 返回类型 {
    // 函数体
    return 返回值;
};
```

### 2.2 示例

```typescript
const greet = function(name: string): string {
    return `Hello, ${name}!`;
};

console.log(greet("Charlie")); // 输出: Hello, Charlie!
```

在这个例子中，`greet` 是一个函数表达式，它被赋值给一个变量，并且可以在之后调用。

### 2.3 函数表达式的提升问题

```typescript
console.log(greet("Dave")); // 错误: greet 未定义

const greet = function(name: string): string {
    return `Hello, ${name}!`;
};
```

在这个例子中，尝试在定义 `greet` 之前调用它会导致错误，因为函数表达式不会被提升。

## 3. 箭头函数

箭头函数是 ES6 引入的一种简洁的函数表达式语法。它使用 `=>` 符号来定义函数，并且具有更简洁的语法和不同的 `this` 绑定行为。

### 3.1 基本语法

```typescript
const 变量名 = (参数1: 类型, 参数2: 类型): 返回类型 => {
    // 函数体
    return 返回值;
};
```

### 3.2 示例

```typescript
const greet = (name: string): string => {
    return `Hello, ${name}!`;
};

console.log(greet("Eve")); // 输出: Hello, Eve!
```

### 3.3 单行箭头函数

如果函数体只有一行代码，可以省略大括号和 `return` 关键字：

```typescript
const greet = (name: string): string => `Hello, ${name}!`;

console.log(greet("Frank")); // 输出: Hello, Frank!
```

## 4. 实践练习

### 练习 1: 函数声明

编写一个函数 `calculateArea`，接受两个参数 `width` 和 `height`，并返回矩形的面积。使用函数声明的方式定义这个函数。

```typescript
function calculateArea(width: number, height: number): number {
    return width * height;
}

console.log(calculateArea(5, 10)); // 输出: 50
```

### 练习 2: 函数表达式

使用函数表达式的方式重写 `calculateArea` 函数。

```typescript
const calculateArea = function(width: number, height: number): number {
    return width * height;
};

console.log(calculateArea(5, 10)); // 输出: 50
```

### 练习 3: 箭头函数

使用箭头函数的方式重写 `calculateArea` 函数。

```typescript
const calculateArea = (width: number, height: number): number => width * height;

console.log(calculateArea(5, 10)); // 输出: 50
```

## 5. 总结

函数声明和函数表达式是 TypeScript 中定义函数的两种主要方式。函数声明具有提升特性，而函数表达式则没有。箭头函数提供了一种更简洁的语法，并且在处理 `this` 关键字时具有不同的行为。通过掌握这些概念，你将能够更灵活地编写 TypeScript 代码。

在接下来的课程中，我们将探讨函数的参数处理，包括可选参数、默认参数和剩余参数。