---
title: 函数式编程概念入门
date: 2023-10-05
description: 本课程介绍函数式编程的核心概念，包括纯函数、不可变数据和高阶函数，帮助你理解并应用函数式编程范式。
slug: functional-programming-concepts
tags:
  - 函数式编程
  - 编程范式
  - 软件开发
category: 编程基础
keywords:
  - 函数式编程
  - 纯函数
  - 不可变数据
  - 高阶函数
  - 编程范式
---

# 函数式编程概念

## 1. 什么是函数式编程？

函数式编程（Functional Programming，简称FP）是一种编程范式，它将计算视为数学函数的求值，并避免使用状态和可变数据。函数式编程强调函数的纯粹性，即函数的输出仅由其输入决定，而不依赖于外部状态或产生副作用。

### 1.1 函数式编程的核心概念

- **纯函数**：函数的输出仅依赖于输入参数，且不会修改外部状态或产生副作用。
- **不可变数据**：数据一旦创建，就不能被修改。任何操作都会返回新的数据副本。
- **高阶函数**：函数可以作为参数传递给其他函数，也可以作为函数的返回值。
- **递归**：通过递归调用自身来处理数据，而不是使用循环。

## 2. 纯函数

纯函数是函数式编程的基石。纯函数的定义如下：

- 相同的输入总是返回相同的输出。
- 不会产生副作用（如修改全局变量、I/O操作等）。

### 2.1 纯函数示例

```typescript
function add(a: number, b: number): number {
    return a + b;
}

console.log(add(2, 3)); // 输出: 5
console.log(add(2, 3)); // 输出: 5
```

### 2.2 非纯函数示例

```typescript
let counter = 0;

function increment(): number {
    counter++;
    return counter;
}

console.log(increment()); // 输出: 1
console.log(increment()); // 输出: 2
```

## 3. 不可变数据

在函数式编程中，数据是不可变的。任何对数据的修改都会返回一个新的数据副本，而不是修改原始数据。

### 3.1 不可变数据示例

```typescript
interface Person {
    name: string;
    age: number;
}

function incrementAge(person: Person): Person {
    return { ...person, age: person.age + 1 };
}

const john: Person = { name: "John", age: 30 };
const olderJohn = incrementAge(john);

console.log(john);       // 输出: { name: "John", age: 30 }
console.log(olderJohn);  // 输出: { name: "John", age: 31 }
```

## 4. 高阶函数

高阶函数是指可以接受一个或多个函数作为参数，或者返回一个函数的函数。

### 4.1 高阶函数示例

```typescript
function multiplyBy(factor: number): (value: number) => number {
    return function(value: number): number {
        return value * factor;
    };
}

const double = multiplyBy(2);
console.log(double(5)); // 输出: 10

const triple = multiplyBy(3);
console.log(triple(5)); // 输出: 15
```

## 5. 递归

递归是函数式编程中常用的技术，通过函数调用自身来解决问题。

### 5.1 递归示例：计算阶乘

```typescript
function factorial(n: number): number {
    if (n === 0) {
        return 1;
    }
    return n * factorial(n - 1);
}

console.log(factorial(5)); // 输出: 120
```

## 6. 实践练习

### 6.1 练习1：实现一个纯函数 `sumArray`，计算数组中所有元素的和

```typescript
function sumArray(arr: number[]): number {
    return arr.reduce((acc, curr) => acc + curr, 0);
}

console.log(sumArray([1, 2, 3, 4, 5])); // 输出: 15
```

### 6.2 练习2：实现一个高阶函数 `filterArray`，过滤数组中的元素

```typescript
function filterArray<T>(arr: T[], predicate: (item: T) => boolean): T[] {
    return arr.filter(predicate);
}

const numbers = [1, 2, 3, 4, 5];
const evenNumbers = filterArray(numbers, (num) => num % 2 === 0);

console.log(evenNumbers); // 输出: [2, 4]
```

### 6.3 练习3：使用递归实现斐波那契数列

```typescript
function fibonacci(n: number): number {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

console.log(fibonacci(10)); // 输出: 55
```

## 7. 总结

函数式编程是一种强调纯函数、不可变数据和高阶函数的编程范式。通过理解和实践这些概念，你可以编写出更加简洁、可维护和可测试的代码。希望本教程能帮助你更好地掌握函数式编程的核心思想，并在实际项目中应用这些技术。

## 8. 进一步学习资源

- [TypeScript 官方文档](https://www.typescriptlang.org/docs/)
- [Functional Programming in JavaScript](https://www.amazon.com/Functional-Programming-JavaScript-functional-techniques/dp/1617292826)
- [Functional-Light JavaScript](https://github.com/getify/Functional-Light-JS)

通过这些资源，你可以进一步深入学习函数式编程，并在 TypeScript 中应用这些概念。