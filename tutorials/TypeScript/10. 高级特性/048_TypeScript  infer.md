---
title: 深入理解 TypeScript 中的 infer 关键字
date: 2023-10-05
description: 本课程将详细介绍 TypeScript 中的 infer 关键字，帮助你理解其在类型推断中的应用和高级用法。
slug: typescript-infer-keyword
tags:
  - TypeScript
  - 类型推断
  - 高级类型
category: 编程语言
keywords:
  - TypeScript infer
  - 类型推断
  - 高级类型
---

# infer 关键字

## 概述

在 TypeScript 中，`infer` 关键字用于在条件类型中推断类型。它允许我们在类型系统中进行更复杂的类型操作，特别是在处理泛型和条件类型时。`infer` 关键字通常与 `extends` 关键字一起使用，用于从复杂类型中提取出特定的部分。

## 理论解释

### 什么是 infer 关键字？

`infer` 关键字用于在条件类型中推断类型。它允许我们在类型系统中进行更复杂的类型操作，特别是在处理泛型和条件类型时。`infer` 关键字通常与 `extends` 关键字一起使用，用于从复杂类型中提取出特定的部分。

### 基本语法

```typescript
type MyType<T> = T extends SomeComplexType<infer U> ? U : never;
```

在这个例子中，`infer U` 表示我们希望从 `SomeComplexType` 中推断出类型 `U`。如果 `T` 是 `SomeComplexType` 的实例，那么 `U` 将被推断为 `SomeComplexType` 中的某个类型。

## 代码示例

### 示例 1：从函数类型中提取返回类型

```typescript
type ReturnType<T> = T extends (...args: any[]) => infer R ? R : never;

function greet() {
    return "Hello, World!";
}

type GreetReturnType = ReturnType<typeof greet>; // "Hello, World!"
```

在这个例子中，`ReturnType` 类型用于从函数类型中提取返回类型。`infer R` 表示我们希望从函数类型中推断出返回类型 `R`。

### 示例 2：从数组类型中提取元素类型

```typescript
type ElementType<T> = T extends (infer E)[] ? E : never;

type Numbers = number[];
type Strings = string[];

type NumberElement = ElementType<Numbers>; // number
type StringElement = ElementType<Strings>; // string
```

在这个例子中，`ElementType` 类型用于从数组类型中提取元素类型。`infer E` 表示我们希望从数组类型中推断出元素类型 `E`。

## 实践练习

### 练习 1：从元组类型中提取第一个元素的类型

编写一个类型 `FirstElement`，它可以从元组类型中提取第一个元素的类型。

```typescript
type FirstElement<T> = T extends [infer F, ...any[]] ? F : never;

type Tuple1 = [string, number, boolean];
type Tuple2 = [number, string];

type FirstOfTuple1 = FirstElement<Tuple1>; // string
type FirstOfTuple2 = FirstElement<Tuple2>; // number
```

### 练习 2：从 Promise 类型中提取返回值类型

编写一个类型 `PromiseValue`，它可以从 `Promise` 类型中提取返回值类型。

```typescript
type PromiseValue<T> = T extends Promise<infer V> ? V : never;

type PromiseString = Promise<string>;
type PromiseNumber = Promise<number>;

type ValueOfPromiseString = PromiseValue<PromiseString>; // string
type ValueOfPromiseNumber = PromiseValue<PromiseNumber>; // number
```

## 总结

`infer` 关键字是 TypeScript 中一个强大的工具，它允许我们在类型系统中进行更复杂的类型操作。通过结合 `extends` 关键字，我们可以从复杂类型中提取出特定的部分，从而实现更灵活的类型推断。掌握 `infer` 关键字的使用，将帮助你更好地理解和利用 TypeScript 的类型系统。

## 下一步

接下来，你可以尝试在实际项目中使用 `infer` 关键字，结合泛型和条件类型，实现更复杂的类型操作。同时，你也可以继续学习 TypeScript 中的其他高级类型特性，如工具类型、映射类型等，进一步提升你的 TypeScript 编程技能。