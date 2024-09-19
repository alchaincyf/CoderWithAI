---
title: 深入理解条件类型：TypeScript中的类型推断与条件判断
date: 2023-10-05
description: 本课程将深入探讨TypeScript中的条件类型，包括如何使用条件类型进行类型推断和条件判断，以及在实际项目中的应用。
slug: typescript-conditional-types
tags:
  - TypeScript
  - 类型系统
  - 条件类型
category: 编程语言
keywords:
  - TypeScript条件类型
  - 类型推断
  - 类型判断
---

# 条件类型

## 概述

条件类型（Conditional Types）是 TypeScript 中一种强大的类型系统特性，它允许我们根据某些条件来选择不同的类型。条件类型通常用于类型推断和类型映射，使得类型系统更加灵活和强大。

## 基本语法

条件类型的基本语法如下：

```typescript
T extends U ? X : Y
```

- `T` 和 `U` 是类型参数。
- `X` 和 `Y` 是类型结果。
- 如果 `T` 可以赋值给 `U`，则返回类型 `X`，否则返回类型 `Y`。

## 示例

### 示例 1: 简单的条件类型

```typescript
type IsString<T> = T extends string ? true : false;

type Result1 = IsString<"hello">; // true
type Result2 = IsString<42>; // false
```

在这个例子中，`IsString` 是一个条件类型，它检查传入的类型是否为 `string`。如果是 `string`，则返回 `true`，否则返回 `false`。

### 示例 2: 使用条件类型进行类型推断

```typescript
type ExtractArrayType<T> = T extends (infer U)[] ? U : never;

type Result3 = ExtractArrayType<number[]>; // number
type Result4 = ExtractArrayType<string[]>; // string
type Result5 = ExtractArrayType<{ key: string }[]>; // { key: string }
```

在这个例子中，`ExtractArrayType` 是一个条件类型，它使用 `infer` 关键字来推断数组元素的类型。如果 `T` 是一个数组类型，则返回数组元素的类型 `U`，否则返回 `never`。

## 实践练习

### 练习 1: 实现一个条件类型来判断是否为对象类型

```typescript
type IsObject<T> = T extends object ? true : false;

type Result6 = IsObject<{ key: string }>; // true
type Result7 = IsObject<"hello">; // false
```

### 练习 2: 实现一个条件类型来提取函数的返回类型

```typescript
type ExtractReturnType<T> = T extends (...args: any[]) => infer R ? R : never;

type Result8 = ExtractReturnType<() => string>; // string
type Result9 = ExtractReturnType<(x: number) => boolean>; // boolean
```

## 深入理解

### 分布式条件类型

当条件类型作用于一个联合类型时，它会自动分布到每个成员上。这种行为称为分布式条件类型。

```typescript
type ToArray<T> = T extends any ? T[] : never;

type Result10 = ToArray<string | number>; // string[] | number[]
```

在这个例子中，`ToArray<string | number>` 会被分布为 `ToArray<string> | ToArray<number>`，最终结果是 `string[] | number[]`。

### 条件类型与 `infer` 关键字

`infer` 关键字用于在条件类型中推断类型。它可以出现在条件类型的 `extends` 子句中，用于捕获类型变量。

```typescript
type Flatten<T> = T extends Array<infer U> ? U : T;

type Result11 = Flatten<number[]>; // number
type Result12 = Flatten<string>; // string
```

在这个例子中，`Flatten` 类型用于展平数组类型。如果 `T` 是一个数组类型，则推断出数组元素的类型 `U`，否则返回 `T` 本身。

## 总结

条件类型是 TypeScript 中一种非常强大的类型系统特性，它允许我们根据条件选择不同的类型。通过结合 `infer` 关键字，我们可以实现复杂的类型推断和类型映射。掌握条件类型将使你在 TypeScript 编程中更加得心应手。

## 下一步

接下来，我们将学习 TypeScript 中的映射类型和 `infer` 关键字，这些内容将进一步扩展你对 TypeScript 类型系统的理解。