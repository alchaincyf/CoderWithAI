---
title: Understanding Observables and Observers in Programming
date: 2023-10-05
description: Learn the fundamentals of Observables and Observers in programming, including their implementation and use cases in reactive programming.
slug: observables-and-observers
tags:
  - Reactive Programming
  - JavaScript
  - Observables
category: Programming Concepts
keywords:
  - Observables
  - Observers
  - Reactive Programming
  - JavaScript Observables
  - Observer Pattern
---

# Observable 和 Observer

## 概述

在现代前端开发中，尤其是在使用 Angular 这样的框架时，处理异步数据流是一个常见的需求。`Observable` 和 `Observer` 是 RxJS 库中的核心概念，它们帮助我们以一种声明式的方式处理异步操作和数据流。本教程将详细介绍 `Observable` 和 `Observer` 的概念，并通过代码示例和实践练习帮助你理解和应用这些概念。

## 1. Observable 简介

### 1.1 什么是 Observable？

`Observable` 是一种表示未来值或事件的集合的对象。它可以被订阅（`subscribe`），订阅者（`Observer`）会接收到 `Observable` 发出的值、错误或完成信号。`Observable` 可以看作是一个数据源，它可以随时间推移发出多个值。

### 1.2 Observable 的创建

你可以使用 `Observable` 构造函数或 RxJS 提供的各种操作符来创建 `Observable`。以下是一些常见的创建方式：

```typescript
import { Observable } from 'rxjs';

// 使用构造函数创建 Observable
const observable = new Observable<number>(subscriber => {
  subscriber.next(1);
  subscriber.next(2);
  subscriber.next(3);
  subscriber.complete();
});

// 使用 of 操作符创建 Observable
import { of } from 'rxjs';
const observableOf = of(1, 2, 3);

// 使用 from 操作符创建 Observable
import { from } from 'rxjs';
const observableFrom = from([1, 2, 3]);
```

### 1.3 Observable 的生命周期

一个 `Observable` 的生命周期包括以下几个阶段：

1. **创建**：使用构造函数或操作符创建 `Observable`。
2. **订阅**：`Observer` 订阅 `Observable`。
3. **执行**：`Observable` 开始执行，发出值、错误或完成信号。
4. **取消订阅**：`Observer` 可以取消订阅 `Observable`，释放资源。

## 2. Observer 简介

### 2.1 什么是 Observer？

`Observer` 是 `Observable` 的消费者。它是一个对象，包含三个回调函数：`next`、`error` 和 `complete`。当 `Observable` 发出值时，`next` 回调会被调用；当 `Observable` 发生错误时，`error` 回调会被调用；当 `Observable` 完成时，`complete` 回调会被调用。

### 2.2 Observer 的结构

一个典型的 `Observer` 对象如下：

```typescript
const observer = {
  next: (value) => console.log('Received value: ', value),
  error: (err) => console.error('Error occurred: ', err),
  complete: () => console.log('Completed'),
};
```

### 2.3 订阅 Observable

你可以使用 `subscribe` 方法将 `Observer` 订阅到 `Observable`：

```typescript
observable.subscribe(observer);
```

或者直接传递回调函数：

```typescript
observable.subscribe(
  value => console.log('Received value: ', value),
  err => console.error('Error occurred: ', err),
  () => console.log('Completed')
);
```

## 3. 实践练习

### 3.1 创建并订阅 Observable

创建一个 `Observable`，每隔一秒发出一个递增的数字，并在发出 5 个数字后完成。然后订阅这个 `Observable`，并在控制台输出接收到的值。

```typescript
import { Observable } from 'rxjs';

const observable = new Observable<number>(subscriber => {
  let count = 0;
  const intervalId = setInterval(() => {
    subscriber.next(count++);
    if (count === 5) {
      subscriber.complete();
      clearInterval(intervalId);
    }
  }, 1000);
});

observable.subscribe({
  next: value => console.log('Received value: ', value),
  error: err => console.error('Error occurred: ', err),
  complete: () => console.log('Completed'),
});
```

### 3.2 使用操作符创建 Observable

使用 `interval` 操作符创建一个 `Observable`，每隔一秒发出一个递增的数字，并在发出 5 个数字后完成。然后订阅这个 `Observable`，并在控制台输出接收到的值。

```typescript
import { interval } from 'rxjs';
import { take } from 'rxjs/operators';

const observable = interval(1000).pipe(take(5));

observable.subscribe({
  next: value => console.log('Received value: ', value),
  error: err => console.error('Error occurred: ', err),
  complete: () => console.log('Completed'),
});
```

## 4. 总结

通过本教程，你应该已经理解了 `Observable` 和 `Observer` 的基本概念，并学会了如何创建和订阅 `Observable`。`Observable` 和 `Observer` 是处理异步数据流的重要工具，掌握它们将帮助你更好地使用 Angular 和其他现代前端框架。

在接下来的课程中，我们将深入探讨 RxJS 提供的各种操作符，以及如何在 Angular 应用中使用 `Observable` 和 `Observer` 来处理复杂的异步场景。