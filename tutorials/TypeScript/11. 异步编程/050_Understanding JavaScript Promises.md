---
title: Understanding JavaScript Promises
date: 2023-10-05
description: Learn how to handle asynchronous operations in JavaScript using Promises. This tutorial covers the basics of creating and chaining Promises, handling errors, and using async/await for cleaner code.
slug: understanding-javascript-promises
tags:
  - JavaScript
  - Asynchronous Programming
  - Promises
category: Web Development
keywords:
  - JavaScript Promises
  - Asynchronous JavaScript
  - Promise Chaining
  - Error Handling in Promises
  - Async/Await
---

# Promises 教程

## 1. 什么是 Promises？

在 JavaScript 中，`Promise` 是一种用于处理异步操作的对象。它代表了一个异步操作的最终完成（或失败）及其结果值。`Promise` 提供了一种更清晰、更简洁的方式来处理异步代码，避免了“回调地狱”（callback hell）的问题。

### 1.1 Promise 的状态

一个 `Promise` 有三种状态：

- **Pending（进行中）**: 初始状态，既不是成功，也不是失败。
- **Fulfilled（已成功）**: 意味着操作成功完成。
- **Rejected（已失败）**: 意味着操作失败。

### 1.2 Promise 的基本结构

```typescript
const myPromise = new Promise((resolve, reject) => {
    // 异步操作
    if (/* 操作成功 */) {
        resolve(value); // 成功时调用 resolve
    } else {
        reject(error); // 失败时调用 reject
    }
});
```

## 2. 创建和使用 Promise

### 2.1 创建一个简单的 Promise

```typescript
const promise = new Promise<string>((resolve, reject) => {
    setTimeout(() => {
        resolve("操作成功！");
    }, 1000);
});
```

### 2.2 使用 `.then()` 和 `.catch()` 处理 Promise

```typescript
promise
    .then((message) => {
        console.log(message); // 输出: 操作成功！
    })
    .catch((error) => {
        console.error(error);
    });
```

### 2.3 链式调用 `.then()`

```typescript
promise
    .then((message) => {
        console.log(message); // 输出: 操作成功！
        return "下一步操作";
    })
    .then((nextMessage) => {
        console.log(nextMessage); // 输出: 下一步操作
    })
    .catch((error) => {
        console.error(error);
    });
```

## 3. Promise 的静态方法

### 3.1 `Promise.resolve()`

```typescript
const resolvedPromise = Promise.resolve("立即成功");
resolvedPromise.then((value) => console.log(value)); // 输出: 立即成功
```

### 3.2 `Promise.reject()`

```typescript
const rejectedPromise = Promise.reject("立即失败");
rejectedPromise.catch((error) => console.error(error)); // 输出: 立即失败
```

### 3.3 `Promise.all()`

```typescript
const promise1 = Promise.resolve(3);
const promise2 = 42;
const promise3 = new Promise((resolve, reject) => {
    setTimeout(resolve, 100, 'foo');
});

Promise.all([promise1, promise2, promise3]).then((values) => {
    console.log(values); // 输出: [3, 42, "foo"]
});
```

### 3.4 `Promise.race()`

```typescript
const promise1 = new Promise((resolve, reject) => {
    setTimeout(resolve, 500, 'one');
});

const promise2 = new Promise((resolve, reject) => {
    setTimeout(resolve, 100, 'two');
});

Promise.race([promise1, promise2]).then((value) => {
    console.log(value); // 输出: "two"
});
```

## 4. 实践练习

### 4.1 练习：模拟异步数据加载

编写一个 `Promise`，模拟从服务器加载数据。如果数据加载成功，返回数据；如果失败，返回错误信息。

```typescript
function fetchData(url: string): Promise<string> {
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            if (url === "https://api.example.com/data") {
                resolve("数据加载成功！");
            } else {
                reject("数据加载失败！");
            }
        }, 2000);
    });
}

fetchData("https://api.example.com/data")
    .then((data) => console.log(data))
    .catch((error) => console.error(error));
```

### 4.2 练习：使用 `Promise.all()` 并行加载多个资源

编写一个函数，使用 `Promise.all()` 并行加载多个资源，并在所有资源加载完成后输出结果。

```typescript
function loadResource(url: string): Promise<string> {
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            resolve(`资源 ${url} 加载成功！`);
        }, Math.random() * 2000);
    });
}

const resources = [
    "https://api.example.com/resource1",
    "https://api.example.com/resource2",
    "https://api.example.com/resource3"
];

Promise.all(resources.map(loadResource))
    .then((results) => {
        console.log(results);
    })
    .catch((error) => {
        console.error(error);
    });
```

## 5. 总结

`Promise` 是处理异步操作的一种强大工具，它使得异步代码更加清晰和易于管理。通过本教程，你应该已经掌握了如何创建和使用 `Promise`，以及如何使用 `Promise` 的静态方法来处理多个异步操作。

在接下来的课程中，我们将学习如何使用 `Async/Await` 进一步简化异步代码的处理。