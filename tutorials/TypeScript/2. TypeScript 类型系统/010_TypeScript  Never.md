---
title: 深入理解 TypeScript 中的 Never 类型
date: 2023-10-05
description: 本课程将详细介绍 TypeScript 中的 Never 类型，探讨其在类型系统中的作用和使用场景，帮助开发者更好地理解和应用这一高级类型。
slug: typescript-never-type
tags:
  - TypeScript
  - 类型系统
  - 高级类型
category: 编程语言
keywords:
  - TypeScript Never 类型
  - 类型系统
  - 函数返回类型
---

# Never 类型

## 1. 概述

在 TypeScript 中，`Never` 类型是一个特殊的类型，用于表示那些永远不会发生的值。它通常用于以下几种情况：

1. **函数永远不会返回**：当一个函数永远不会正常返回时，例如抛出异常或进入无限循环。
2. **类型保护**：用于确保某些代码路径永远不会被执行。

## 2. 理论解释

### 2.1 函数永远不会返回

当一个函数永远不会返回时，例如抛出异常或进入无限循环，TypeScript 会推断该函数的返回类型为 `never`。

```typescript
function throwError(message: string): never {
    throw new Error(message);
}

function infiniteLoop(): never {
    while (true) {
        // 无限循环
    }
}
```

### 2.2 类型保护

`never` 类型还可以用于类型保护，确保某些代码路径永远不会被执行。例如，在一个联合类型中，使用 `never` 来确保所有可能的情况都被处理。

```typescript
function assertNever(x: never): never {
    throw new Error("Unexpected object: " + x);
}

type Shape = Square | Circle | Triangle;

function getArea(shape: Shape) {
    switch (shape.kind) {
        case "square":
            return shape.size * shape.size;
        case "circle":
            return Math.PI * shape.radius ** 2;
        case "triangle":
            return (shape.base * shape.height) / 2;
        default:
            return assertNever(shape); // 确保所有情况都被处理
    }
}
```

## 3. 代码示例

### 3.1 函数永远不会返回

```typescript
function throwError(message: string): never {
    throw new Error(message);
}

function infiniteLoop(): never {
    while (true) {
        // 无限循环
    }
}

// 调用 throwError 函数
try {
    throwError("This is an error");
} catch (e) {
    console.error(e);
}

// 调用 infiniteLoop 函数
// infiniteLoop(); // 这会导致无限循环，请谨慎运行
```

### 3.2 类型保护

```typescript
function assertNever(x: never): never {
    throw new Error("Unexpected object: " + x);
}

type Shape = Square | Circle | Triangle;

interface Square {
    kind: "square";
    size: number;
}

interface Circle {
    kind: "circle";
    radius: number;
}

interface Triangle {
    kind: "triangle";
    base: number;
    height: number;
}

function getArea(shape: Shape) {
    switch (shape.kind) {
        case "square":
            return shape.size * shape.size;
        case "circle":
            return Math.PI * shape.radius ** 2;
        case "triangle":
            return (shape.base * shape.height) / 2;
        default:
            return assertNever(shape); // 确保所有情况都被处理
    }
}

// 使用 getArea 函数
const square: Square = { kind: "square", size: 5 };
const circle: Circle = { kind: "circle", radius: 3 };
const triangle: Triangle = { kind: "triangle", base: 4, height: 6 };

console.log(getArea(square)); // 25
console.log(getArea(circle)); // 28.274333882308138
console.log(getArea(triangle)); // 12
```

## 4. 实践练习

### 4.1 练习 1：编写一个永远不会返回的函数

编写一个函数 `crashApp`，该函数会抛出一个错误，并且函数的返回类型应该是 `never`。

```typescript
function crashApp(): never {
    throw new Error("Application crashed!");
}

// 调用 crashApp 函数
try {
    crashApp();
} catch (e) {
    console.error(e);
}
```

### 4.2 练习 2：使用 `never` 进行类型保护

编写一个函数 `processMessage`，该函数接受一个联合类型 `Message`，并根据消息类型执行不同的操作。确保所有可能的消息类型都被处理，并在 `default` 分支中使用 `assertNever` 函数。

```typescript
type Message = TextMessage | ImageMessage | VideoMessage;

interface TextMessage {
    type: "text";
    content: string;
}

interface ImageMessage {
    type: "image";
    url: string;
}

interface VideoMessage {
    type: "video";
    url: string;
}

function assertNever(x: never): never {
    throw new Error("Unexpected message type: " + x);
}

function processMessage(message: Message) {
    switch (message.type) {
        case "text":
            console.log("Processing text message: " + message.content);
            break;
        case "image":
            console.log("Processing image message: " + message.url);
            break;
        case "video":
            console.log("Processing video message: " + message.url);
            break;
        default:
            assertNever(message); // 确保所有情况都被处理
    }
}

// 使用 processMessage 函数
const textMessage: TextMessage = { type: "text", content: "Hello, world!" };
const imageMessage: ImageMessage = { type: "image", url: "https://example.com/image.png" };
const videoMessage: VideoMessage = { type: "video", url: "https://example.com/video.mp4" };

processMessage(textMessage); // Processing text message: Hello, world!
processMessage(imageMessage); // Processing image message: https://example.com/image.png
processMessage(videoMessage); // Processing video message: https://example.com/video.mp4
```

## 5. 总结

`Never` 类型在 TypeScript 中是一个非常有用的工具，用于表示那些永远不会发生的值。通过理解 `never` 类型的使用场景，你可以更好地编写类型安全的代码，并确保所有可能的情况都被处理。希望这篇教程能帮助你更好地理解和使用 `never` 类型。