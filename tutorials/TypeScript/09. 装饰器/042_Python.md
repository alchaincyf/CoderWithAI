---
title: 深入理解Python方法装饰器
date: 2023-10-05
description: 本课程详细讲解Python中的方法装饰器，包括其定义、使用场景以及如何创建自定义装饰器。
slug: python-method-decorators
tags:
  - Python
  - 装饰器
  - 高级编程
category: 编程语言
keywords:
  - Python装饰器
  - 方法装饰器
  - 自定义装饰器
---

# 方法装饰器

## 概述

在 TypeScript 中，装饰器是一种特殊类型的声明，它可以附加到类声明、方法、访问器、属性或参数上。装饰器使用 `@expression` 这种形式，其中 `expression` 必须是一个函数，该函数将在运行时被调用，并带有关于被装饰的声明的信息。

本教程将重点介绍**方法装饰器**，它是一种可以用来修改或扩展类方法行为的强大工具。

## 方法装饰器的基础

### 定义方法装饰器

方法装饰器应用于类的方法上。它可以通过在方法声明前添加 `@decorator` 来使用。方法装饰器可以用来观察、修改或替换方法定义。

```typescript
function logMethod(target: any, propertyKey: string, descriptor: PropertyDescriptor) {
    const originalMethod = descriptor.value;

    descriptor.value = function (...args: any[]) {
        console.log(`Calling ${propertyKey} with arguments:`, args);
        const result = originalMethod.apply(this, args);
        console.log(`Method ${propertyKey} returned:`, result);
        return result;
    };

    return descriptor;
}

class Example {
    @logMethod
    greet(name: string) {
        return `Hello, ${name}!`;
    }
}

const example = new Example();
example.greet("TypeScript"); // 输出调用信息和返回值
```

### 方法装饰器的参数

方法装饰器接收三个参数：

1. **target**: 对于静态成员来说是类的构造函数，对于实例成员来说是类的原型。
2. **propertyKey**: 方法的名称。
3. **descriptor**: 方法的属性描述符。

### 修改方法行为

通过修改 `descriptor.value`，我们可以改变方法的行为。例如，我们可以在方法调用前后添加日志记录。

## 实践练习

### 练习 1: 计时器装饰器

编写一个方法装饰器，用于计算方法的执行时间。

```typescript
function timeMethod(target: any, propertyKey: string, descriptor: PropertyDescriptor) {
    const originalMethod = descriptor.value;

    descriptor.value = function (...args: any[]) {
        const startTime = performance.now();
        const result = originalMethod.apply(this, args);
        const endTime = performance.now();
        console.log(`Method ${propertyKey} took ${endTime - startTime} ms to execute.`);
        return result;
    };

    return descriptor;
}

class Example {
    @timeMethod
    slowMethod() {
        let sum = 0;
        for (let i = 0; i < 100000000; i++) {
            sum += i;
        }
        return sum;
    }
}

const example = new Example();
example.slowMethod(); // 输出方法执行时间
```

### 练习 2: 缓存装饰器

编写一个方法装饰器，用于缓存方法的结果。如果方法被调用多次且参数相同，则返回缓存的结果。

```typescript
function cacheMethod(target: any, propertyKey: string, descriptor: PropertyDescriptor) {
    const originalMethod = descriptor.value;
    const cache = new Map<string, any>();

    descriptor.value = function (...args: any[]) {
        const key = JSON.stringify(args);
        if (cache.has(key)) {
            console.log(`Returning cached result for ${propertyKey} with arguments:`, args);
            return cache.get(key);
        }
        const result = originalMethod.apply(this, args);
        cache.set(key, result);
        return result;
    };

    return descriptor;
}

class Example {
    @cacheMethod
    expensiveCalculation(x: number, y: number) {
        return x * y;
    }
}

const example = new Example();
console.log(example.expensiveCalculation(2, 3)); // 计算并缓存
console.log(example.expensiveCalculation(2, 3)); // 返回缓存结果
```

## 总结

方法装饰器是 TypeScript 中一个非常强大的特性，它允许我们在不修改原始方法代码的情况下，扩展或修改方法的行为。通过本教程的学习，你应该已经掌握了如何定义和使用方法装饰器，并且能够通过实践练习来进一步巩固这些知识。

在实际开发中，方法装饰器可以用于日志记录、性能监控、缓存等多种场景，帮助我们编写更加灵活和可维护的代码。