---
title: 深入理解Python中的属性装饰器
date: 2023-10-05
description: 本课程将深入探讨Python中的属性装饰器，帮助你理解如何使用@property装饰器来管理类属性，提升代码的可读性和可维护性。
slug: understanding-python-property-decorators
tags:
  - Python
  - 装饰器
  - 面向对象编程
category: 编程基础
keywords:
  - Python装饰器
  - @property
  - 类属性管理
---

# 属性装饰器

## 概述

在 TypeScript 中，装饰器是一种特殊类型的声明，它可以附加到类声明、方法、访问器、属性或参数上。装饰器使用 `@expression` 这种形式，其中 `expression` 必须是一个函数，该函数将在运行时被调用，并带有关于被装饰的声明的信息。

属性装饰器是装饰器的一种，它应用于类的属性上。属性装饰器可以用来观察、修改或替换属性的定义。

## 属性装饰器的定义

属性装饰器表达式会在运行时作为函数被调用，传入两个参数：

1. `target`: 对于静态成员来说是类的构造函数，对于实例成员来说是类的原型对象。
2. `propertyKey`: 成员的名称。

### 示例代码

```typescript
function logProperty(target: any, propertyKey: string) {
    console.log(`Property ${propertyKey} is being decorated.`);
}

class Example {
    @logProperty
    public name: string;

    constructor(name: string) {
        this.name = name;
    }
}

const example = new Example("TypeScript");
```

### 解释

- `logProperty` 是一个属性装饰器函数。
- 当 `Example` 类的 `name` 属性被定义时，`logProperty` 函数会被调用，并输出 `Property name is being decorated.`。

## 属性装饰器的应用场景

属性装饰器可以用于多种场景，例如：

- **日志记录**：记录属性被访问或修改的日志。
- **数据验证**：在属性被赋值时进行数据验证。
- **属性代理**：通过装饰器实现属性的代理，例如实现一个只读属性。

### 示例：数据验证

```typescript
function validate(target: any, propertyKey: string, descriptor: PropertyDescriptor) {
    let value = target[propertyKey];

    const getter = function() {
        return value;
    };

    const setter = function(newVal: any) {
        if (typeof newVal !== 'string') {
            throw new Error(`${propertyKey} must be a string.`);
        }
        value = newVal;
    };

    Object.defineProperty(target, propertyKey, {
        get: getter,
        set: setter,
        enumerable: true,
        configurable: true
    });
}

class User {
    @validate
    public username: string;

    constructor(username: string) {
        this.username = username;
    }
}

const user = new User("TypeScript");
console.log(user.username); // 输出: TypeScript

try {
    user.username = 123; // 抛出错误: username must be a string.
} catch (e) {
    console.error(e.message);
}
```

### 解释

- `validate` 装饰器用于验证 `username` 属性是否为字符串类型。
- 如果尝试将 `username` 设置为非字符串类型，装饰器会抛出一个错误。

## 实践练习

### 练习 1：日志记录

编写一个属性装饰器，用于记录属性被访问和修改的日志。

```typescript
function logAccess(target: any, propertyKey: string) {
    let value = target[propertyKey];

    const getter = function() {
        console.log(`Getting ${propertyKey}: ${value}`);
        return value;
    };

    const setter = function(newVal: any) {
        console.log(`Setting ${propertyKey} to: ${newVal}`);
        value = newVal;
    };

    Object.defineProperty(target, propertyKey, {
        get: getter,
        set: setter,
        enumerable: true,
        configurable: true
    });
}

class Logger {
    @logAccess
    public message: string;

    constructor(message: string) {
        this.message = message;
    }
}

const logger = new Logger("Hello, TypeScript!");
logger.message = "Updated message";
console.log(logger.message);
```

### 练习 2：只读属性

编写一个属性装饰器，用于将属性设置为只读。

```typescript
function readonly(target: any, propertyKey: string) {
    Object.defineProperty(target, propertyKey, {
        writable: false
    });
}

class ReadOnlyExample {
    @readonly
    public readonlyProperty: string;

    constructor(readonlyProperty: string) {
        this.readonlyProperty = readonlyProperty;
    }
}

const example = new ReadOnlyExample("This is read-only.");
console.log(example.readonlyProperty);

try {
    example.readonlyProperty = "Attempt to change"; // 抛出错误: Cannot assign to read only property 'readonlyProperty'
} catch (e) {
    console.error(e.message);
}
```

## 总结

属性装饰器是 TypeScript 中强大的工具，可以用于增强类的属性的功能。通过属性装饰器，我们可以实现日志记录、数据验证、属性代理等功能。掌握属性装饰器的使用，将有助于你编写更加灵活和可维护的 TypeScript 代码。

希望这篇教程能够帮助你理解属性装饰器的概念和应用。继续探索 TypeScript 的更多高级特性，你将能够编写出更加强大和优雅的代码。