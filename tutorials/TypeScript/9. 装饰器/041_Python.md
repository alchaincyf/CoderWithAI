---
title: 深入理解Python类装饰器
date: 2023-10-05
description: 本课程详细讲解Python中的类装饰器，帮助你理解其工作原理及如何在实际项目中应用。
slug: understanding-python-class-decorators
tags:
  - Python
  - 装饰器
  - 高级编程
category: 编程教程
keywords:
  - Python类装饰器
  - 装饰器模式
  - Python高级编程
---

# 类装饰器

## 1. 简介

装饰器（Decorators）是 TypeScript 中的一种高级特性，允许你在不修改类定义的情况下，动态地为类、方法、属性或参数添加额外的功能。装饰器是 ES7 的一个提案，TypeScript 提供了对它的实验性支持。

在本教程中，我们将重点介绍类装饰器，并通过理论解释、代码示例和实践练习来帮助你理解和掌握这一概念。

## 2. 装饰器的基本概念

### 2.1 什么是装饰器？

装饰器是一种特殊类型的声明，可以附加到类声明、方法、访问符、属性或参数上。装饰器使用 `@expression` 的形式，其中 `expression` 必须是一个函数，该函数在运行时被调用，并带有关于被装饰的声明的信息。

### 2.2 装饰器的分类

- **类装饰器**：应用于类声明。
- **方法装饰器**：应用于方法声明。
- **属性装饰器**：应用于属性声明。
- **参数装饰器**：应用于参数声明。

## 3. 类装饰器的使用

### 3.1 启用装饰器

在使用装饰器之前，你需要在 `tsconfig.json` 中启用 `experimentalDecorators` 选项：

```json
{
  "compilerOptions": {
    "target": "ES5",
    "experimentalDecorators": true
  }
}
```

### 3.2 定义类装饰器

类装饰器应用于类的构造函数，可以用来观察、修改或替换类的定义。类装饰器的表达式会在运行时作为函数被调用，类的构造函数作为其唯一的参数。

```typescript
function Logger(constructor: Function) {
  console.log('Logging...');
  console.log(constructor);
}

@Logger
class Person {
  name = 'Max';

  constructor() {
    console.log('Creating person object...');
  }
}

const person = new Person();
console.log(person);
```

### 3.3 装饰器工厂

装饰器工厂是一个返回装饰器函数的函数。通过使用装饰器工厂，你可以传递参数给装饰器。

```typescript
function Logger(logString: string) {
  return function(constructor: Function) {
    console.log(logString);
    console.log(constructor);
  };
}

@Logger('LOGGING - PERSON')
class Person {
  name = 'Max';

  constructor() {
    console.log('Creating person object...');
  }
}

const person = new Person();
console.log(person);
```

### 3.4 修改类的行为

你可以通过类装饰器修改类的行为。例如，你可以通过装饰器动态地添加新属性或方法。

```typescript
function WithTemplate(template: string, hookId: string) {
  return function<T extends { new(...args: any[]): { name: string } }>(originalConstructor: T) {
    return class extends originalConstructor {
      constructor(..._: any[]) {
        super();
        console.log('Rendering template');
        const hookEl = document.getElementById(hookId);
        if (hookEl) {
          hookEl.innerHTML = template;
          hookEl.querySelector('h1')!.textContent = this.name;
        }
      }
    };
  };
}

@WithTemplate('<h1>My Person Object</h1>', 'app')
class Person {
  name = 'Max';

  constructor() {
    console.log('Creating person object...');
  }
}

const person = new Person();
console.log(person);
```

## 4. 实践练习

### 4.1 练习：创建一个简单的类装饰器

1. 创建一个名为 `LogClass` 的类装饰器，该装饰器在类被实例化时记录一条消息。
2. 创建一个简单的类 `User`，并应用 `LogClass` 装饰器。
3. 实例化 `User` 类，观察控制台输出。

```typescript
function LogClass(constructor: Function) {
  console.log('Class instantiated:', constructor.name);
}

@LogClass
class User {
  name: string;

  constructor(name: string) {
    this.name = name;
  }
}

const user = new User('Alice');
console.log(user);
```

### 4.2 练习：使用装饰器工厂

1. 创建一个名为 `LogMethod` 的装饰器工厂，该工厂接受一个字符串参数，并在方法被调用时记录该字符串。
2. 创建一个类 `Calculator`，并在其方法上应用 `LogMethod` 装饰器。
3. 调用 `Calculator` 的方法，观察控制台输出。

```typescript
function LogMethod(logString: string) {
  return function(target: any, propertyKey: string, descriptor: PropertyDescriptor) {
    const originalMethod = descriptor.value;

    descriptor.value = function(...args: any[]) {
      console.log(logString);
      return originalMethod.apply(this, args);
    };
  };
}

class Calculator {
  @LogMethod('LOG: add method called')
  add(a: number, b: number): number {
    return a + b;
  }
}

const calculator = new Calculator();
console.log(calculator.add(10, 20));
```

## 5. 总结

类装饰器是 TypeScript 中一个强大的特性，允许你在不修改类定义的情况下，动态地为类添加额外的功能。通过本教程，你应该已经掌握了如何定义和使用类装饰器，以及如何通过装饰器工厂传递参数。

在实际开发中，装饰器可以帮助你实现诸如日志记录、权限控制、模板渲染等功能，从而提高代码的可维护性和可扩展性。

## 6. 进一步学习

- 探索其他类型的装饰器：方法装饰器、属性装饰器和参数装饰器。
- 学习如何使用装饰器实现依赖注入。
- 研究 TypeScript 中的高级装饰器模式，如 AOP（面向切面编程）。

通过不断实践和学习，你将能够更深入地理解和应用 TypeScript 的装饰器特性，从而提升你的编程技能。