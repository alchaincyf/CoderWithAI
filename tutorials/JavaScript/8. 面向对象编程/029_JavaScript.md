---
title: 深入理解JavaScript中的原型和原型链
date: 2023-10-05
description: 本课程将深入探讨JavaScript中的原型和原型链，帮助你理解对象继承和属性查找的机制。
slug: javascript-prototype-and-prototype-chain
tags:
  - JavaScript
  - 原型
  - 原型链
category: 编程基础
keywords:
  - JavaScript原型
  - 原型链
  - 对象继承
---

# 原型和原型链

## 1. 概述

在 JavaScript 中，原型和原型链是理解对象和继承机制的核心概念。每个 JavaScript 对象都有一个内部属性 `[[Prototype]]`，它指向另一个对象，这个对象就是原型。通过原型链，对象可以继承其他对象的属性和方法。

## 2. 原型基础

### 2.1 对象的原型

每个 JavaScript 对象都有一个原型。你可以使用 `Object.getPrototypeOf()` 方法来获取一个对象的原型。

```javascript
let obj = {};
let proto = Object.getPrototypeOf(obj);
console.log(proto); // 输出: [Object: null prototype] {}
```

### 2.2 构造函数和原型

在 JavaScript 中，构造函数创建的对象会自动关联到构造函数的 `prototype` 属性。

```javascript
function Person(name) {
    this.name = name;
}

Person.prototype.sayHello = function() {
    console.log(`Hello, my name is ${this.name}`);
};

let person1 = new Person('Alice');
person1.sayHello(); // 输出: Hello, my name is Alice
```

在这个例子中，`person1` 的原型是 `Person.prototype`，因此它可以调用 `sayHello` 方法。

## 3. 原型链

### 3.1 原型链的工作原理

当你访问一个对象的属性或方法时，JavaScript 引擎会首先在对象本身查找。如果找不到，它会沿着原型链向上查找，直到找到该属性或方法，或者到达原型链的顶端（即 `null`）。

```javascript
let animal = {
    eats: true
};

let rabbit = {
    jumps: true
};

Object.setPrototypeOf(rabbit, animal); // 将 rabbit 的原型设置为 animal

console.log(rabbit.eats); // 输出: true
console.log(rabbit.jumps); // 输出: true
```

在这个例子中，`rabbit` 没有 `eats` 属性，但它的原型是 `animal`，所以 `rabbit.eats` 返回 `true`。

### 3.2 原型链的顶端

原型链的顶端是 `null`。所有对象最终都会继承自 `Object.prototype`，而 `Object.prototype` 的原型是 `null`。

```javascript
console.log(Object.getPrototypeOf(Object.prototype)); // 输出: null
```

## 4. 实践练习

### 4.1 创建一个简单的继承链

创建一个 `Animal` 构造函数，并让 `Dog` 和 `Cat` 继承 `Animal` 的属性和方法。

```javascript
function Animal(name) {
    this.name = name;
}

Animal.prototype.speak = function() {
    console.log(`${this.name} makes a noise.`);
};

function Dog(name) {
    Animal.call(this, name);
}

Dog.prototype = Object.create(Animal.prototype);
Dog.prototype.constructor = Dog;

Dog.prototype.speak = function() {
    console.log(`${this.name} barks.`);
};

let dog = new Dog('Rex');
dog.speak(); // 输出: Rex barks.
```

### 4.2 使用 `Object.create` 创建对象

使用 `Object.create` 方法创建一个对象，并设置其原型。

```javascript
let vehicle = {
    wheels: 4,
    drive: function() {
        console.log('Driving...');
    }
};

let car = Object.create(vehicle);
car.drive(); // 输出: Driving...
```

## 5. 总结

原型和原型链是 JavaScript 中实现继承和共享属性的核心机制。理解这些概念对于掌握 JavaScript 的面向对象编程至关重要。通过构造函数、`Object.create` 和原型链，你可以创建复杂的对象关系和继承结构。

## 6. 进一步学习

- 深入研究 `Object.create` 和 `Object.setPrototypeOf` 的使用。
- 探索 ES6 中的 `class` 和 `extends` 关键字，它们提供了更简洁的语法来实现继承。
- 学习如何使用 `hasOwnProperty` 方法来检查对象是否拥有某个属性，而不是从原型链继承。

通过这些练习和进一步的学习，你将能够更深入地理解 JavaScript 的原型和原型链，并在实际项目中灵活运用这些知识。