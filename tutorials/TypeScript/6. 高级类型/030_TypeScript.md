---
title: 类型保护与类型区分：深入理解TypeScript中的类型操作
date: 2023-10-05
description: 本课程深入探讨TypeScript中的类型保护和类型区分技术，帮助开发者编写更安全、更高效的代码。
slug: typescript-type-guards-and-type-discrimination
tags:
  - TypeScript
  - 类型保护
  - 类型区分
category: 编程语言
keywords:
  - TypeScript类型保护
  - 类型区分
  - TypeScript类型操作
---

# 类型保护和类型区分

## 概述

在 TypeScript 中，类型保护（Type Guards）和类型区分（Type Narrowing）是用于在运行时确定变量类型的技术。它们帮助我们在编写代码时更安全地处理不同类型的数据，避免类型错误。

## 类型保护

类型保护是一种在运行时检查变量类型的机制。通过类型保护，我们可以在代码中明确地知道某个变量在特定条件下的类型。

### 常见的类型保护方法

1. **`typeof` 操作符**
2. **`instanceof` 操作符**
3. **自定义类型保护函数**

#### 1. `typeof` 操作符

`typeof` 操作符用于检查基本类型（如 `number`, `string`, `boolean`, `symbol`, `undefined`, `function`, `object`）。

```typescript
function printLength(value: number | string) {
    if (typeof value === "string") {
        console.log(value.length); // value 在这里是 string 类型
    } else {
        console.log(value); // value 在这里是 number 类型
    }
}

printLength("Hello"); // 输出: 5
printLength(123);    // 输出: 123
```

#### 2. `instanceof` 操作符

`instanceof` 操作符用于检查对象是否是某个类的实例。

```typescript
class Dog {
    bark() {
        console.log("Woof!");
    }
}

class Cat {
    meow() {
        console.log("Meow!");
    }
}

function makeSound(animal: Dog | Cat) {
    if (animal instanceof Dog) {
        animal.bark(); // animal 在这里是 Dog 类型
    } else {
        animal.meow(); // animal 在这里是 Cat 类型
    }
}

const dog = new Dog();
const cat = new Cat();

makeSound(dog); // 输出: Woof!
makeSound(cat); // 输出: Meow!
```

#### 3. 自定义类型保护函数

自定义类型保护函数允许我们定义自己的类型检查逻辑。

```typescript
interface Fish {
    swim(): void;
}

interface Bird {
    fly(): void;
}

function isFish(pet: Fish | Bird): pet is Fish {
    return (pet as Fish).swim !== undefined;
}

function move(pet: Fish | Bird) {
    if (isFish(pet)) {
        pet.swim(); // pet 在这里是 Fish 类型
    } else {
        pet.fly(); // pet 在这里是 Bird 类型
    }
}

const fish: Fish = {
    swim() {
        console.log("Swimming");
    }
};

const bird: Bird = {
    fly() {
        console.log("Flying");
    }
};

move(fish); // 输出: Swimming
move(bird); // 输出: Flying
```

## 类型区分

类型区分是指在代码中通过类型保护或其他方式，将一个联合类型（Union Type）缩小到更具体的类型。

### 示例：使用 `in` 操作符进行类型区分

`in` 操作符用于检查对象是否具有某个属性，从而进行类型区分。

```typescript
interface Admin {
    role: string;
}

interface User {
    email: string;
}

function printUserInfo(user: Admin | User) {
    if ("role" in user) {
        console.log(`Admin role: ${user.role}`); // user 在这里是 Admin 类型
    } else {
        console.log(`User email: ${user.email}`); // user 在这里是 User 类型
    }
}

const admin: Admin = { role: "SuperAdmin" };
const user: User = { email: "user@example.com" };

printUserInfo(admin); // 输出: Admin role: SuperAdmin
printUserInfo(user);  // 输出: User email: user@example.com
```

## 实践练习

### 练习 1：使用 `typeof` 进行类型保护

编写一个函数 `calculateArea`，该函数接受一个参数 `shape`，`shape` 可以是 `number` 或 `string`。如果 `shape` 是 `number`，则返回该数字的平方；如果 `shape` 是 `string`，则返回字符串的长度。

```typescript
function calculateArea(shape: number | string): number {
    if (typeof shape === "number") {
        return shape * shape;
    } else {
        return shape.length;
    }
}

console.log(calculateArea(5));    // 输出: 25
console.log(calculateArea("abc")); // 输出: 3
```

### 练习 2：使用 `instanceof` 进行类型保护

编写一个函数 `playSound`，该函数接受一个参数 `animal`，`animal` 可以是 `Dog` 或 `Cat` 类的实例。如果 `animal` 是 `Dog` 的实例，则调用 `bark` 方法；如果 `animal` 是 `Cat` 的实例，则调用 `meow` 方法。

```typescript
class Dog {
    bark() {
        console.log("Woof!");
    }
}

class Cat {
    meow() {
        console.log("Meow!");
    }
}

function playSound(animal: Dog | Cat) {
    if (animal instanceof Dog) {
        animal.bark();
    } else {
        animal.meow();
    }
}

const dog = new Dog();
const cat = new Cat();

playSound(dog); // 输出: Woof!
playSound(cat); // 输出: Meow!
```

### 练习 3：使用自定义类型保护函数

编写一个函数 `isCircle`，该函数接受一个参数 `shape`，`shape` 可以是 `Circle` 或 `Rectangle` 类的实例。如果 `shape` 是 `Circle` 的实例，则返回 `true`，否则返回 `false`。然后编写一个函数 `calculateArea`，该函数接受一个参数 `shape`，如果 `shape` 是 `Circle`，则返回其面积；如果 `shape` 是 `Rectangle`，则返回其面积。

```typescript
class Circle {
    radius: number;
    constructor(radius: number) {
        this.radius = radius;
    }
}

class Rectangle {
    width: number;
    height: number;
    constructor(width: number, height: number) {
        this.width = width;
        this.height = height;
    }
}

function isCircle(shape: Circle | Rectangle): shape is Circle {
    return (shape as Circle).radius !== undefined;
}

function calculateArea(shape: Circle | Rectangle): number {
    if (isCircle(shape)) {
        return Math.PI * shape.radius * shape.radius;
    } else {
        return shape.width * shape.height;
    }
}

const circle = new Circle(5);
const rectangle = new Rectangle(4, 6);

console.log(calculateArea(circle));     // 输出: 78.53981633974483
console.log(calculateArea(rectangle));  // 输出: 24
```

## 总结

类型保护和类型区分是 TypeScript 中非常重要的概念，它们帮助我们在编写代码时更安全地处理不同类型的数据。通过 `typeof`、`instanceof` 和自定义类型保护函数，我们可以有效地进行类型检查和类型区分，从而提高代码的健壮性和可维护性。

希望这篇教程能帮助你更好地理解 TypeScript 中的类型保护和类型区分。继续练习和探索，你将能够更熟练地应用这些技术。