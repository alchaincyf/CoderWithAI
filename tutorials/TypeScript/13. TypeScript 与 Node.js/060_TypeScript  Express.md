---
title: 使用 TypeScript 开发 Express 应用
date: 2023-10-05
description: 本课程将教你如何使用 TypeScript 来开发和增强 Express 应用，提升代码的可维护性和类型安全性。
slug: express-typescript-course
tags:
  - TypeScript
  - Express
  - Node.js
category: Web 开发
keywords:
  - TypeScript Express
  - Node.js 框架
  - 类型安全
---

# Express 与 TypeScript 教程

## 1. TypeScript 简介和优势

### 1.1 什么是 TypeScript？
TypeScript 是 JavaScript 的超集，添加了静态类型定义和其他高级功能。它由 Microsoft 开发，旨在提高代码的可维护性和可读性。

### 1.2 TypeScript 的优势
- **静态类型检查**：在编译时捕获错误，减少运行时错误。
- **更好的工具支持**：IDE 可以提供更好的代码补全和重构支持。
- **可读性和可维护性**：类型定义使代码更易于理解和维护。

## 2. 开发环境搭建

### 2.1 安装 Node.js
首先，确保你已经安装了 Node.js。你可以从 [Node.js 官网](https://nodejs.org/) 下载并安装。

### 2.2 安装 TypeScript 编译器
使用 npm 安装 TypeScript 编译器：
```bash
npm install -g typescript
```

### 2.3 创建项目目录
创建一个新的项目目录并初始化 npm：
```bash
mkdir my-express-app
cd my-express-app
npm init -y
```

### 2.4 安装 Express 和 TypeScript 依赖
安装 Express 和 TypeScript 相关的依赖：
```bash
npm install express
npm install --save-dev typescript @types/node @types/express
```

## 3. 第一个 TypeScript 程序

### 3.1 创建 `tsconfig.json`
在项目根目录下创建 `tsconfig.json` 文件：
```json
{
  "compilerOptions": {
    "target": "ES6",
    "module": "commonjs",
    "strict": true,
    "esModuleInterop": true,
    "outDir": "./dist"
  },
  "include": ["src/**/*"]
}
```

### 3.2 编写第一个 TypeScript 程序
在 `src` 目录下创建 `index.ts` 文件：
```typescript
import express from 'express';

const app = express();
const port = 3000;

app.get('/', (req, res) => {
  res.send('Hello, TypeScript with Express!');
});

app.listen(port, () => {
  console.log(`Server is running on http://localhost:${port}`);
});
```

### 3.3 编译和运行
使用 TypeScript 编译器编译代码：
```bash
tsc
```

编译完成后，运行生成的 JavaScript 文件：
```bash
node dist/index.js
```

打开浏览器访问 `http://localhost:3000`，你应该会看到 "Hello, TypeScript with Express!"。

## 4. 基本类型和变量声明

### 4.1 基本类型
TypeScript 支持以下基本类型：
- `number`
- `string`
- `boolean`
- `array`
- `tuple`
- `enum`
- `any`
- `unknown`
- `void`
- `null`
- `undefined`
- `never`

### 4.2 变量声明
```typescript
let age: number = 25;
let name: string = "Alice";
let isStudent: boolean = true;
let hobbies: string[] = ["reading", "coding"];
let person: [string, number] = ["Alice", 25];
```

## 5. TypeScript 配置文件 (`tsconfig.json`)

`tsconfig.json` 文件用于配置 TypeScript 编译器的行为。常见的配置选项包括：
- `target`：指定编译后的 JavaScript 版本。
- `module`：指定模块系统。
- `strict`：启用严格模式。
- `outDir`：指定输出目录。

## 6. 基本类型详解

### 6.1 `number`
```typescript
let age: number = 25;
```

### 6.2 `string`
```typescript
let name: string = "Alice";
```

### 6.3 `boolean`
```typescript
let isStudent: boolean = true;
```

### 6.4 `array`
```typescript
let hobbies: string[] = ["reading", "coding"];
```

### 6.5 `tuple`
```typescript
let person: [string, number] = ["Alice", 25];
```

## 7. 枚举类型

枚举类型用于定义一组命名的常量：
```typescript
enum Color {
  Red,
  Green,
  Blue
}

let c: Color = Color.Green;
```

## 8. Any 和 Unknown 类型

### 8.1 `any`
`any` 类型允许变量存储任何类型的值：
```typescript
let notSure: any = 4;
notSure = "maybe a string instead";
```

### 8.2 `unknown`
`unknown` 类型类似于 `any`，但更安全：
```typescript
let notSure: unknown = 4;
if (typeof notSure === "string") {
  console.log(notSure.length);
}
```

## 9. Void, Null 和 Undefined

### 9.1 `void`
`void` 类型通常用于函数没有返回值的情况：
```typescript
function warnUser(): void {
  console.log("This is a warning message");
}
```

### 9.2 `null` 和 `undefined`
```typescript
let u: undefined = undefined;
let n: null = null;
```

## 10. Never 类型

`never` 类型表示永远不会发生的值：
```typescript
function error(message: string): never {
  throw new Error(message);
}
```

## 11. 类型断言

类型断言允许你告诉编译器某个值的类型：
```typescript
let someValue: any = "this is a string";
let strLength: number = (someValue as string).length;
```

## 12. 函数声明和表达式

### 12.1 函数声明
```typescript
function add(x: number, y: number): number {
  return x + y;
}
```

### 12.2 函数表达式
```typescript
let add = function(x: number, y: number): number {
  return x + y;
};
```

## 13. 可选参数和默认参数

### 13.1 可选参数
```typescript
function buildName(firstName: string, lastName?: string) {
  if (lastName) {
    return firstName + " " + lastName;
  } else {
    return firstName;
  }
}
```

### 13.2 默认参数
```typescript
function buildName(firstName: string, lastName = "Smith") {
  return firstName + " " + lastName;
}
```

## 14. 剩余参数

剩余参数允许你将多个参数收集到一个数组中：
```typescript
function buildName(firstName: string, ...restOfName: string[]) {
  return firstName + " " + restOfName.join(" ");
}
```

## 15. 函数重载

函数重载允许你定义多个函数签名：
```typescript
function add(x: number, y: number): number;
function add(x: string, y: string): string;
function add(x: any, y: any): any {
  return x + y;
}
```

## 16. This 和箭头函数

### 16.1 `this`
```typescript
let deck = {
  suits: ["hearts", "spades", "clubs", "diamonds"],
  cards: Array(52),
  createCardPicker: function() {
    return () => {
      let pickedCard = Math.floor(Math.random() * 52);
      let pickedSuit = Math.floor(pickedCard / 13);
      return {suit: this.suits[pickedSuit], card: pickedCard % 13};
    };
  }
};
```

### 16.2 箭头函数
```typescript
let deck = {
  suits: ["hearts", "spades", "clubs", "diamonds"],
  cards: Array(52),
  createCardPicker: function() {
    return () => {
      let pickedCard = Math.floor(Math.random() * 52);
      let pickedSuit = Math.floor(pickedCard / 13);
      return {suit: this.suits[pickedSuit], card: pickedCard % 13};
    };
  }
};
```

## 17. 接口定义和使用

接口用于定义对象的形状：
```typescript
interface Person {
  firstName: string;
  lastName: string;
}

function greeter(person: Person) {
  return "Hello, " + person.firstName + " " + person.lastName;
}
```

## 18. 可选属性和只读属性

### 18.1 可选属性
```typescript
interface SquareConfig {
  color?: string;
  width?: number;
}
```

### 18.2 只读属性
```typescript
interface Point {
  readonly x: number;
  readonly y: number;
}
```

## 19. 函数类型接口

函数类型接口用于定义函数的形状：
```typescript
interface SearchFunc {
  (source: string, subString: string): boolean;
}
```

## 20. 类类型接口

类类型接口用于定义类的形状：
```typescript
interface ClockInterface {
  currentTime: Date;
  setTime(d: Date): void;
}

class Clock implements ClockInterface {
  currentTime: Date = new Date();
  setTime(d: Date) {
    this.currentTime = d;
  }
}
```

## 21. 继承接口

接口可以继承其他接口：
```typescript
interface Shape {
  color: string;
}

interface Square extends Shape {
  sideLength: number;
}
```

## 22. 类型别名 vs 接口

类型别名和接口都可以用于定义类型，但有一些区别：
```typescript
type Name = string;
type NameResolver = () => string;
type NameOrResolver = Name | NameResolver;
```

## 23. 类的定义和实例化

### 23.1 类的定义
```typescript
class Greeter {
  greeting: string;
  constructor(message: string) {
    this.greeting = message;
  }
  greet() {
    return "Hello, " + this.greeting;
  }
}
```

### 23.2 类的实例化
```typescript
let greeter = new Greeter("world");
```

## 24. 继承和多态

### 24.1 继承
```typescript
class Animal {
  move(distanceInMeters: number = 0) {
    console.log(`Animal moved ${distanceInMeters}m.`);
  }
}

class Dog extends Animal {
  bark() {
    console.log("Woof! Woof!");
  }
}
```

### 24.2 多态
```typescript
let dog = new Dog();
dog.bark();
dog.move(10);
```

## 25. 访问修饰符 (public, private, protected)

### 25.1 `public`
```typescript
class Animal {
  public name: string;
  public constructor(theName: string) { this.name = theName; }
  public move(distanceInMeters: number) {
    console.log(`${this.name} moved ${distanceInMeters}m.`);
  }
}
```

### 25.2 `private`
```typescript
class Animal {
  private name: string;
  constructor(theName: string) { this.name = theName; }
}
```

### 25.3 `protected`
```typescript
class Person {
  protected name: string;
  constructor(name: string) { this.name = name; }
}

class Employee extends Person {
  private department: string;

  constructor(name: string, department: string) {
    super(name);
    this.department = department;
  }

  public getElevatorPitch() {
    return `Hello, my name is ${this.name} and I work in ${this.department}.`;
  }
}
```

## 26. 静态属性和方法

静态属性和方法属于类本身，而不是类的实例：
```typescript
class Grid {
  static origin = {x: 0, y: 0};
  calculateDistanceFromOrigin(point: {x: number; y: number}) {
    let xDist = (point.x - Grid.origin.x);
    let yDist = (point.y - Grid.origin.y);
    return Math.sqrt(xDist * xDist + yDist * yDist) / this.scale;
  }
  constructor (public scale: number) { }
}
```

## 27. 抽象类

抽象类不能直接实例化，通常用作其他类的基类：
```typescript
abstract class Department {
  constructor(public name: string) {
  }

  printName(): void {
    console.log("Department name: " + this.name);
  }

  abstract printMeeting(): void; // 必须在派生类中实现
}

class AccountingDepartment extends Department {
  constructor() {
    super("Accounting and Auditing"); // 在派生类的构造函数中必须调用 super()
  }

  printMeeting(): void {
    console.log("The Accounting Department meets each Monday at 10am.");
  }

  generateReports(): void {
    console.log("Generating accounting reports...");
  }
}
```

## 28. 交叉类型

交叉类型用于组合多个类型：
```typescript
function extend<T, U>(first: T, second: U): T & U {
  let result = <T & U>{};
  for (let id in first) {
    (<any>result)[id] = (<any>first)[id];
  }
  for (let id in second) {
    if (!result.hasOwnProperty(id)) {
      (<any>result)[id] = (<any>second)[id];
    }
  }
  return result;
}
```

## 29. 联合类型

联合类型允许变量具有多种类型之一：
```typescript
function padLeft(value: string, padding: string | number) {
  if (typeof padding === "number") {
    return Array(padding + 1).join(" ") + value;
  }
  if (typeof padding === "string") {
    return padding + value;
  }
  throw new Error(`Expected string or number, got '${padding}'.`);
}
```

## 30. 类型保护和类型区分

类型保护用于在运行时确定变量的类型：
```typescript
function isFish(pet: Fish | Bird): pet is Fish {
  return (<Fish>pet).swim !== undefined;
}
```

## 31. 可为 null 的类型

TypeScript 支持可为 null 的类型：
```typescript
let s = "foo";
s = null; // 错误, 'null' 不能赋值给 'string'
let sn: string | null = "bar";
sn = null; // 正确
```

## 32. 字面量类型

字面量类型允许你定义具体的值作为类型：
```typescript
let x: "hello" = "hello";
x = "hello"; // 正确
x = "howdy"; // 错误
```

## 33. 索引类型和映射类型

### 33.1 索引类型
```typescript
function pluck<T, K extends keyof T>(o: T, names: K[]): T[K][] {
  return names.map(n => o[n]);
}
```

### 33.2 映射类型
```typescript
type Readonly<T> = {
  readonly [P in keyof T]: T[P];
}
```

## 34. 泛型函数

泛型函数允许你编写可以处理多种类型的函数：
```typescript
function identity<T>(arg: T): T {
  return arg;
}
```

## 35. 泛型接口

泛型接口允许你定义可以处理多种类型的接口：
```typescript
interface GenericIdentityFn<T> {
  (arg: T): T;
}
```

## 36. 泛型类

泛型类允许你定义可以处理多种类型的类：
```typescript
class GenericNumber<