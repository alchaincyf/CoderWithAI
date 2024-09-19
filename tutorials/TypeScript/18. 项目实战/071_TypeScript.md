---
title: 全栈 TypeScript 应用开发教程
date: 2023-10-05
description: 本课程将带你从零开始，学习如何使用 TypeScript 开发全栈应用，涵盖前端、后端及数据库的完整技术栈。
slug: full-stack-typescript-development
tags:
  - TypeScript
  - 全栈开发
  - 前端开发
  - 后端开发
category: 编程教程
keywords:
  - TypeScript
  - 全栈开发
  - 前端
  - 后端
  - 数据库
---

# 全栈 TypeScript 应用开发教程

## 1. TypeScript 简介和优势

### 1.1 什么是 TypeScript？
TypeScript 是一种由微软开发的开源编程语言，它是 JavaScript 的超集，添加了可选的静态类型和基于类的面向对象编程。TypeScript 最终会被编译成纯 JavaScript，因此可以在任何支持 JavaScript 的环境中运行。

### 1.2 TypeScript 的优势
- **静态类型检查**：在编译阶段就能发现类型错误，减少运行时错误。
- **更好的代码提示和文档**：IDE 可以提供更好的代码补全和错误提示。
- **面向对象编程**：支持类、接口、继承等面向对象特性。
- **更好的可维护性**：类型系统使得代码更易于理解和维护。

## 2. 开发环境搭建

### 2.1 安装 Node.js
首先，你需要安装 Node.js。Node.js 是一个基于 Chrome V8 引擎的 JavaScript 运行时环境，它允许你在服务器端运行 JavaScript。

```bash
# 安装 Node.js
curl -fsSL https://deb.nodesource.com/setup_14.x | sudo -E bash -
sudo apt-get install -y nodejs
```

### 2.2 安装 TypeScript 编译器
TypeScript 编译器（`tsc`）可以将 TypeScript 代码编译成 JavaScript 代码。

```bash
# 全局安装 TypeScript 编译器
npm install -g typescript
```

### 2.3 验证安装
安装完成后，你可以通过以下命令验证 TypeScript 是否安装成功：

```bash
tsc --version
```

## 3. 第一个 TypeScript 程序

### 3.1 创建项目目录
首先，创建一个新的项目目录并进入该目录：

```bash
mkdir my-typescript-project
cd my-typescript-project
```

### 3.2 初始化项目
使用 `npm` 初始化项目：

```bash
npm init -y
```

### 3.3 编写第一个 TypeScript 程序
创建一个名为 `index.ts` 的文件，并编写以下代码：

```typescript
// index.ts
let message: string = "Hello, TypeScript!";
console.log(message);
```

### 3.4 编译 TypeScript 代码
使用 `tsc` 命令编译 TypeScript 代码：

```bash
tsc index.ts
```

编译完成后，会生成一个 `index.js` 文件。

### 3.5 运行 JavaScript 代码
使用 Node.js 运行生成的 JavaScript 代码：

```bash
node index.js
```

你应该会看到输出：

```
Hello, TypeScript!
```

## 4. 基本类型和变量声明

### 4.1 基本类型
TypeScript 支持以下基本类型：
- `number`：表示数字类型，包括整数和浮点数。
- `string`：表示字符串类型。
- `boolean`：表示布尔类型，值为 `true` 或 `false`。
- `array`：表示数组类型，可以指定数组元素的类型。
- `tuple`：表示元组类型，允许数组中的元素具有不同的类型。

### 4.2 变量声明
在 TypeScript 中，你可以使用 `let` 或 `const` 关键字声明变量，并指定其类型。

```typescript
let age: number = 25;
let name: string = "Alice";
let isStudent: boolean = true;
let numbers: number[] = [1, 2, 3];
let person: [string, number] = ["Alice", 25];
```

## 5. TypeScript 配置文件 (tsconfig.json)

### 5.1 创建 tsconfig.json
`tsconfig.json` 文件用于配置 TypeScript 编译器的选项。你可以通过以下命令生成一个默认的 `tsconfig.json` 文件：

```bash
tsc --init
```

### 5.2 配置选项
`tsconfig.json` 文件中包含许多配置选项，例如：
- `target`：指定编译后的 JavaScript 版本。
- `module`：指定模块系统。
- `outDir`：指定编译后文件的输出目录。
- `strict`：启用严格的类型检查。

```json
{
  "compilerOptions": {
    "target": "ES6",
    "module": "CommonJS",
    "outDir": "./dist",
    "strict": true
  }
}
```

## 6. 基本类型详解

### 6.1 Number 类型
TypeScript 中的 `number` 类型用于表示数字，包括整数和浮点数。

```typescript
let decimal: number = 6;
let hex: number = 0xf00d;
let binary: number = 0b1010;
let octal: number = 0o744;
```

### 6.2 String 类型
`string` 类型用于表示字符串。

```typescript
let color: string = "blue";
color = 'red';
```

### 6.3 Boolean 类型
`boolean` 类型用于表示布尔值。

```typescript
let isDone: boolean = false;
```

### 6.4 Array 类型
`array` 类型用于表示数组，可以指定数组元素的类型。

```typescript
let list: number[] = [1, 2, 3];
let list2: Array<number> = [1, 2, 3];
```

### 6.5 Tuple 类型
`tuple` 类型允许数组中的元素具有不同的类型。

```typescript
let x: [string, number];
x = ["hello", 10]; // OK
x = [10, "hello"]; // Error
```

## 7. 枚举类型

### 7.1 枚举的定义
枚举类型用于定义一组命名的常量。

```typescript
enum Color {
  Red,
  Green,
  Blue
}
let c: Color = Color.Green;
```

### 7.2 枚举的值
默认情况下，枚举的值从 0 开始递增，但你也可以手动指定枚举的值。

```typescript
enum Color {
  Red = 1,
  Green = 2,
  Blue = 4
}
let c: Color = Color.Green;
```

## 8. Any 和 Unknown 类型

### 8.1 Any 类型
`any` 类型允许你绕过类型检查，适用于不确定类型的变量。

```typescript
let notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // okay, definitely a boolean
```

### 8.2 Unknown 类型
`unknown` 类型类似于 `any`，但它更安全，因为它不允许你直接操作 `unknown` 类型的值。

```typescript
let notSure: unknown = 4;
notSure = "maybe a string instead";
notSure = false; // okay, definitely a boolean

let value: unknown = notSure;
if (typeof value === "string") {
  console.log(value.length); // 现在可以安全地操作 value
}
```

## 9. Void, Null 和 Undefined

### 9.1 Void 类型
`void` 类型表示没有任何类型。通常用于函数的返回类型，表示函数不返回任何值。

```typescript
function warnUser(): void {
  console.log("This is my warning message");
}
```

### 9.2 Null 和 Undefined
`null` 和 `undefined` 是所有类型的子类型。默认情况下，它们可以赋值给任何类型，但可以通过 `strictNullChecks` 选项来限制这种行为。

```typescript
let u: undefined = undefined;
let n: null = null;
```

## 10. Never 类型

### 10.1 Never 类型
`never` 类型表示那些永不存在的值的类型。例如，一个总是抛出异常的函数或一个无限循环的函数。

```typescript
function error(message: string): never {
  throw new Error(message);
}

function infiniteLoop(): never {
  while (true) {
  }
}
```

## 11. 类型断言

### 11.1 类型断言
类型断言允许你告诉编译器“相信我，我知道自己在做什么”。它类似于类型转换，但不进行特殊的数据检查和解构。

```typescript
let someValue: any = "this is a string";
let strLength: number = (someValue as string).length;
```

## 12. 函数声明和表达式

### 12.1 函数声明
函数声明使用 `function` 关键字定义函数。

```typescript
function add(x: number, y: number): number {
  return x + y;
}
```

### 12.2 函数表达式
函数表达式将函数赋值给一个变量。

```typescript
let add = function(x: number, y: number): number {
  return x + y;
};
```

## 13. 可选参数和默认参数

### 13.1 可选参数
在参数名后添加 `?` 表示该参数是可选的。

```typescript
function buildName(firstName: string, lastName?: string) {
  if (lastName)
    return firstName + " " + lastName;
  else
    return firstName;
}
```

### 13.2 默认参数
默认参数在参数名后使用 `=` 指定默认值。

```typescript
function buildName(firstName: string, lastName = "Smith") {
  return firstName + " " + lastName;
}
```

## 14. 剩余参数

### 14.1 剩余参数
剩余参数允许你将多个参数收集到一个数组中。

```typescript
function buildName(firstName: string, ...restOfName: string[]) {
  return firstName + " " + restOfName.join(" ");
}
```

## 15. 函数重载

### 15.1 函数重载
函数重载允许你定义多个具有相同名称但参数类型或数量不同的函数。

```typescript
function add(a: number, b: number): number;
function add(a: string, b: string): string;
function add(a: any, b: any): any {
  return a + b;
}
```

## 16. This 和箭头函数

### 16.1 This 关键字
`this` 关键字在 JavaScript 中具有动态上下文，但在 TypeScript 中可以通过类型注解来明确其类型。

```typescript
let deck = {
  suits: ["hearts", "spades", "clubs", "diamonds"],
  cards: Array(52),
  createCardPicker: function() {
    return () => {
      let pickedCard = Math.floor(Math.random() * 52);
      let pickedSuit = Math.floor(pickedCard / 13);

      return {suit: this.suits[pickedSuit], card: pickedCard % 13};
    }
  }
}
```

### 16.2 箭头函数
箭头函数提供了一种更简洁的函数定义方式，并且不会改变 `this` 的上下文。

```typescript
let deck = {
  suits: ["hearts", "spades", "clubs", "diamonds"],
  cards: Array(52),
  createCardPicker: function() {
    return () => {
      let pickedCard = Math.floor(Math.random() * 52);
      let pickedSuit = Math.floor(pickedCard / 13);

      return {suit: this.suits[pickedSuit], card: pickedCard % 13};
    }
  }
}
```

## 17. 接口定义和使用

### 17.1 接口定义
接口用于定义对象的形状。

```typescript
interface Person {
  firstName: string;
  lastName: string;
}

function greeter(person: Person) {
  return "Hello, " + person.firstName + " " + person.lastName;
}
```

### 17.2 接口使用
接口可以用于约束对象的结构。

```typescript
let user = { firstName: "Jane", lastName: "User" };
console.log(greeter(user));
```

## 18. 可选属性和只读属性

### 18.1 可选属性
在属性名后添加 `?` 表示该属性是可选的。

```typescript
interface SquareConfig {
  color?: string;
  width?: number;
}
```

### 18.2 只读属性
使用 `readonly` 关键字定义只读属性。

```typescript
interface Point {
  readonly x: number;
  readonly y: number;
}
```

## 19. 函数类型接口

### 19.1 函数类型接口
接口可以用于定义函数的类型。

```typescript
interface SearchFunc {
  (source: string, subString: string): boolean;
}

let mySearch: SearchFunc;
mySearch = function(source: string, subString: string) {
  let result = source.search(subString);
  return result > -1;
}
```

## 20. 类类型接口

### 20.1 类类型接口
接口可以用于定义类的结构。

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
  constructor(h: number, m: number) { }
}
```

## 21. 继承接口

### 21.1 继承接口
接口可以继承其他接口。

```typescript
interface Shape {
  color: string;
}

interface Square extends Shape {
  sideLength: number;
}

let square = {} as Square;
square.color = "blue";
square.sideLength = 10;
```

## 22. 类型别名 vs 接口

### 22.1 类型别名
类型别名用于给类型起一个新名字。

```typescript
type Name = string;
type NameResolver = () => string;
type NameOrResolver = Name | NameResolver;
```

### 22.2 接口 vs 类型别名
接口和类型别名都可以用于定义对象的形状，但接口更适合用于定义对象的结构，而类型别名更适合用于定义复杂类型。

## 23. 类的定义和实例化

### 23.1 类的定义
类用于定义对象的蓝图。

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
使用 `new` 关键字实例化类。

```typescript
let greeter = new Greeter("world");
console.log(greeter.greet());
```

## 24. 继承和多态

### 24.1 继承
类可以继承其他类，从而继承父类的属性和方法。

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

const dog = new Dog();
dog.bark();
dog.move(10);
```

### 24.2 多态
多态允许子类重写父类的方法。

```typescript
class Animal {
  makeSound() {
    console.log("Animal sound");
  }
}

class Dog extends Animal {
  makeSound() {
    console.log("Woof! Woof!");
  }
}

const animal: Animal = new Dog();
animal.makeSound(); // 输出: Woof! Woof!
```

## 25. 访问修饰符 (public, private, protected)

### 25.1 Public 修饰符
默认情况下，类的成员是 `public` 的，可以在任何地方访问。

```typescript
class Animal {
  public name: string;
  public constructor(theName: string) { this.name = theName; }
  public move(distanceInMeters: number) {
    console.log(`${this.name} moved ${distanceInMeters}m.`);
 