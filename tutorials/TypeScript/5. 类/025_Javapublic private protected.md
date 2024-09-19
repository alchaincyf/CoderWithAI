---
title: 深入理解Java中的访问修饰符：public, private, protected
date: 2023-10-05
description: 本课程详细讲解Java中的访问修饰符public, private, protected，帮助你掌握类成员的访问控制，提升代码的安全性和可维护性。
slug: java-access-modifiers
tags:
  - Java
  - 面向对象编程
  - 访问控制
category: 编程基础
keywords:
  - Java访问修饰符
  - public修饰符
  - private修饰符
  - protected修饰符
---

# 访问修饰符 (public, private, protected)

在 TypeScript 中，访问修饰符用于控制类成员（属性、方法）的可访问性。通过使用访问修饰符，我们可以定义哪些成员可以在类的外部访问，哪些只能在类的内部访问，以及哪些可以在子类中访问。TypeScript 提供了三种主要的访问修饰符：`public`、`private` 和 `protected`。

## 1. `public` 修饰符

`public` 是默认的访问修饰符。如果一个类成员没有显式地指定访问修饰符，那么它默认是 `public` 的。`public` 修饰符表示该成员可以在任何地方访问，包括类的外部、类的内部以及子类中。

### 代码示例

```typescript
class Person {
    public name: string;

    constructor(name: string) {
        this.name = name;
    }

    public greet(): void {
        console.log(`Hello, my name is ${this.name}`);
    }
}

const person = new Person("Alice");
person.name = "Bob"; // 可以访问
person.greet(); // 可以访问
```

### 解释

- `name` 属性和 `greet` 方法都被标记为 `public`，因此它们可以在类的外部访问。

## 2. `private` 修饰符

`private` 修饰符表示该成员只能在类的内部访问，不能在类的外部或子类中访问。

### 代码示例

```typescript
class Person {
    private name: string;

    constructor(name: string) {
        this.name = name;
    }

    public greet(): void {
        console.log(`Hello, my name is ${this.name}`);
    }
}

const person = new Person("Alice");
// person.name = "Bob"; // 错误：属性 'name' 是私有的，只能在类 'Person' 中访问
person.greet(); // 可以访问
```

### 解释

- `name` 属性被标记为 `private`，因此它只能在 `Person` 类的内部访问，不能在类的外部或子类中访问。

## 3. `protected` 修饰符

`protected` 修饰符表示该成员可以在类的内部和子类中访问，但不能在类的外部访问。

### 代码示例

```typescript
class Person {
    protected name: string;

    constructor(name: string) {
        this.name = name;
    }

    public greet(): void {
        console.log(`Hello, my name is ${this.name}`);
    }
}

class Employee extends Person {
    private department: string;

    constructor(name: string, department: string) {
        super(name);
        this.department = department;
    }

    public introduce(): void {
        console.log(`Hello, my name is ${this.name} and I work in the ${this.department} department.`);
    }
}

const person = new Person("Alice");
// person.name = "Bob"; // 错误：属性 'name' 是受保护的，只能在类 'Person' 及其子类中访问
person.greet(); // 可以访问

const employee = new Employee("Charlie", "Engineering");
employee.introduce(); // 可以访问
```

### 解释

- `name` 属性被标记为 `protected`，因此它可以在 `Person` 类及其子类（如 `Employee`）中访问，但不能在类的外部访问。

## 4. 实践练习

### 练习 1：创建一个 `BankAccount` 类

1. 创建一个 `BankAccount` 类，包含以下属性：
   - `accountNumber`（私有）
   - `balance`（受保护）
   - `owner`（公共）

2. 添加一个构造函数，用于初始化 `accountNumber`、`balance` 和 `owner`。

3. 添加一个 `deposit` 方法，用于向账户中存入金额。

4. 添加一个 `withdraw` 方法，用于从账户中取出金额。

5. 添加一个 `getBalance` 方法，用于获取当前账户余额。

### 练习 2：创建一个 `SavingsAccount` 子类

1. 创建一个 `SavingsAccount` 类，继承自 `BankAccount`。

2. 添加一个 `interestRate` 属性（私有）。

3. 重写 `deposit` 方法，使其在存入金额时自动添加利息。

4. 添加一个 `applyInterest` 方法，用于手动应用利息。

### 代码示例

```typescript
class BankAccount {
    private accountNumber: string;
    protected balance: number;
    public owner: string;

    constructor(accountNumber: string, balance: number, owner: string) {
        this.accountNumber = accountNumber;
        this.balance = balance;
        this.owner = owner;
    }

    public deposit(amount: number): void {
        this.balance += amount;
    }

    public withdraw(amount: number): void {
        if (amount > this.balance) {
            console.log("Insufficient funds");
        } else {
            this.balance -= amount;
        }
    }

    public getBalance(): number {
        return this.balance;
    }
}

class SavingsAccount extends BankAccount {
    private interestRate: number;

    constructor(accountNumber: string, balance: number, owner: string, interestRate: number) {
        super(accountNumber, balance, owner);
        this.interestRate = interestRate;
    }

    public deposit(amount: number): void {
        super.deposit(amount);
        this.applyInterest();
    }

    public applyInterest(): void {
        this.balance += this.balance * this.interestRate;
    }
}

const account = new BankAccount("123456789", 1000, "Alice");
account.deposit(500);
account.withdraw(200);
console.log(account.getBalance()); // 输出: 1300

const savingsAccount = new SavingsAccount("987654321", 2000, "Bob", 0.05);
savingsAccount.deposit(1000);
console.log(savingsAccount.getBalance()); // 输出: 3150 (2000 + 1000 + 150 利息)
```

### 解释

- `BankAccount` 类包含私有属性 `accountNumber`、受保护属性 `balance` 和公共属性 `owner`。
- `SavingsAccount` 类继承自 `BankAccount`，并添加了私有属性 `interestRate`。
- `SavingsAccount` 类重写了 `deposit` 方法，使其在存入金额时自动应用利息。

通过这些练习，你应该能够更好地理解 TypeScript 中的访问修饰符及其在类和继承中的应用。