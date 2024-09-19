---
title: 类型别名 vs 接口：TypeScript中的选择
date: 2023-10-05
description: 本课程深入探讨TypeScript中类型别名和接口的区别与应用场景，帮助开发者更好地理解并选择适合的类型定义方式。
slug: typescript-type-aliases-vs-interfaces
tags:
  - TypeScript
  - 类型系统
  - 接口
category: 编程语言
keywords:
  - TypeScript类型别名
  - TypeScript接口
  - 类型定义
---

# 类型别名 vs 接口

在 TypeScript 中，类型别名（Type Aliases）和接口（Interfaces）是两种常用的定义数据结构的方式。虽然它们在很多情况下可以互换使用，但它们之间存在一些关键的区别。理解这些区别对于编写高效且易于维护的 TypeScript 代码至关重要。

## 1. 类型别名（Type Aliases）

类型别名允许你为现有的类型创建一个新的名字。它可以用于基本类型、联合类型、交叉类型、元组等。类型别名不会创建新的类型，它只是为现有类型提供一个别名。

### 1.1 基本用法

```typescript
type Name = string;
type Age = number;
type Person = {
    name: Name;
    age: Age;
};

const person: Person = {
    name: "Alice",
    age: 30
};
```

在这个例子中，`Name` 和 `Age` 是基本类型的别名，而 `Person` 是一个对象类型的别名。

### 1.2 联合类型和交叉类型

类型别名也可以用于联合类型和交叉类型：

```typescript
type ID = number | string;
type Employee = Person & { employeeId: ID };

const employee: Employee = {
    name: "Bob",
    age: 25,
    employeeId: "E12345"
};
```

在这个例子中，`ID` 是一个联合类型，而 `Employee` 是一个交叉类型。

## 2. 接口（Interfaces）

接口是 TypeScript 中定义对象结构的主要方式。它用于描述对象的形状，并且可以被类实现。接口可以扩展其他接口，从而实现继承。

### 2.1 基本用法

```typescript
interface Person {
    name: string;
    age: number;
}

const person: Person = {
    name: "Alice",
    age: 30
};
```

在这个例子中，`Person` 接口定义了一个对象的形状，包含 `name` 和 `age` 两个属性。

### 2.2 扩展接口

接口可以扩展其他接口，从而实现继承：

```typescript
interface Employee extends Person {
    employeeId: string;
}

const employee: Employee = {
    name: "Bob",
    age: 25,
    employeeId: "E12345"
};
```

在这个例子中，`Employee` 接口扩展了 `Person` 接口，并添加了一个新的属性 `employeeId`。

## 3. 类型别名 vs 接口

### 3.1 语法差异

- **类型别名** 使用 `type` 关键字：
  ```typescript
  type Name = string;
  ```

- **接口** 使用 `interface` 关键字：
  ```typescript
  interface Person {
      name: string;
      age: number;
  }
  ```

### 3.2 扩展性

- **类型别名** 不能被扩展，但可以通过交叉类型实现类似的效果：
  ```typescript
  type Employee = Person & { employeeId: string };
  ```

- **接口** 可以扩展其他接口：
  ```typescript
  interface Employee extends Person {
      employeeId: string;
  }
  ```

### 3.3 联合类型和交叉类型

- **类型别名** 非常适合用于联合类型和交叉类型：
  ```typescript
  type ID = number | string;
  ```

- **接口** 不能直接用于联合类型和交叉类型，但可以通过类型别名来实现：
  ```typescript
  type ID = number | string;
  interface Employee extends Person {
      employeeId: ID;
  }
  ```

### 3.4 类型合并

- **接口** 支持类型合并，即相同名称的接口会自动合并：
  ```typescript
  interface Person {
      name: string;
  }

  interface Person {
      age: number;
  }

  const person: Person = {
      name: "Alice",
      age: 30
  };
  ```

- **类型别名** 不支持类型合并，重复定义会报错：
  ```typescript
  type Person = {
      name: string;
  };

  type Person = { // 报错：Duplicate identifier 'Person'.
      age: number;
  };
  ```

## 4. 实践练习

### 4.1 练习1：定义一个类型别名

定义一个类型别名 `Coordinate`，表示一个二维坐标点，包含 `x` 和 `y` 两个属性，类型均为 `number`。

```typescript
type Coordinate = {
    x: number;
    y: number;
};

const point: Coordinate = {
    x: 10,
    y: 20
};
```

### 4.2 练习2：定义一个接口

定义一个接口 `Shape`，表示一个几何形状，包含 `name` 属性（类型为 `string`）和 `area` 方法（返回类型为 `number`）。

```typescript
interface Shape {
    name: string;
    area(): number;
}

class Circle implements Shape {
    constructor(public radius: number) {}

    name = "Circle";

    area(): number {
        return Math.PI * this.radius ** 2;
    }
}

const circle = new Circle(5);
console.log(circle.area()); // 输出: 78.53981633974483
```

### 4.3 练习3：扩展接口

定义一个接口 `Rectangle`，扩展 `Shape` 接口，并添加 `width` 和 `height` 属性（类型均为 `number`）。

```typescript
interface Rectangle extends Shape {
    width: number;
    height: number;
}

class RectangleImpl implements Rectangle {
    constructor(public width: number, public height: number) {}

    name = "Rectangle";

    area(): number {
        return this.width * this.height;
    }
}

const rectangle = new RectangleImpl(10, 20);
console.log(rectangle.area()); // 输出: 200
```

## 5. 总结

类型别名和接口是 TypeScript 中定义数据结构的两种主要方式。类型别名更适合用于基本类型、联合类型和交叉类型，而接口更适合用于定义对象的形状，并且支持扩展和类型合并。在实际开发中，根据具体需求选择合适的方式来定义类型，可以使代码更加清晰和易于维护。

通过本教程的学习，你应该能够理解类型别名和接口的区别，并能够在实际项目中灵活运用它们。