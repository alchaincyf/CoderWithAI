---
title: 封装和访问修饰符详解
date: 2023-10-05
description: 本课程详细讲解了面向对象编程中的封装概念及其核心组成部分——访问修饰符，帮助你理解如何通过封装来保护数据和方法，以及如何使用不同的访问修饰符来控制类成员的可见性。
slug: encapsulation-and-access-modifiers
tags:
  - 面向对象编程
  - 封装
  - 访问修饰符
category: 编程基础
keywords:
  - 封装
  - 访问修饰符
  - 面向对象编程
---

# 封装和访问修饰符

## 概述

封装是面向对象编程（OOP）的四大基本概念之一，其他三个是继承、多态和抽象。封装的主要目的是隐藏对象的内部状态和实现细节，只暴露必要的接口给外部使用。通过封装，我们可以控制对象的访问权限，提高代码的安全性和可维护性。

在Java中，封装通过访问修饰符来实现。访问修饰符决定了类、方法和变量的可见性。Java提供了四种访问修饰符：`public`、`private`、`protected` 和默认（不使用任何修饰符）。

## 访问修饰符

### 1. `public`

- **作用范围**：全局可见。
- **使用场景**：当你希望一个类、方法或变量在任何地方都可以被访问时，使用`public`修饰符。

```java
public class MyClass {
    public int publicVariable = 10;

    public void publicMethod() {
        System.out.println("This is a public method.");
    }
}
```

### 2. `private`

- **作用范围**：仅在当前类中可见。
- **使用场景**：当你希望一个方法或变量只能在当前类中访问时，使用`private`修饰符。通常用于隐藏类的内部实现细节。

```java
public class MyClass {
    private int privateVariable = 20;

    private void privateMethod() {
        System.out.println("This is a private method.");
    }
}
```

### 3. `protected`

- **作用范围**：在同一个包内和所有子类中可见。
- **使用场景**：当你希望一个方法或变量在同一个包内和所有子类中可以访问时，使用`protected`修饰符。

```java
public class MyClass {
    protected int protectedVariable = 30;

    protected void protectedMethod() {
        System.out.println("This is a protected method.");
    }
}
```

### 4. 默认（不使用任何修饰符）

- **作用范围**：在同一个包内可见。
- **使用场景**：当你希望一个方法或变量在同一个包内可以访问时，不使用任何修饰符。

```java
class MyClass {
    int defaultVariable = 40;

    void defaultMethod() {
        System.out.println("This is a default method.");
    }
}
```

## 封装实践

封装的核心思想是通过`private`修饰符隐藏类的内部状态，并通过`public`或`protected`方法提供对外的接口。这样可以确保类的内部状态不会被外部直接修改，从而提高代码的安全性和可维护性。

### 示例代码

```java
public class Student {
    private String name;
    private int age;

    // 构造方法
    public Student(String name, int age) {
        this.name = name;
        this.age = age;
    }

    // Getter方法
    public String getName() {
        return name;
    }

    // Setter方法
    public void setName(String name) {
        this.name = name;
    }

    // Getter方法
    public int getAge() {
        return age;
    }

    // Setter方法
    public void setAge(int age) {
        if (age > 0) {
            this.age = age;
        } else {
            System.out.println("Age must be a positive number.");
        }
    }

    // 显示学生信息的方法
    public void displayInfo() {
        System.out.println("Name: " + name);
        System.out.println("Age: " + age);
    }
}
```

### 解释

1. **私有变量**：`name` 和 `age` 被声明为`private`，这意味着它们只能在`Student`类内部访问。
2. **Getter和Setter方法**：通过`public`的`getName`、`setName`、`getAge`和`setAge`方法，外部代码可以访问和修改这些私有变量。
3. **数据验证**：在`setAge`方法中，我们添加了一个简单的验证逻辑，确保年龄是一个正数。

### 使用示例

```java
public class Main {
    public static void main(String[] args) {
        Student student = new Student("Alice", 20);
        student.displayInfo();

        // 修改学生信息
        student.setName("Bob");
        student.setAge(25);
        student.displayInfo();

        // 尝试设置无效的年龄
        student.setAge(-5);
        student.displayInfo();
    }
}
```

### 输出结果

```
Name: Alice
Age: 20
Name: Bob
Age: 25
Age must be a positive number.
Name: Bob
Age: 25
```

## 实践练习

### 练习1：银行账户类

创建一个`BankAccount`类，包含以下私有变量：
- `accountNumber`（账户号码）
- `balance`（余额）

提供以下公共方法：
- `deposit(double amount)`：存入指定金额。
- `withdraw(double amount)`：取出指定金额。如果余额不足，打印错误信息。
- `getBalance()`：返回当前余额。
- `displayAccountInfo()`：显示账户号码和余额。

### 练习2：图书类

创建一个`Book`类，包含以下私有变量：
- `title`（书名）
- `author`（作者）
- `isbn`（ISBN号码）

提供以下公共方法：
- `setTitle(String title)`：设置书名。
- `setAuthor(String author)`：设置作者。
- `setIsbn(String isbn)`：设置ISBN号码。
- `displayBookInfo()`：显示书名、作者和ISBN号码。

## 总结

封装是面向对象编程中的一个重要概念，通过访问修饰符可以控制类、方法和变量的可见性。合理使用封装可以提高代码的安全性和可维护性。通过实践练习，你可以更好地理解和掌握封装的概念和应用。