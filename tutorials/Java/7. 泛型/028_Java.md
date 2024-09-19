---
title: 深入理解Java中的泛型类和接口
date: 2023-10-05
description: 本课程详细讲解Java中泛型类和接口的概念、用法及其在实际编程中的应用，帮助开发者更好地理解和使用泛型。
slug: java-generics-classes-interfaces
tags:
  - Java
  - 泛型
  - 编程基础
category: 编程语言
keywords:
  - Java泛型
  - 泛型类
  - 泛型接口
---

# 泛型类和接口

## 1. 概述

泛型（Generics）是Java编程语言中的一项强大功能，它允许你编写可以处理多种数据类型的代码，而不需要为每种数据类型编写单独的实现。泛型类和接口是泛型编程的基础，它们使得代码更加灵活、可重用和类型安全。

## 2. 泛型类的定义

泛型类是使用类型参数（Type Parameter）定义的类。类型参数通常用大写字母表示，如`T`、`E`、`K`、`V`等。泛型类的定义语法如下：

```java
public class GenericClass<T> {
    private T value;

    public GenericClass(T value) {
        this.value = value;
    }

    public T getValue() {
        return value;
    }

    public void setValue(T value) {
        this.value = value;
    }
}
```

在这个例子中，`GenericClass`是一个泛型类，它有一个类型参数`T`。`T`可以是任何数据类型，如`Integer`、`String`、`Double`等。

### 2.1 使用泛型类

你可以通过在实例化泛型类时指定具体的类型来使用它：

```java
public class Main {
    public static void main(String[] args) {
        GenericClass<Integer> intObj = new GenericClass<>(10);
        System.out.println("Integer value: " + intObj.getValue());

        GenericClass<String> strObj = new GenericClass<>("Hello");
        System.out.println("String value: " + strObj.getValue());
    }
}
```

在这个例子中，我们分别创建了一个`GenericClass<Integer>`和一个`GenericClass<String>`对象，并分别存储了整数和字符串类型的值。

## 3. 泛型接口的定义

泛型接口的定义与泛型类类似，也是使用类型参数来定义的。泛型接口的定义语法如下：

```java
public interface GenericInterface<T> {
    T getValue();
    void setValue(T value);
}
```

### 3.1 实现泛型接口

你可以通过在实现类中指定具体的类型来实现泛型接口：

```java
public class GenericImpl<T> implements GenericInterface<T> {
    private T value;

    @Override
    public T getValue() {
        return value;
    }

    @Override
    public void setValue(T value) {
        this.value = value;
    }
}
```

### 3.2 使用泛型接口

你可以通过在实例化实现类时指定具体的类型来使用泛型接口：

```java
public class Main {
    public static void main(String[] args) {
        GenericImpl<Double> doubleObj = new GenericImpl<>();
        doubleObj.setValue(3.14);
        System.out.println("Double value: " + doubleObj.getValue());

        GenericImpl<Boolean> boolObj = new GenericImpl<>();
        boolObj.setValue(true);
        System.out.println("Boolean value: " + boolObj.getValue());
    }
}
```

在这个例子中，我们分别创建了一个`GenericImpl<Double>`和一个`GenericImpl<Boolean>`对象，并分别存储了双精度浮点数和布尔类型的值。

## 4. 泛型方法

除了泛型类和泛型接口，Java还支持泛型方法。泛型方法是在方法声明中使用类型参数的方法。泛型方法的定义语法如下：

```java
public class GenericMethodExample {
    public static <T> void printArray(T[] array) {
        for (T element : array) {
            System.out.println(element);
        }
    }
}
```

### 4.1 使用泛型方法

你可以通过在调用泛型方法时指定具体的类型来使用它：

```java
public class Main {
    public static void main(String[] args) {
        Integer[] intArray = {1, 2, 3, 4, 5};
        GenericMethodExample.printArray(intArray);

        String[] strArray = {"A", "B", "C", "D"};
        GenericMethodExample.printArray(strArray);
    }
}
```

在这个例子中，我们分别调用了`printArray`方法来打印整数数组和字符串数组。

## 5. 类型擦除

Java的泛型是通过类型擦除（Type Erasure）实现的。类型擦除意味着在编译时，泛型类型参数会被替换为它们的边界类型（通常是`Object`），并且在运行时不会保留泛型类型的信息。

### 5.1 类型擦除的影响

类型擦除的影响包括：

- 无法在运行时获取泛型类型的具体信息。
- 无法创建泛型类型的数组。
- 无法在泛型类型上使用`instanceof`操作符。

### 5.2 示例

```java
public class TypeErasureExample {
    public static void main(String[] args) {
        List<String> stringList = new ArrayList<>();
        List<Integer> intList = new ArrayList<>();

        // 编译错误：无法创建泛型类型的数组
        // List<String>[] arrayOfLists = new List<String>[10];

        // 编译错误：无法在泛型类型上使用 instanceof
        // if (stringList instanceof List<String>) { }
    }
}
```

## 6. 通配符

通配符（Wildcard）是泛型编程中的一个重要概念，它允许你在泛型类型中使用不确定的类型。通配符用`?`表示。

### 6.1 无界通配符

无界通配符表示任何类型：

```java
public void printList(List<?> list) {
    for (Object element : list) {
        System.out.println(element);
    }
}
```

### 6.2 上界通配符

上界通配符表示某个类型及其子类型：

```java
public void printNumberList(List<? extends Number> list) {
    for (Number number : list) {
        System.out.println(number);
    }
}
```

### 6.3 下界通配符

下界通配符表示某个类型及其父类型：

```java
public void addNumbers(List<? super Integer> list) {
    list.add(1);
    list.add(2);
}
```

## 7. 实践练习

### 7.1 练习1：实现一个泛型栈

实现一个泛型栈类`GenericStack`，支持`push`、`pop`和`peek`操作。

```java
public class GenericStack<T> {
    private List<T> stack = new ArrayList<>();

    public void push(T item) {
        stack.add(item);
    }

    public T pop() {
        if (stack.isEmpty()) {
            throw new IllegalStateException("Stack is empty");
        }
        return stack.remove(stack.size() - 1);
    }

    public T peek() {
        if (stack.isEmpty()) {
            throw new IllegalStateException("Stack is empty");
        }
        return stack.get(stack.size() - 1);
    }

    public boolean isEmpty() {
        return stack.isEmpty();
    }
}
```

### 7.2 练习2：实现一个泛型队列

实现一个泛型队列类`GenericQueue`，支持`enqueue`、`dequeue`和`peek`操作。

```java
public class GenericQueue<T> {
    private List<T> queue = new LinkedList<>();

    public void enqueue(T item) {
        queue.add(item);
    }

    public T dequeue() {
        if (queue.isEmpty()) {
            throw new IllegalStateException("Queue is empty");
        }
        return queue.remove(0);
    }

    public T peek() {
        if (queue.isEmpty()) {
            throw new IllegalStateException("Queue is empty");
        }
        return queue.get(0);
    }

    public boolean isEmpty() {
        return queue.isEmpty();
    }
}
```

## 8. 总结

泛型类和接口是Java泛型编程的基础，它们使得代码更加灵活、可重用和类型安全。通过类型参数，你可以编写处理多种数据类型的代码，而不需要为每种数据类型编写单独的实现。泛型方法、类型擦除和通配符是泛型编程中的重要概念，理解它们将帮助你更好地利用泛型功能。

通过实践练习，你可以更好地掌握泛型类和接口的使用，并能够在实际项目中灵活应用泛型编程技术。