---
title: 动态内存分配 (new 和 delete) 教程
date: 2023-10-05
description: 本课程详细讲解C++中的动态内存分配，包括如何使用new和delete操作符来管理内存，避免内存泄漏和提高程序效率。
slug: dynamic-memory-allocation-new-delete
tags:
  - C++
  - 内存管理
  - 编程基础
category: 编程基础
keywords:
  - 动态内存分配
  - new
  - delete
  - 内存管理
  - C++编程
---

# 动态内存分配 (new 和 delete)

## 概述

在C++中，动态内存分配是指在程序运行时根据需要分配和释放内存的过程。这与静态内存分配不同，静态内存分配在编译时就已经确定了内存的大小。动态内存分配允许程序在运行时根据需要创建和销毁对象，从而提高内存使用的灵活性和效率。

C++提供了两个关键字`new`和`delete`来实现动态内存分配和释放。`new`用于分配内存并构造对象，`delete`用于销毁对象并释放内存。

## 1. `new` 关键字

### 1.1 基本用法

`new`关键字用于在堆上分配内存，并返回指向该内存的指针。基本语法如下：

```cpp
Type* pointer = new Type;
```

其中，`Type`是数据类型，`pointer`是指向该类型的指针。

### 1.2 示例代码

```cpp
#include <iostream>

int main() {
    int* pInt = new int;  // 分配一个int类型的内存
    *pInt = 10;           // 给分配的内存赋值
    std::cout << *pInt << std::endl;  // 输出10
    delete pInt;          // 释放内存
    return 0;
}
```

### 1.3 分配数组

`new`还可以用于分配数组，语法如下：

```cpp
Type* pointer = new Type[size];
```

其中，`size`是数组的大小。

### 1.4 示例代码

```cpp
#include <iostream>

int main() {
    int* pArray = new int[5];  // 分配一个包含5个int的数组
    for (int i = 0; i < 5; ++i) {
        pArray[i] = i * 10;
    }
    for (int i = 0; i < 5; ++i) {
        std::cout << pArray[i] << " ";
    }
    std::cout << std::endl;
    delete[] pArray;  // 释放数组内存
    return 0;
}
```

## 2. `delete` 关键字

### 2.1 基本用法

`delete`关键字用于释放由`new`分配的内存。基本语法如下：

```cpp
delete pointer;
```

其中，`pointer`是指向动态分配内存的指针。

### 2.2 示例代码

```cpp
#include <iostream>

int main() {
    int* pInt = new int;
    *pInt = 10;
    std::cout << *pInt << std::endl;
    delete pInt;  // 释放内存
    return 0;
}
```

### 2.3 释放数组

释放数组内存时，需要使用`delete[]`，语法如下：

```cpp
delete[] pointer;
```

### 2.4 示例代码

```cpp
#include <iostream>

int main() {
    int* pArray = new int[5];
    for (int i = 0; i < 5; ++i) {
        pArray[i] = i * 10;
    }
    for (int i = 0; i < 5; ++i) {
        std::cout << pArray[i] << " ";
    }
    std::cout << std::endl;
    delete[] pArray;  // 释放数组内存
    return 0;
}
```

## 3. 内存泄漏

### 3.1 什么是内存泄漏

内存泄漏是指程序在动态分配内存后，未能正确释放该内存，导致内存资源被浪费。内存泄漏会导致程序的内存使用量逐渐增加，最终可能导致程序崩溃。

### 3.2 示例代码

```cpp
#include <iostream>

int main() {
    int* pInt = new int;
    *pInt = 10;
    std::cout << *pInt << std::endl;
    // 忘记释放内存
    return 0;
}
```

### 3.3 避免内存泄漏

为了避免内存泄漏，应该始终确保在动态分配内存后，使用`delete`或`delete[]`释放内存。

## 4. 实践练习

### 4.1 练习1：动态分配和释放单个对象

编写一个程序，动态分配一个`double`类型的内存，并将其初始化为3.14，然后输出该值，最后释放内存。

### 4.2 练习2：动态分配和释放数组

编写一个程序，动态分配一个包含10个`float`类型的数组，并将其初始化为0.0到9.0，然后输出数组中的所有值，最后释放数组内存。

### 4.3 练习3：避免内存泄漏

修改以下代码，确保在程序结束前释放所有动态分配的内存。

```cpp
#include <iostream>

int main() {
    int* pInt = new int;
    *pInt = 10;
    std::cout << *pInt << std::endl;
    // 忘记释放内存
    return 0;
}
```

## 5. 总结

动态内存分配是C++中一个强大的特性，允许程序在运行时根据需要分配和释放内存。通过使用`new`和`delete`，可以有效地管理内存资源，避免内存泄漏。然而，动态内存管理也需要谨慎，确保在适当的时候释放内存，以避免资源浪费和程序崩溃。

通过本教程的学习，你应该能够理解动态内存分配的基本概念，并能够在实际编程中正确使用`new`和`delete`。