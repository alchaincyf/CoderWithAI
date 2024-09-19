---
title: 深入理解C++中的范围for循环
date: 2023-10-05
description: 本课程详细讲解C++中的范围for循环，包括其语法、使用场景以及与传统for循环的对比，帮助你更高效地遍历集合和数组。
slug: cpp-range-based-for-loop
tags:
  - C++
  - 循环
  - 编程基础
category: 编程语言
keywords:
  - C++范围for循环
  - 范围for循环
  - C++循环
---

# 范围for循环

## 概述

范围for循环（Range-based for loop）是C++11引入的一种简化循环语法，用于遍历容器（如数组、向量、列表等）中的元素。它提供了一种简洁、直观的方式来遍历集合中的每个元素，而不需要显式地使用索引或迭代器。

## 语法

范围for循环的基本语法如下：

```cpp
for (元素类型 元素变量 : 容器) {
    // 循环体
}
```

- `元素类型`：表示容器中元素的类型。
- `元素变量`：表示当前遍历到的元素。
- `容器`：表示要遍历的容器对象。

## 示例代码

### 示例1：遍历数组

```cpp
#include <iostream>

int main() {
    int arr[] = {1, 2, 3, 4, 5};

    for (int num : arr) {
        std::cout << num << " ";
    }

    return 0;
}
```

**输出：**

```
1 2 3 4 5
```

### 示例2：遍历向量

```cpp
#include <iostream>
#include <vector>

int main() {
    std::vector<int> vec = {10, 20, 30, 40, 50};

    for (int num : vec) {
        std::cout << num << " ";
    }

    return 0;
}
```

**输出：**

```
10 20 30 40 50
```

### 示例3：遍历字符串

```cpp
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, World!";

    for (char ch : str) {
        std::cout << ch << " ";
    }

    return 0;
}
```

**输出：**

```
H e l l o ,   W o r l d !
```

## 自动类型推导

在C++11及更高版本中，可以使用`auto`关键字来自动推导元素类型，使代码更加简洁。

### 示例4：使用`auto`自动推导类型

```cpp
#include <iostream>
#include <vector>

int main() {
    std::vector<int> vec = {100, 200, 300, 400, 500};

    for (auto num : vec) {
        std::cout << num << " ";
    }

    return 0;
}
```

**输出：**

```
100 200 300 400 500
```

## 修改容器元素

范围for循环中的元素变量默认是只读的，如果需要修改容器中的元素，可以使用引用类型。

### 示例5：修改容器元素

```cpp
#include <iostream>
#include <vector>

int main() {
    std::vector<int> vec = {1, 2, 3, 4, 5};

    for (int& num : vec) {
        num *= 2;  // 将每个元素乘以2
    }

    for (int num : vec) {
        std::cout << num << " ";
    }

    return 0;
}
```

**输出：**

```
2 4 6 8 10
```

## 实践练习

### 练习1：计算数组元素的和

编写一个程序，使用范围for循环计算并输出数组中所有元素的和。

```cpp
#include <iostream>

int main() {
    int arr[] = {1, 2, 3, 4, 5};
    int sum = 0;

    for (int num : arr) {
        sum += num;
    }

    std::cout << "Sum of array elements: " << sum << std::endl;

    return 0;
}
```

**输出：**

```
Sum of array elements: 15
```

### 练习2：统计字符串中某个字符的出现次数

编写一个程序，使用范围for循环统计字符串中某个字符的出现次数。

```cpp
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, World!";
    char target = 'o';
    int count = 0;

    for (char ch : str) {
        if (ch == target) {
            count++;
        }
    }

    std::cout << "The character '" << target << "' appears " << count << " times." << std::endl;

    return 0;
}
```

**输出：**

```
The character 'o' appears 2 times.
```

## 总结

范围for循环是C++中一种非常方便的循环结构，特别适用于遍历容器中的元素。它简化了代码，提高了可读性，并且可以通过`auto`关键字自动推导类型。通过引用类型，还可以方便地修改容器中的元素。掌握范围for循环的使用，将大大提高你的编程效率。

## 下一步

接下来，我们将学习C++中的初始化列表，它与范围for循环结合使用，可以实现更加灵活的初始化和遍历操作。