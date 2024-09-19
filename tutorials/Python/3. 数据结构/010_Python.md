---
title: 深入理解Python中的字典和集合
date: 2023-10-05
description: 本课程将深入探讨Python中的字典和集合，包括它们的创建、操作、性能优化以及在实际编程中的应用。
slug: python-dictionaries-and-sets
tags:
  - Python
  - 数据结构
  - 字典
  - 集合
category: 编程基础
keywords:
  - Python字典
  - Python集合
  - 数据结构
  - 字典操作
  - 集合操作
---

# 字典和集合

## 1. 字典 (Dictionary)

### 1.1 什么是字典？

字典是Python中的一种数据结构，用于存储键值对（key-value pairs）。每个键（key）都与一个值（value）相关联，键是唯一的，而值可以是任何数据类型。字典是无序的，这意味着字典中的元素没有特定的顺序。

### 1.2 创建字典

你可以使用花括号 `{}` 来创建一个字典，或者使用 `dict()` 函数。

```python
# 使用花括号创建字典
student = {
    "name": "Alice",
    "age": 20,
    "major": "Computer Science"
}

# 使用 dict() 函数创建字典
student = dict(name="Alice", age=20, major="Computer Science")
```

### 1.3 访问字典中的值

你可以通过键来访问字典中的值。

```python
print(student["name"])  # 输出: Alice
print(student["age"])   # 输出: 20
```

### 1.4 修改字典

你可以通过键来修改字典中的值，或者添加新的键值对。

```python
student["age"] = 21  # 修改年龄
student["year"] = 3  # 添加新的键值对
```

### 1.5 删除键值对

你可以使用 `del` 关键字来删除字典中的键值对。

```python
del student["year"]
```

### 1.6 字典方法

Python提供了许多有用的字典方法，例如 `keys()`、`values()`、`items()` 等。

```python
print(student.keys())   # 输出: dict_keys(['name', 'age', 'major'])
print(student.values()) # 输出: dict_values(['Alice', 21, 'Computer Science'])
print(student.items())  # 输出: dict_items([('name', 'Alice'), ('age', 21), ('major', 'Computer Science')])
```

## 2. 集合 (Set)

### 2.1 什么是集合？

集合是Python中的一种数据结构，用于存储无序且不重复的元素。集合中的元素必须是不可变的（例如数字、字符串、元组），但集合本身是可变的。

### 2.2 创建集合

你可以使用花括号 `{}` 来创建一个集合，或者使用 `set()` 函数。

```python
# 使用花括号创建集合
fruits = {"apple", "banana", "cherry"}

# 使用 set() 函数创建集合
fruits = set(["apple", "banana", "cherry"])
```

### 2.3 添加和删除元素

你可以使用 `add()` 方法来添加元素，使用 `remove()` 方法来删除元素。

```python
fruits.add("orange")
fruits.remove("banana")
```

### 2.4 集合运算

集合支持常见的集合运算，如并集、交集、差集等。

```python
set1 = {1, 2, 3, 4}
set2 = {3, 4, 5, 6}

# 并集
print(set1 | set2)  # 输出: {1, 2, 3, 4, 5, 6}

# 交集
print(set1 & set2)  # 输出: {3, 4}

# 差集
print(set1 - set2)  # 输出: {1, 2}
```

## 3. 实践练习

### 3.1 练习1：字典操作

创建一个字典来存储学生的成绩，键为科目，值为分数。然后，计算并输出学生的平均成绩。

```python
grades = {
    "math": 85,
    "science": 90,
    "history": 78
}

# 计算平均成绩
average_grade = sum(grades.values()) / len(grades)
print(f"平均成绩: {average_grade}")
```

### 3.2 练习2：集合操作

创建两个集合，分别存储两个班级的学生名单。然后，找出两个班级的共同学生，并输出结果。

```python
class1 = {"Alice", "Bob", "Charlie"}
class2 = {"Charlie", "David", "Eve"}

# 找出共同学生
common_students = class1 & class2
print(f"共同学生: {common_students}")
```

## 4. 总结

字典和集合是Python中非常强大的数据结构，它们分别用于存储键值对和无序不重复的元素。通过本教程，你应该已经掌握了如何创建、访问、修改和操作字典和集合。继续练习和探索，你将能够更熟练地使用这些数据结构来解决实际问题。