---
title: 深入理解Python字典：从基础到高级
date: 2023-10-05
description: 本课程将带你从基础到高级，全面掌握Python字典的使用，包括创建、访问、更新和高级操作。
slug: python-dictionary-tutorial
tags:
  - Python
  - 数据结构
  - 字典
category: 编程基础
keywords:
  - Python字典
  - 字典操作
  - 数据结构
---

# 字典

## 1. 什么是字典？

在 Swift 中，字典（Dictionary）是一种无序的集合类型，用于存储键值对（key-value pairs）。每个键（key）都是唯一的，并且与一个值（value）相关联。字典非常适合用于需要通过唯一标识符（键）来查找对应值的场景。

### 1.1 字典的语法

字典的语法如下：

```swift
var dictionaryName: [KeyType: ValueType] = [key1: value1, key2: value2, ...]
```

- `KeyType` 是键的类型。
- `ValueType` 是值的类型。
- `key1`, `key2`, ... 是字典中的键。
- `value1`, `value2`, ... 是与键对应的值。

### 1.2 字典的创建

你可以通过以下几种方式创建字典：

#### 1.2.1 空字典

```swift
var emptyDictionary: [String: Int] = [:]
```

#### 1.2.2 带有初始值的字典

```swift
var person: [String: String] = ["name": "Alice", "age": "30", "city": "New York"]
```

### 1.3 字典的访问与修改

#### 1.3.1 访问字典中的值

你可以通过键来访问字典中的值：

```swift
let name = person["name"]
print(name)  // 输出: Optional("Alice")
```

注意：访问字典中的值会返回一个可选类型（Optional），因为键可能不存在。

#### 1.3.2 修改字典中的值

你可以通过键来修改字典中的值：

```swift
person["age"] = "31"
print(person)  // 输出: ["name": "Alice", "age": "31", "city": "New York"]
```

#### 1.3.3 添加新的键值对

你可以通过赋值来添加新的键值对：

```swift
person["occupation"] = "Engineer"
print(person)  // 输出: ["name": "Alice", "age": "31", "city": "New York", "occupation": "Engineer"]
```

#### 1.3.4 删除键值对

你可以通过将键对应的值设为 `nil` 来删除键值对：

```swift
person["city"] = nil
print(person)  // 输出: ["name": "Alice", "age": "31", "occupation": "Engineer"]
```

### 1.4 字典的遍历

你可以使用 `for-in` 循环来遍历字典中的所有键值对：

```swift
for (key, value) in person {
    print("\(key): \(value)")
}
```

输出：

```
name: Alice
age: 31
occupation: Engineer
```

### 1.5 字典的常用属性

- `count`：返回字典中键值对的数量。
- `isEmpty`：判断字典是否为空。

```swift
print(person.count)  // 输出: 3
print(person.isEmpty)  // 输出: false
```

## 2. 实践练习

### 2.1 练习1：创建并操作字典

创建一个字典来存储学生的成绩，键是学生的名字，值是他们的分数。然后进行以下操作：

1. 添加一个新的学生及其分数。
2. 修改某个学生的分数。
3. 删除某个学生的记录。
4. 遍历字典并打印所有学生的名字和分数。

```swift
var scores: [String: Int] = ["Alice": 90, "Bob": 85, "Charlie": 88]

// 添加新的学生
scores["David"] = 92

// 修改分数
scores["Bob"] = 87

// 删除学生
scores["Charlie"] = nil

// 遍历字典
for (name, score) in scores {
    print("\(name): \(score)")
}
```

### 2.2 练习2：统计单词频率

编写一个程序，统计一段文本中每个单词出现的频率，并将结果存储在一个字典中。

```swift
let text = "Swift is a powerful and intuitive programming language for iOS, macOS, watchOS, and tvOS."

var wordFrequency: [String: Int] = [:]

// 将文本分割成单词
let words = text.split(separator: " ")

// 统计单词频率
for word in words {
    let lowercasedWord = word.lowercased()
    if let count = wordFrequency[lowercasedWord] {
        wordFrequency[lowercasedWord] = count + 1
    } else {
        wordFrequency[lowercasedWord] = 1
    }
}

// 打印结果
for (word, count) in wordFrequency {
    print("\(word): \(count)")
}
```

## 3. 总结

字典是 Swift 中非常有用的数据结构，适用于需要通过唯一标识符来查找对应值的场景。通过本教程，你应该已经掌握了字典的基本操作，包括创建、访问、修改、删除和遍历字典。希望你能通过实践练习进一步巩固这些知识。