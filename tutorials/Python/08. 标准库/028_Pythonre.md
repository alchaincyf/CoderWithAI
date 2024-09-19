---
title: 掌握Python中的re模块：正则表达式详解
date: 2023-10-05
description: 本课程深入探讨Python中的re模块，教你如何使用正则表达式进行文本匹配和处理，提升你的编程技能。
slug: python-re-module-regex-tutorial
tags:
  - Python
  - 正则表达式
  - 文本处理
category: 编程教程
keywords:
  - Python re模块
  - 正则表达式教程
  - 文本匹配
---

# re 模块 (正则表达式) 教程

## 1. 概述

正则表达式（Regular Expression，简称 regex 或 regexp）是一种强大的工具，用于在文本中查找、匹配和操作字符串。Python 的 `re` 模块提供了对正则表达式的支持，使得处理复杂的字符串操作变得简单。

## 2. 基本概念

### 2.1 正则表达式的基本元素

- **字符类**: 匹配特定字符集，如 `[abc]` 匹配 `a`, `b`, 或 `c`。
- **量词**: 指定匹配次数，如 `*` 匹配 0 次或多次，`+` 匹配 1 次或多次，`?` 匹配 0 次或 1 次。
- **边界匹配**: 如 `^` 匹配字符串开头，`$` 匹配字符串结尾。
- **分组**: 使用 `()` 进行分组，如 `(abc)+` 匹配 `abc`, `abcabc` 等。

### 2.2 常用元字符

- `.` 匹配任意单个字符（除换行符外）。
- `\d` 匹配任意数字，相当于 `[0-9]`。
- `\w` 匹配任意字母、数字或下划线，相当于 `[a-zA-Z0-9_]`。
- `\s` 匹配任意空白字符，包括空格、制表符、换行符等。

## 3. re 模块的基本用法

### 3.1 导入 re 模块

```python
import re
```

### 3.2 常用函数

- `re.match()`: 从字符串开头匹配模式。
- `re.search()`: 在整个字符串中搜索匹配模式。
- `re.findall()`: 返回所有匹配的子串列表。
- `re.sub()`: 替换匹配的子串。

### 3.3 示例代码

```python
import re

# 示例字符串
text = "The quick brown fox jumps over the lazy dog."

# 使用 re.match()
match = re.match(r"The", text)
if match:
    print("Match found:", match.group())
else:
    print("No match")

# 使用 re.search()
search = re.search(r"fox", text)
if search:
    print("Search found:", search.group())
else:
    print("No search result")

# 使用 re.findall()
findall = re.findall(r"\b\w{5}\b", text)
print("Find all 5-letter words:", findall)

# 使用 re.sub()
sub = re.sub(r"fox", "cat", text)
print("After substitution:", sub)
```

## 4. 实践练习

### 4.1 练习1: 提取电子邮件地址

编写一个正则表达式，从给定的文本中提取所有的电子邮件地址。

```python
text = "Please contact us at support@example.com or info@example.org for more information."
emails = re.findall(r"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}", text)
print("Extracted emails:", emails)
```

### 4.2 练习2: 替换日期格式

编写一个正则表达式，将文本中的日期格式从 `MM/DD/YYYY` 替换为 `YYYY-MM-DD`。

```python
text = "The event is scheduled for 12/31/2023."
new_text = re.sub(r"(\d{2})/(\d{2})/(\d{4})", r"\3-\1-\2", text)
print("New date format:", new_text)
```

## 5. 高级用法

### 5.1 分组与捕获

使用 `()` 进行分组，并通过 `group()` 方法获取捕获的内容。

```python
text = "John Doe, 30 years old"
match = re.search(r"(\w+) (\w+), (\d+) years old", text)
if match:
    print("First name:", match.group(1))
    print("Last name:", match.group(2))
    print("Age:", match.group(3))
```

### 5.2 非捕获分组

使用 `(?:...)` 进行非捕获分组，不保存匹配结果。

```python
text = "The quick brown fox jumps over the lazy dog."
match = re.search(r"(?:quick|slow) (\w+)", text)
if match:
    print("Matched word:", match.group(1))
```

## 6. 总结

正则表达式是一个强大的工具，能够帮助你高效地处理字符串操作。通过 `re` 模块，你可以轻松地在 Python 中使用正则表达式。希望本教程能帮助你掌握正则表达式的基本用法，并激发你进一步探索其高级功能的兴趣。

## 7. 进一步学习资源

- [Python 官方文档 - re 模块](https://docs.python.org/3/library/re.html)
- [正则表达式教程 - 菜鸟教程](https://www.runoob.com/regexp/regexp-tutorial.html)
- [正则表达式测试工具](https://regex101.com/)

通过这些资源，你可以进一步巩固和扩展你的正则表达式知识。