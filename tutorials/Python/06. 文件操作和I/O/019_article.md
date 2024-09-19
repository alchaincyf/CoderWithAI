---
title: 读写文本文件教程
date: 2023-10-05
description: 本课程详细讲解如何在编程中读取和写入文本文件，涵盖Python、Java和C++等多种编程语言的实现方法。
slug: reading-writing-text-files
tags:
  - 文件操作
  - 文本处理
  - 编程基础
category: 编程教程
keywords:
  - 读取文本文件
  - 写入文本文件
  - 文件操作
---

# 读写文本文件

在编程中，处理文件是一个非常常见的任务。无论是读取配置文件、保存日志、还是处理用户数据，文本文件的读写操作都是必不可少的。Python 提供了简单而强大的工具来处理文本文件。本教程将详细介绍如何在 Python 中读写文本文件。

## 1. 打开文件

在 Python 中，使用内置的 `open()` 函数来打开文件。`open()` 函数的基本语法如下：

```python
file = open('filename', 'mode')
```

- `filename` 是文件的路径和名称。
- `mode` 是文件的打开模式，常用的模式有：
  - `'r'`：只读模式（默认）。
  - `'w'`：写入模式，如果文件存在则覆盖，不存在则创建。
  - `'a'`：追加模式，如果文件存在则在文件末尾追加内容，不存在则创建。
  - `'b'`：二进制模式，通常与其他模式结合使用，如 `'rb'` 或 `'wb'`。

### 示例：打开一个文件

```python
# 打开一个文件以只读模式
file = open('example.txt', 'r')
```

## 2. 读取文件内容

打开文件后，可以使用多种方法读取文件内容。

### 2.1 读取整个文件内容

使用 `read()` 方法可以一次性读取整个文件的内容。

```python
# 读取整个文件内容
content = file.read()
print(content)
```

### 2.2 逐行读取文件内容

使用 `readlines()` 方法可以逐行读取文件内容，并返回一个包含所有行的列表。

```python
# 逐行读取文件内容
lines = file.readlines()
for line in lines:
    print(line)
```

### 2.3 使用 `with` 语句自动关闭文件

为了避免忘记关闭文件，可以使用 `with` 语句。`with` 语句会在代码块执行完毕后自动关闭文件。

```python
# 使用 with 语句读取文件
with open('example.txt', 'r') as file:
    content = file.read()
    print(content)
```

## 3. 写入文件内容

写入文件内容与读取类似，但需要使用写入模式 `'w'` 或追加模式 `'a'`。

### 3.1 写入模式

使用 `write()` 方法可以将字符串写入文件。

```python
# 写入文件内容
with open('example.txt', 'w') as file:
    file.write('Hello, World!\n')
    file.write('This is a new line.')
```

### 3.2 追加模式

使用追加模式 `'a'` 可以在文件末尾追加内容。

```python
# 追加文件内容
with open('example.txt', 'a') as file:
    file.write('\nThis line is appended.')
```

## 4. 实践练习

### 练习 1：读取文件并统计行数

编写一个程序，读取一个文本文件并统计文件中的行数。

```python
# 读取文件并统计行数
with open('example.txt', 'r') as file:
    lines = file.readlines()
    line_count = len(lines)
    print(f'文件共有 {line_count} 行。')
```

### 练习 2：写入用户输入到文件

编写一个程序，提示用户输入内容，并将输入内容写入到一个新的文件中。

```python
# 写入用户输入到文件
user_input = input('请输入内容：')
with open('user_input.txt', 'w') as file:
    file.write(user_input)
```

## 5. 总结

通过本教程，你学会了如何在 Python 中读写文本文件。掌握了文件的打开、读取和写入操作，并了解了如何使用 `with` 语句来自动管理文件的关闭。这些技能在日常编程中非常有用，希望你能通过实践练习进一步巩固这些知识。

## 6. 下一步

接下来，你可以学习如何处理 CSV 和 JSON 数据，或者深入了解文件系统操作。这些内容将帮助你更全面地掌握文件处理的相关知识。

---

希望这篇教程对你有所帮助！如果你有任何问题或需要进一步的解释，请随时提问。