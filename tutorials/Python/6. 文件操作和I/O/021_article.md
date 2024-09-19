---
title: 文件系统操作教程
date: 2023-10-05
description: 本课程详细讲解如何在编程中进行文件系统操作，包括文件的创建、读取、写入和删除等基本操作。
slug: file-system-operations-tutorial
tags:
  - 文件操作
  - 编程基础
  - 系统编程
category: 编程基础
keywords:
  - 文件系统
  - 文件操作
  - 编程教程
---

# 文件系统操作

在编程中，文件系统操作是非常重要的一部分。它允许我们读取、写入和管理文件，从而实现数据的持久化存储和检索。Python 提供了丰富的内置模块来处理文件系统操作，使得这些任务变得简单而直观。

## 1. 文件系统基础

### 1.1 文件和目录

在计算机中，文件是存储数据的容器，而目录（或文件夹）是组织文件的容器。文件系统操作主要包括：

- 创建、删除文件和目录
- 读取和写入文件内容
- 遍历目录结构
- 获取文件和目录的属性

### 1.2 Python 中的文件系统模块

Python 提供了多个模块来处理文件系统操作，其中最常用的是 `os` 和 `os.path` 模块。此外，`shutil` 模块提供了更高层次的文件操作功能。

## 2. 文件操作

### 2.1 打开和关闭文件

在 Python 中，使用 `open()` 函数来打开文件。`open()` 函数返回一个文件对象，通过该对象可以对文件进行读写操作。

```python
# 打开文件进行读取
file = open('example.txt', 'r')

# 读取文件内容
content = file.read()
print(content)

# 关闭文件
file.close()
```

### 2.2 文件模式

`open()` 函数的第二个参数是文件模式，用于指定文件的打开方式：

- `'r'`：只读模式（默认）
- `'w'`：写入模式，如果文件存在则覆盖，不存在则创建
- `'a'`：追加模式，如果文件存在则在末尾追加，不存在则创建
- `'b'`：二进制模式
- `'t'`：文本模式（默认）

例如，以写入模式打开文件：

```python
file = open('example.txt', 'w')
file.write('Hello, World!')
file.close()
```

### 2.3 使用 `with` 语句

为了避免忘记关闭文件，可以使用 `with` 语句。`with` 语句会在代码块执行完毕后自动关闭文件。

```python
with open('example.txt', 'r') as file:
    content = file.read()
    print(content)
```

## 3. 目录操作

### 3.1 创建目录

使用 `os.mkdir()` 函数创建目录：

```python
import os

os.mkdir('new_directory')
```

### 3.2 删除目录

使用 `os.rmdir()` 函数删除空目录：

```python
os.rmdir('new_directory')
```

### 3.3 遍历目录

使用 `os.listdir()` 函数列出目录中的所有文件和子目录：

```python
for item in os.listdir('.'):
    print(item)
```

### 3.4 获取文件属性

使用 `os.stat()` 函数获取文件的详细属性：

```python
stat_info = os.stat('example.txt')
print(f"Size: {stat_info.st_size} bytes")
print(f"Last modified: {stat_info.st_mtime}")
```

## 4. 实践练习

### 4.1 练习1：创建并写入文件

编写一个程序，创建一个名为 `output.txt` 的文件，并向其中写入一些文本内容。

```python
with open('output.txt', 'w') as file:
    file.write('This is a test file.\n')
    file.write('It contains some text.')
```

### 4.2 练习2：遍历目录并打印文件名

编写一个程序，遍历当前目录下的所有文件和子目录，并打印它们的名称。

```python
import os

for root, dirs, files in os.walk('.'):
    for file in files:
        print(os.path.join(root, file))
```

### 4.3 练习3：删除指定类型的文件

编写一个程序，删除当前目录下所有扩展名为 `.log` 的文件。

```python
import os

for file in os.listdir('.'):
    if file.endswith('.log'):
        os.remove(file)
```

## 5. 总结

通过本教程，我们学习了如何在 Python 中进行文件系统操作，包括文件的打开、读写、关闭，以及目录的创建、删除和遍历。这些操作是编程中非常基础且实用的技能，掌握它们将帮助你更好地管理和处理数据。

希望你能通过实践练习巩固所学知识，并在实际项目中灵活运用这些技能。继续探索 Python 的更多功能，你会发现编程的乐趣和无限可能！