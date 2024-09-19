---
title: 文件读写基础教程
date: 2023-10-05
description: 本课程将详细介绍如何在编程中进行文件的读取和写入操作，涵盖多种编程语言和常见文件格式。
slug: file-io-basics
tags:
  - 文件操作
  - 输入输出
  - 编程基础
category: 编程基础
keywords:
  - 文件读写
  - 文件操作
  - 编程教程
---

# 文件读写

在编程中，文件读写是一项基本且重要的操作。它允许程序与外部文件进行交互，读取数据或保存数据。Ruby 提供了简单而强大的工具来处理文件操作。本教程将详细介绍如何在 Ruby 中进行文件读写操作。

## 1. 文件读取

### 1.1 打开文件

在 Ruby 中，可以使用 `File.open` 方法来打开一个文件。`File.open` 方法接受两个参数：文件路径和打开模式。

```ruby
file = File.open("example.txt", "r")
```

- `"r"` 表示以只读模式打开文件。

### 1.2 读取文件内容

打开文件后，可以使用 `read` 方法读取文件的全部内容。

```ruby
content = file.read
puts content
```

### 1.3 关闭文件

读取完文件内容后，应该关闭文件以释放资源。

```ruby
file.close
```

### 1.4 使用块简化操作

Ruby 允许在 `File.open` 方法中使用块，这样文件会在块结束时自动关闭。

```ruby
File.open("example.txt", "r") do |file|
  content = file.read
  puts content
end
```

### 1.5 逐行读取文件

如果文件较大，逐行读取文件内容会更高效。可以使用 `each_line` 方法。

```ruby
File.open("example.txt", "r") do |file|
  file.each_line do |line|
    puts line
  end
end
```

## 2. 文件写入

### 2.1 打开文件以写入

要写入文件，可以使用 `"w"` 模式打开文件。如果文件不存在，Ruby 会创建一个新文件；如果文件已存在，Ruby 会覆盖原有内容。

```ruby
File.open("output.txt", "w") do |file|
  file.write("Hello, World!")
end
```

### 2.2 追加写入

如果希望在文件末尾追加内容，可以使用 `"a"` 模式。

```ruby
File.open("output.txt", "a") do |file|
  file.write("\nThis is a new line.")
end
```

## 3. 实践练习

### 3.1 练习：读取并处理文件内容

编写一个 Ruby 脚本，读取 `example.txt` 文件的内容，并统计文件中每行字符的数量。

```ruby
File.open("example.txt", "r") do |file|
  file.each_line do |line|
    puts "Line length: #{line.length}"
  end
end
```

### 3.2 练习：写入用户输入到文件

编写一个 Ruby 脚本，提示用户输入内容，并将输入内容写入 `user_input.txt` 文件。

```ruby
puts "Please enter some text:"
user_input = gets.chomp

File.open("user_input.txt", "w") do |file|
  file.write(user_input)
end
```

## 4. 总结

通过本教程，你学习了如何在 Ruby 中进行文件读写操作。文件读写是编程中非常基础且常用的技能，掌握这些操作将帮助你更好地处理数据和文件。

## 5. 进一步学习

- 探索更多文件操作方法，如 `File.readlines`、`File.foreach` 等。
- 学习如何处理二进制文件。
- 了解如何在 Ruby 中进行目录操作。

希望本教程对你有所帮助，祝你在编程学习中取得更多进步！