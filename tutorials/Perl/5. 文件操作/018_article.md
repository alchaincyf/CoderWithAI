---
title: 读写文本文件教程
date: 2023-10-05
description: 本课程将教你如何在Python中读取和写入文本文件，包括文件打开、读取、写入和关闭的基本操作。
slug: reading-writing-text-files
tags:
  - Python
  - 文件操作
  - 编程基础
category: 编程基础
keywords:
  - Python文件操作
  - 读取文本文件
  - 写入文本文件
---

# 读写文本文件

在编程中，处理文件是一项非常常见的任务。无论是读取配置文件、日志文件，还是生成报告，文本文件的读写都是必不可少的技能。本教程将详细介绍如何在 Perl 中读写文本文件，并提供相应的代码示例和实践练习。

## 1. 文件句柄

在 Perl 中，文件句柄（File Handle）是用于标识文件的变量。通过文件句柄，我们可以对文件进行读写操作。文件句柄通常使用大写字母命名，以区别于其他变量。

### 1.1 打开文件

要打开一个文件，可以使用 `open` 函数。`open` 函数的语法如下：

```perl
open(FILEHANDLE, MODE, FILENAME) or die "无法打开文件 $FILENAME: $!";
```

- `FILEHANDLE`：文件句柄的名称。
- `MODE`：文件的打开模式。常见的模式有：
  - `'<'`：读取模式（默认）。
  - `'>'`：写入模式，如果文件存在则覆盖，不存在则创建。
  - `'>>'`：追加模式，如果文件存在则在末尾追加，不存在则创建。
- `FILENAME`：文件的路径。

### 1.2 关闭文件

文件操作完成后，应该使用 `close` 函数关闭文件句柄，以释放系统资源。

```perl
close(FILEHANDLE);
```

## 2. 读取文本文件

### 2.1 逐行读取文件

最常见的读取文件的方式是逐行读取。可以使用 `while` 循环结合 `readline` 函数来实现。

```perl
open(FH, '<', 'example.txt') or die "无法打开文件 example.txt: $!";
while (my $line = <FH>) {
    chomp($line);  # 去掉行尾的换行符
    print "$line\n";
}
close(FH);
```

### 2.2 一次性读取整个文件

如果文件较小，可以一次性读取整个文件内容。

```perl
open(FH, '<', 'example.txt') or die "无法打开文件 example.txt: $!";
my $content = do { local $/; <FH> };  # 读取整个文件内容
close(FH);
print $content;
```

## 3. 写入文本文件

### 3.1 覆盖写入

使用 `>` 模式打开文件，可以覆盖写入内容。

```perl
open(FH, '>', 'output.txt') or die "无法打开文件 output.txt: $!";
print FH "这是第一行\n";
print FH "这是第二行\n";
close(FH);
```

### 3.2 追加写入

使用 `>>` 模式打开文件，可以在文件末尾追加内容。

```perl
open(FH, '>>', 'output.txt') or die "无法打开文件 output.txt: $!";
print FH "这是追加的第一行\n";
print FH "这是追加的第二行\n";
close(FH);
```

## 4. 实践练习

### 4.1 练习1：读取文件并统计行数

编写一个 Perl 脚本，读取一个文本文件并统计文件中的行数。

```perl
open(FH, '<', 'example.txt') or die "无法打开文件 example.txt: $!";
my $line_count = 0;
while (<FH>) {
    $line_count++;
}
close(FH);
print "文件 example.txt 共有 $line_count 行。\n";
```

### 4.2 练习2：复制文件

编写一个 Perl 脚本，将一个文件的内容复制到另一个文件中。

```perl
open(FH1, '<', 'source.txt') or die "无法打开文件 source.txt: $!";
open(FH2, '>', 'destination.txt') or die "无法打开文件 destination.txt: $!";
while (<FH1>) {
    print FH2 $_;
}
close(FH1);
close(FH2);
print "文件复制完成。\n";
```

## 5. 总结

通过本教程，我们学习了如何在 Perl 中读写文本文件。我们了解了文件句柄的概念，掌握了打开、读取、写入和关闭文件的方法。通过实践练习，我们进一步巩固了这些知识。

在实际编程中，文件操作是非常常见的任务。掌握这些技能将帮助你更好地处理各种文件相关的任务。继续练习和探索，你将能够更加熟练地使用 Perl 进行文件操作。