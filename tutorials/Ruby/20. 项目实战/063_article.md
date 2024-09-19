---
title: 命令行工具开发入门教程
date: 2023-10-05
description: 本课程将带你从零开始学习如何开发高效的命令行工具，涵盖基础概念、常用工具和实战案例。
slug: command-line-tool-development
tags:
  - 命令行
  - 工具开发
  - 编程教程
category: 编程与开发
keywords:
  - 命令行工具
  - 工具开发
  - 命令行编程
---

# 命令行工具开发

## 1. 简介

命令行工具是计算机程序的一种，通常在终端或命令提示符中运行。它们通常用于自动化任务、数据处理、系统管理等。Ruby 是一种非常适合编写命令行工具的语言，因为它具有简洁的语法和强大的标准库。

## 2. 基本概念

### 2.1 命令行参数

命令行工具通常需要接收用户输入的参数。Ruby 提供了 `ARGV` 数组来获取这些参数。`ARGV` 是一个包含所有命令行参数的数组。

```ruby
# 示例：打印所有命令行参数
puts "命令行参数: #{ARGV.join(', ')}"
```

### 2.2 选项解析

命令行工具通常还需要处理选项（flags），例如 `--help` 或 `-v`。Ruby 提供了 `OptionParser` 类来帮助解析这些选项。

```ruby
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: example.rb [options]"

  opts.on("-v", "--verbose", "Run verbosely") do |v|
    options[:verbose] = v
  end

  opts.on("-h", "--help", "Prints this help") do
    puts opts
    exit
  end
end.parse!

puts "Verbose mode: #{options[:verbose]}" if options[:verbose]
```

## 3. 实践练习

### 3.1 创建一个简单的文件列表工具

我们将创建一个命令行工具，用于列出指定目录中的文件。

```ruby
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: list_files.rb [options] [directory]"

  opts.on("-a", "--all", "Include hidden files") do |a|
    options[:all] = a
  end

  opts.on("-h", "--help", "Prints this help") do
    puts opts
    exit
  end
end.parse!

directory = ARGV[0] || "."
Dir.entries(directory).each do |file|
  next if file.start_with?('.') && !options[:all]
  puts file
end
```

### 3.2 运行和测试

1. 将上述代码保存为 `list_files.rb`。
2. 在终端中运行：

```bash
ruby list_files.rb -a
```

这将列出当前目录中的所有文件，包括隐藏文件。

## 4. 高级功能

### 4.1 子命令

许多命令行工具支持子命令（如 `git commit` 或 `docker run`）。我们可以使用 `OptionParser` 来实现这一点。

```ruby
require 'optparse'

subcommands = {
  list: OptionParser.new do |opts|
    opts.banner = "Usage: example.rb list [options]"
    opts.on("-a", "--all", "List all items") do |a|
      options[:all] = a
    end
  end,
  add: OptionParser.new do |opts|
    opts.banner = "Usage: example.rb add [options]"
    opts.on("-f", "--force", "Force addition") do |f|
      options[:force] = f
    end
  end
}

options = {}
global = OptionParser.new do |opts|
  opts.banner = "Usage: example.rb [global options] command [command options]"
  opts.on("-v", "--verbose", "Run verbosely") do |v|
    options[:verbose] = v
  end
end

command = ARGV.shift
if subcommands.key?(command.to_sym)
  subcommands[command.to_sym].parse!(ARGV)
else
  puts global
  exit
end

puts "Command: #{command}"
puts "Options: #{options}"
```

### 4.2 实践练习

将上述代码保存为 `example.rb`，并在终端中运行：

```bash
ruby example.rb list -a
```

这将列出所有项目，并显示详细的选项信息。

## 5. 总结

通过本教程，你已经学会了如何使用 Ruby 编写简单的命令行工具，并了解了如何处理命令行参数和选项。命令行工具是自动化任务和提高工作效率的强大工具，掌握它们将使你在日常开发中更加高效。

## 6. 进一步学习

- 探索更多 `OptionParser` 的高级用法。
- 学习如何使用 `Thor` 或 `GLI` 等第三方库来简化命令行工具的开发。
- 尝试编写更复杂的命令行工具，如文件处理、数据转换等。

希望本教程能帮助你更好地理解和掌握命令行工具开发！