---
title: Python
slug: python
aka: ["Python", "python3", "pip", "虚拟环境", "venv", "py"]
summary: 一门好读好写的编程语言，擅长数据处理、自动化和AI相关工作。
category: 基础
level: 2
related: [script, terminal, command, nodejs, cli]
updated: 2026-11-18
---

## 一句话
Python 是一门编程语言，语法接近英语，常被用来写数据处理、批量自动化、爬取整理、AI 相关的程序。

## 打个比方
如果 JavaScript 是网页世界的通用语，Python 就是办公室里的万能助理：整理表格、批量改文件名、处理数据，它特别拿手。

不贴切的地方：它们的分工不是绝对的，Python 也能做网站，JavaScript 也能做自动化。

## 你会在哪儿碰到它
- 让 AI 帮你批量处理文件、分析表格、做个小工具，它常会写 .py 文件。
- 运行方式：在终端输入 python3 文件名.py。Mac 上用 python3，不要只写 python。
- pip 是 Python 的包管理工具，作用类似 [npm](/words/npm)，命令是 pip install 某某包。
- 很多 AI 项目是 Python 写的。碰到它们的安装说明里有 requirements.txt，就是依赖清单，见[依赖](/words/dependencies)。
- 为避免不同项目的包互相打架，常会建一个「虚拟环境」，AI 会让你运行 python3 -m venv 之类的命令。

## AI提到它时，你该怎么接
```prompt
请用 Python 写一个脚本完成：（描述，例如把一个文件夹里所有图片按日期重命名）。要求：先不要真的改文件，加一个「预览模式」只打印将要做的事。告诉我怎么运行，需要装哪些包。
```

```prompt
运行 Python 脚本时报错（粘贴）。请判断是 Python 版本、缺少包还是代码问题，并告诉我怎么修。我的系统是（填写）。
```

## 容易踩的坑
- python 和 python3 指向不同版本，Mac 上尤其容易混。让 AI 查当前用的是哪个。
- pip 装的包装到了别的 Python 上，运行时却提示找不到。这是环境混乱，让 AI 带你排查，别盲目重装。
- 系统自带的 Python 不要乱动，用虚拟环境隔离。
- 缩进错误。Python 靠缩进表示结构，一个空格的差别也会报错，AI 复制粘贴时尤其注意。
