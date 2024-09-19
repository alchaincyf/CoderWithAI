---
title: 环境搭建：JDK、Scala 与 IDE 配置指南
date: 2023-10-05
description: 本课程详细讲解如何搭建Java开发环境，包括JDK的安装与配置、Scala环境的设置，以及集成开发环境（IDE）的选择与使用。
slug: environment-setup-jdk-scala-ide
tags:
  - 环境搭建
  - JDK
  - Scala
  - IDE
category: 编程基础
keywords:
  - JDK安装
  - Scala配置
  - IDE选择
  - 开发环境搭建
---

# 环境搭建 (JDK, Scala, IDE)

在开始学习Scala编程之前，首先需要搭建一个适合的开发环境。本教程将详细介绍如何安装和配置JDK（Java Development Kit）、Scala编译器以及一个集成开发环境（IDE），如IntelliJ IDEA。

## 1. 安装JDK

### 1.1 下载JDK

JDK是Java开发的基础工具包，Scala依赖于Java虚拟机（JVM）运行，因此首先需要安装JDK。

1. 访问[Oracle JDK下载页面](https://www.oracle.com/java/technologies/javase-downloads.html)或[OpenJDK下载页面](https://openjdk.java.net/install/)。
2. 根据你的操作系统选择合适的JDK版本进行下载。

### 1.2 安装JDK

- **Windows**: 运行下载的安装程序，按照提示完成安装。
- **macOS**: 双击下载的DMG文件，拖动JDK图标到“Applications”文件夹。
- **Linux**: 解压下载的压缩包，并将解压后的目录移动到合适的位置。

### 1.3 配置环境变量

为了在命令行中能够使用`java`和`javac`命令，需要配置环境变量。

- **Windows**:
  1. 右键点击“此电脑”，选择“属性”。
  2. 点击“高级系统设置”，然后点击“环境变量”。
  3. 在“系统变量”中找到`Path`，点击“编辑”。
  4. 添加JDK的`bin`目录路径，例如`C:\Program Files\Java\jdk-11.0.11\bin`。

- **macOS/Linux**:
  1. 打开终端。
  2. 编辑`~/.bashrc`或`~/.zshrc`文件，添加以下内容：
     ```bash
     export JAVA_HOME=/path/to/jdk
     export PATH=$JAVA_HOME/bin:$PATH
     ```
  3. 保存文件并运行`source ~/.bashrc`或`source ~/.zshrc`使配置生效。

### 1.4 验证安装

打开命令行，输入以下命令验证JDK是否安装成功：

```bash
java -version
javac -version
```

如果显示版本信息，说明JDK安装成功。

## 2. 安装Scala

### 2.1 下载Scala

1. 访问[Scala官方下载页面](https://www.scala-lang.org/download/)。
2. 下载适合你操作系统的Scala二进制包。

### 2.2 安装Scala

- **Windows**:
  1. 解压下载的压缩包到你选择的目录。
  2. 将Scala的`bin`目录路径添加到系统环境变量`Path`中。

- **macOS/Linux**:
  1. 解压下载的压缩包到你选择的目录。
  2. 编辑`~/.bashrc`或`~/.zshrc`文件，添加以下内容：
     ```bash
     export SCALA_HOME=/path/to/scala
     export PATH=$SCALA_HOME/bin:$PATH
     ```
  3. 保存文件并运行`source ~/.bashrc`或`source ~/.zshrc`使配置生效。

### 2.3 验证安装

打开命令行，输入以下命令验证Scala是否安装成功：

```bash
scala -version
```

如果显示版本信息，说明Scala安装成功。

## 3. 安装IntelliJ IDEA

### 3.1 下载IntelliJ IDEA

1. 访问[IntelliJ IDEA官方下载页面](https://www.jetbrains.com/idea/download/)。
2. 下载适合你操作系统的社区版（Community Edition）或专业版（Ultimate Edition）。

### 3.2 安装IntelliJ IDEA

- **Windows**:
  1. 运行下载的安装程序，按照提示完成安装。

- **macOS**:
  1. 双击下载的DMG文件，拖动IntelliJ IDEA图标到“Applications”文件夹。

- **Linux**:
  1. 解压下载的压缩包，并将解压后的目录移动到合适的位置。

### 3.3 配置Scala插件

1. 打开IntelliJ IDEA。
2. 进入`File` -> `Settings`（或`Preferences`） -> `Plugins`。
3. 搜索`Scala`插件并安装。
4. 重启IntelliJ IDEA使插件生效。

## 4. 创建第一个Scala项目

### 4.1 创建新项目

1. 打开IntelliJ IDEA。
2. 选择`File` -> `New` -> `Project`。
3. 选择`Scala` -> `IDEA`，点击`Next`。
4. 输入项目名称和位置，点击`Finish`。

### 4.2 编写第一个Scala程序

1. 在项目结构中，右键点击`src`目录，选择`New` -> `Scala Class`。
2. 输入类名，例如`HelloWorld`，选择`Object`类型。
3. 在生成的`HelloWorld.scala`文件中编写以下代码：

   ```scala
   object HelloWorld {
     def main(args: Array[String]): Unit = {
       println("Hello, Scala!")
     }
   }
   ```

### 4.3 运行程序

1. 右键点击`HelloWorld.scala`文件，选择`Run 'HelloWorld'`。
2. 在控制台中查看输出结果：`Hello, Scala!`。

## 5. 实践练习

### 5.1 练习1：修改程序

修改`HelloWorld`程序，使其输出你的名字。

### 5.2 练习2：创建新项目

创建一个新的Scala项目，编写一个程序，输出当前日期和时间。

### 5.3 练习3：使用Scala REPL

打开命令行，输入`scala`启动Scala REPL，尝试执行一些简单的Scala表达式，如`1 + 1`。

## 6. 总结

通过本教程，你已经成功搭建了Scala开发环境，并创建了第一个Scala程序。接下来，你可以继续学习Scala的其他特性，如变量和数据类型、控制结构、函数式编程等。

---

希望这篇教程对你有所帮助！如果你有任何问题或需要进一步的帮助，请随时提问。