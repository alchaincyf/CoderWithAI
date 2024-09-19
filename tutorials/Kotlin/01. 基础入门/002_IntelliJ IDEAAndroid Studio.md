---
title: 开发环境搭建：IntelliJ IDEA与Android Studio
date: 2023-10-05
description: 本课程详细讲解如何搭建Java和Android开发的理想环境，涵盖IntelliJ IDEA和Android Studio的安装与配置。
slug: setup-development-environment-intellij-android-studio
tags:
  - 开发环境
  - IntelliJ IDEA
  - Android Studio
category: 编程基础
keywords:
  - IntelliJ IDEA安装
  - Android Studio配置
  - Java开发环境
---

# 开发环境搭建 (IntelliJ IDEA, Android Studio)

## 概述

在开始编写Kotlin代码之前，我们需要搭建一个合适的开发环境。对于Kotlin开发，最常用的集成开发环境（IDE）是IntelliJ IDEA和Android Studio。本教程将详细介绍如何在这两个IDE中搭建Kotlin开发环境。

## 1. IntelliJ IDEA 环境搭建

### 1.1 下载与安装 IntelliJ IDEA

1. **下载 IntelliJ IDEA**:
   - 访问 [JetBrains 官网](https://www.jetbrains.com/idea/download/)。
   - 选择适合你操作系统的版本（Community 或 Ultimate）。
   - 下载并安装。

2. **安装 Kotlin 插件**:
   - 打开 IntelliJ IDEA。
   - 进入 `File` -> `Settings` -> `Plugins`。
   - 搜索 `Kotlin` 插件并安装。

### 1.2 创建 Kotlin 项目

1. **新建项目**:
   - 打开 IntelliJ IDEA。
   - 选择 `File` -> `New` -> `Project`。
   - 在左侧选择 `Kotlin`，然后选择 `JVM`。
   - 输入项目名称和位置，点击 `Finish`。

2. **编写第一个 Kotlin 程序**:
   - 在 `src` 目录下新建一个 Kotlin 文件（例如 `Main.kt`）。
   - 输入以下代码：
     ```kotlin
     fun main() {
         println("Hello, Kotlin!")
     }
     ```
   - 右键点击文件，选择 `Run 'MainKt'` 运行程序。

## 2. Android Studio 环境搭建

### 2.1 下载与安装 Android Studio

1. **下载 Android Studio**:
   - 访问 [Android Studio 官网](https://developer.android.com/studio)。
   - 下载并安装。

2. **安装 Kotlin 插件**:
   - 打开 Android Studio。
   - 进入 `File` -> `Settings` -> `Plugins`。
   - 搜索 `Kotlin` 插件并安装。

### 2.2 创建 Kotlin 项目

1. **新建项目**:
   - 打开 Android Studio。
   - 选择 `File` -> `New` -> `New Project`。
   - 选择 `Empty Activity` 模板。
   - 在 `Language` 选项中选择 `Kotlin`。
   - 输入项目名称和位置，点击 `Finish`。

2. **编写第一个 Kotlin 程序**:
   - 在 `MainActivity.kt` 文件中，找到 `onCreate` 方法。
   - 添加以下代码：
     ```kotlin
     override fun onCreate(savedInstanceState: Bundle?) {
         super.onCreate(savedInstanceState)
         setContentView(R.layout.activity_main)
         println("Hello, Kotlin in Android!")
     }
     ```
   - 点击 `Run` 按钮，选择模拟器或连接的设备运行应用。

## 3. 实践练习

### 3.1 在 IntelliJ IDEA 中练习

1. **创建一个简单的计算器**:
   - 创建一个新的 Kotlin 项目。
   - 编写一个函数，接受两个整数并返回它们的和。
   - 调用该函数并打印结果。

### 3.2 在 Android Studio 中练习

1. **创建一个简单的 Android 应用**:
   - 创建一个新的 Android 项目。
   - 在 `MainActivity` 中添加一个按钮，点击按钮时显示一个 Toast 消息。

## 4. 总结

通过本教程，你已经学会了如何在 IntelliJ IDEA 和 Android Studio 中搭建 Kotlin 开发环境，并编写了简单的 Kotlin 程序。接下来，你可以继续学习 Kotlin 的基本语法和特性，逐步深入 Kotlin 编程的世界。

---

希望这篇教程对你有所帮助！如果你有任何问题或需要进一步的帮助，请随时提问。