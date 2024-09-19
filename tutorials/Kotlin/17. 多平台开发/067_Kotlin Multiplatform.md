---
title: Kotlin Multiplatform 项目设置指南
date: 2023-10-05
description: 本课程详细介绍如何设置和配置Kotlin Multiplatform项目，涵盖从基础到高级的配置步骤。
slug: kotlin-multiplatform-project-setup
tags:
  - Kotlin
  - Multiplatform
  - 项目设置
category: 编程教程
keywords:
  - Kotlin Multiplatform
  - 项目配置
  - 跨平台开发
---

# Kotlin Multiplatform 项目设置

## 概述

Kotlin Multiplatform 是一种强大的技术，允许开发者编写一次代码并在多个平台上运行，包括 Android、iOS、Web 和桌面应用。本教程将详细介绍如何设置一个 Kotlin Multiplatform 项目，并编写共享代码和平台特定代码。

## 1. 开发环境搭建

### 1.1 安装 IntelliJ IDEA

首先，确保你已经安装了 IntelliJ IDEA。Kotlin Multiplatform 项目最好在 IntelliJ IDEA 中进行开发，因为它提供了强大的 Kotlin 支持。

### 1.2 安装 Kotlin 插件

在 IntelliJ IDEA 中，确保你已经安装了 Kotlin 插件。你可以通过以下步骤检查和安装插件：

1. 打开 IntelliJ IDEA。
2. 进入 `File` -> `Settings` -> `Plugins`。
3. 搜索 `Kotlin` 插件并安装。

## 2. 创建 Kotlin Multiplatform 项目

### 2.1 新建项目

1. 打开 IntelliJ IDEA，点击 `File` -> `New` -> `Project`。
2. 在弹出的窗口中，选择 `Kotlin` -> `Kotlin Multiplatform`。
3. 输入项目名称和位置，然后点击 `Next`。
4. 选择项目模板（例如 `Kotlin/Multiplatform`），然后点击 `Finish`。

### 2.2 项目结构

创建项目后，你将看到以下目录结构：

```
my-multiplatform-project/
├── commonMain/
│   └── kotlin/
│       └── com/
│           └── example/
│               └── MyClass.kt
├── androidMain/
│   └── kotlin/
│       └── com/
│           └── example/
│               └── AndroidSpecific.kt
├── iosMain/
│   └── kotlin/
│       └── com/
│           └── example/
│               └── IosSpecific.kt
├── build.gradle.kts
└── settings.gradle.kts
```

- `commonMain`：存放共享代码。
- `androidMain`：存放 Android 特定代码。
- `iosMain`：存放 iOS 特定代码。

## 3. 编写共享代码

### 3.1 创建共享类

在 `commonMain/kotlin/com/example/` 目录下创建一个名为 `MyClass.kt` 的文件，并编写以下代码：

```kotlin
package com.example

class MyClass {
    fun sayHello(): String {
        return "Hello from shared code!"
    }
}
```

### 3.2 使用共享类

你可以在 `androidMain` 和 `iosMain` 中使用这个共享类。例如，在 `androidMain/kotlin/com/example/` 目录下创建一个名为 `AndroidSpecific.kt` 的文件：

```kotlin
package com.example

fun androidSpecificFunction() {
    val myClass = MyClass()
    println(myClass.sayHello())
}
```

## 4. 编写平台特定代码

### 4.1 Android 特定代码

在 `androidMain/kotlin/com/example/` 目录下创建一个名为 `AndroidSpecific.kt` 的文件，并编写以下代码：

```kotlin
package com.example

fun androidSpecificFunction() {
    println("This is Android specific code!")
}
```

### 4.2 iOS 特定代码

在 `iosMain/kotlin/com/example/` 目录下创建一个名为 `IosSpecific.kt` 的文件，并编写以下代码：

```kotlin
package com.example

fun iosSpecificFunction() {
    println("This is iOS specific code!")
}
```

## 5. 配置 Gradle

### 5.1 修改 `build.gradle.kts`

在 `build.gradle.kts` 文件中，确保你已经配置了 Kotlin Multiplatform 插件和相应的目标平台：

```kotlin
plugins {
    kotlin("multiplatform") version "1.6.21"
}

kotlin {
    jvm()
    ios()

    sourceSets {
        val commonMain by getting {
            dependencies {
                implementation(kotlin("stdlib-common"))
            }
        }
        val androidMain by getting {
            dependencies {
                implementation(kotlin("stdlib"))
            }
        }
        val iosMain by getting {
            dependencies {
                implementation(kotlin("stdlib"))
            }
        }
    }
}
```

### 5.2 同步项目

在 IntelliJ IDEA 中，点击 `File` -> `Sync Project with Gradle Files` 以确保项目配置正确。

## 6. 运行项目

### 6.1 运行 Android 代码

1. 打开 `AndroidSpecific.kt` 文件。
2. 右键点击 `androidSpecificFunction` 函数，选择 `Run 'androidSpecificFunction'`。

### 6.2 运行 iOS 代码

1. 打开 `IosSpecific.kt` 文件。
2. 右键点击 `iosSpecificFunction` 函数，选择 `Run 'iosSpecificFunction'`。

## 7. 实践练习

### 7.1 练习目标

编写一个 Kotlin Multiplatform 项目，包含以下内容：

1. 一个共享类 `SharedClass`，包含一个函数 `sharedFunction`。
2. Android 特定代码，调用 `sharedFunction` 并输出特定信息。
3. iOS 特定代码，调用 `sharedFunction` 并输出特定信息。

### 7.2 提示

- 使用 `commonMain` 目录编写共享代码。
- 使用 `androidMain` 和 `iosMain` 目录编写平台特定代码。
- 确保在 `build.gradle.kts` 中正确配置了 Kotlin Multiplatform 插件。

## 8. 总结

通过本教程，你已经学会了如何设置一个 Kotlin Multiplatform 项目，并编写共享代码和平台特定代码。Kotlin Multiplatform 提供了一种高效的方式来编写跨平台应用，减少了重复代码，提高了开发效率。继续探索 Kotlin Multiplatform 的更多功能，你将能够开发出更加复杂和强大的跨平台应用。