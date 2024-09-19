---
title: 共享代码编写指南
date: 2023-10-05
description: 本课程详细讲解如何在团队项目中高效编写和共享代码，涵盖版本控制、代码审查和协作工具的使用。
slug: shared-code-writing
tags:
  - 代码编写
  - 团队协作
  - 版本控制
category: 编程技巧
keywords:
  - 共享代码
  - 代码协作
  - Git
---

# 共享代码编写

在现代软件开发中，跨平台开发变得越来越重要。Kotlin Multiplatform 允许开发者编写一次代码，然后在多个平台上使用，如 Android、iOS、Web 和桌面应用。本教程将详细介绍如何在 Kotlin Multiplatform 项目中编写共享代码，并处理平台特定的代码。

## 1. Kotlin Multiplatform 项目设置

### 1.1 创建 Kotlin Multiplatform 项目

首先，我们需要创建一个 Kotlin Multiplatform 项目。你可以使用 IntelliJ IDEA 或 Android Studio 来创建项目。

1. 打开 IntelliJ IDEA 或 Android Studio。
2. 选择 "New Project"。
3. 选择 "Kotlin Multiplatform" 模板。
4. 配置项目名称、包名和项目位置。
5. 点击 "Finish" 完成项目创建。

### 1.2 项目结构

一个典型的 Kotlin Multiplatform 项目结构如下：

```
myapp/
├── build.gradle.kts
├── settings.gradle.kts
├── commonMain/
│   └── kotlin/
│       └── com/
│           └── myapp/
│               └── SharedCode.kt
├── androidMain/
│   └── kotlin/
│       └── com/
│           └── myapp/
│               └── AndroidSpecificCode.kt
├── iosMain/
│   └── kotlin/
│       └── com/
│           └── myapp/
│               └── IosSpecificCode.kt
└── jsMain/
    └── kotlin/
        └── com/
            └── myapp/
                └── JsSpecificCode.kt
```

- `commonMain`：包含所有平台共享的代码。
- `androidMain`、`iosMain`、`jsMain`：分别包含 Android、iOS 和 JavaScript 平台特定的代码。

## 2. 编写共享代码

### 2.1 定义共享代码

在 `commonMain` 目录下，我们可以编写所有平台共享的代码。例如，定义一个简单的共享函数：

```kotlin
// commonMain/kotlin/com/myapp/SharedCode.kt

package com.myapp

fun sharedFunction(): String {
    return "This is shared code!"
}
```

### 2.2 使用平台特定的代码

有时，我们需要在共享代码中使用平台特定的功能。Kotlin 提供了 `expect` 和 `actual` 关键字来处理这种情况。

#### 2.2.1 定义 `expect` 声明

在 `commonMain` 中，使用 `expect` 关键字定义一个平台特定的函数：

```kotlin
// commonMain/kotlin/com/myapp/SharedCode.kt

package com.myapp

expect fun platformSpecificFunction(): String
```

#### 2.2.2 实现 `actual` 声明

在每个平台特定的模块中，使用 `actual` 关键字实现这个函数：

```kotlin
// androidMain/kotlin/com/myapp/AndroidSpecificCode.kt

package com.myapp

actual fun platformSpecificFunction(): String {
    return "This is Android specific code!"
}
```

```kotlin
// iosMain/kotlin/com/myapp/IosSpecificCode.kt

package com.myapp

actual fun platformSpecificFunction(): String {
    return "This is iOS specific code!"
}
```

```kotlin
// jsMain/kotlin/com/myapp/JsSpecificCode.kt

package com.myapp

actual fun platformSpecificFunction(): String {
    return "This is JavaScript specific code!"
}
```

### 2.3 使用共享代码

现在，我们可以在任何平台特定的代码中使用共享代码：

```kotlin
// androidMain/kotlin/com/myapp/AndroidSpecificCode.kt

package com.myapp

fun androidFunction() {
    println(sharedFunction())
    println(platformSpecificFunction())
}
```

## 3. 实践练习

### 3.1 练习目标

编写一个简单的 Kotlin Multiplatform 项目，包含以下功能：

1. 在 `commonMain` 中定义一个共享函数 `greetUser(name: String)`，返回一个问候语。
2. 在 `commonMain` 中定义一个 `expect` 函数 `getCurrentTime()`，返回当前时间。
3. 在 `androidMain`、`iosMain` 和 `jsMain` 中分别实现 `getCurrentTime()` 函数，返回平台特定的当前时间。

### 3.2 练习步骤

1. 创建一个新的 Kotlin Multiplatform 项目。
2. 在 `commonMain` 中定义 `greetUser(name: String)` 函数。
3. 在 `commonMain` 中定义 `expect fun getCurrentTime(): String`。
4. 在 `androidMain`、`iosMain` 和 `jsMain` 中分别实现 `getCurrentTime()` 函数。
5. 在每个平台特定的代码中调用 `greetUser` 和 `getCurrentTime` 函数，并打印结果。

### 3.3 示例代码

```kotlin
// commonMain/kotlin/com/myapp/SharedCode.kt

package com.myapp

fun greetUser(name: String): String {
    return "Hello, $name!"
}

expect fun getCurrentTime(): String
```

```kotlin
// androidMain/kotlin/com/myapp/AndroidSpecificCode.kt

package com.myapp

actual fun getCurrentTime(): String {
    return "Android time: ${System.currentTimeMillis()}"
}

fun androidFunction() {
    println(greetUser("Android User"))
    println(getCurrentTime())
}
```

```kotlin
// iosMain/kotlin/com/myapp/IosSpecificCode.kt

package com.myapp

actual fun getCurrentTime(): String {
    return "iOS time: ${System.currentTimeMillis()}"
}

fun iosFunction() {
    println(greetUser("iOS User"))
    println(getCurrentTime())
}
```

```kotlin
// jsMain/kotlin/com/myapp/JsSpecificCode.kt

package com.myapp

actual fun getCurrentTime(): String {
    return "JavaScript time: ${System.currentTimeMillis()}"
}

fun jsFunction() {
    println(greetUser("JavaScript User"))
    println(getCurrentTime())
}
```

## 4. 总结

通过本教程，你学会了如何在 Kotlin Multiplatform 项目中编写共享代码，并处理平台特定的代码。Kotlin Multiplatform 提供了一种强大的方式来编写跨平台代码，减少了重复工作，提高了开发效率。希望你能继续深入学习 Kotlin Multiplatform，并在实际项目中应用这些知识。

## 5. 进一步学习

- 深入了解 Kotlin 协程在 Multiplatform 中的应用。
- 学习如何使用 Gradle 和 Kotlin DSL 管理 Multiplatform 项目。
- 探索 Kotlin 标准库和第三方库在 Multiplatform 中的使用。

希望本教程对你有所帮助，祝你在 Kotlin 编程的道路上越走越远！