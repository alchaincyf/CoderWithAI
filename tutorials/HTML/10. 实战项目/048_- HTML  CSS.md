---
title: 创建简单的登录表单 - HTML & CSS 教程
date: 2023-10-05
description: 本课程将教你如何使用HTML和CSS创建一个简单的登录表单，适合初学者学习网页开发的基础知识。
slug: simple-login-form
tags:
  - HTML
  - CSS
  - 表单设计
category: 网页开发
keywords:
  - 登录表单
  - HTML表单
  - CSS样式
---

# 简单的登录表单

在本教程中，我们将学习如何使用HTML和CSS创建一个简单的登录表单。这个表单将包括用户名和密码的输入框，以及一个提交按钮。通过这个练习，你将掌握HTML表单的基本结构和CSS的基本样式设置。

## 1. HTML 表单基础

HTML表单是用户与网页交互的重要方式。表单通常用于收集用户输入的数据，如登录信息、注册信息等。表单的核心元素是`<form>`标签，它包含了各种输入元素，如文本框、密码框、按钮等。

### 1.1 表单的基本结构

一个基本的表单结构如下：

```html
<form action="/submit-form" method="post">
    <!-- 表单元素将放在这里 -->
</form>
```

- `action`属性指定了表单提交后数据将被发送到哪个URL。
- `method`属性指定了数据传输的方式，常用的有`post`和`get`。

### 1.2 输入元素

表单中常用的输入元素包括：

- `<input type="text">`：用于输入文本。
- `<input type="password">`：用于输入密码，输入的内容会被隐藏。
- `<input type="submit">`：用于提交表单。

## 2. 创建登录表单

现在让我们创建一个简单的登录表单。这个表单将包括用户名和密码的输入框，以及一个提交按钮。

### 2.1 HTML 代码

```html
<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>简单的登录表单</title>
    <style>
        /* 这里可以添加CSS样式 */
    </style>
</head>
<body>
    <h2>登录</h2>
    <form action="/login" method="post">
        <label for="username">用户名:</label>
        <input type="text" id="username" name="username" required><br><br>
        
        <label for="password">密码:</label>
        <input type="password" id="password" name="password" required><br><br>
        
        <input type="submit" value="登录">
    </form>
</body>
</html>
```

### 2.2 解释代码

- `<label>`标签用于为输入元素提供描述性文本。`for`属性应与输入元素的`id`属性匹配，以确保标签和输入元素关联。
- `<input type="text">`和`<input type="password">`分别用于输入用户名和密码。
- `required`属性确保用户必须填写这些字段。
- `<input type="submit">`创建一个提交按钮，用户点击后表单数据将被发送到服务器。

## 3. 添加基本样式

为了让表单看起来更美观，我们可以添加一些基本的CSS样式。

### 3.1 CSS 代码

```html
<style>
    body {
        font-family: Arial, sans-serif;
        background-color: #f4f4f4;
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100vh;
        margin: 0;
    }

    form {
        background-color: #fff;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
        width: 300px;
    }

    label {
        display: block;
        margin-bottom: 8px;
    }

    input[type="text"], input[type="password"] {
        width: 100%;
        padding: 10px;
        margin-bottom: 15px;
        border: 1px solid #ccc;
        border-radius: 4px;
    }

    input[type="submit"] {
        width: 100%;
        padding: 10px;
        background-color: #28a745;
        border: none;
        border-radius: 4px;
        color: white;
        font-size: 16px;
        cursor: pointer;
    }

    input[type="submit"]:hover {
        background-color: #218838;
    }
</style>
```

### 3.2 解释样式

- `body`样式设置了页面的背景颜色、字体和居中对齐。
- `form`样式设置了表单的背景颜色、内边距、圆角和阴影效果。
- `label`样式使标签显示为块级元素，并添加了底部间距。
- `input[type="text"]`和`input[type="password"]`样式设置了输入框的宽度、内边距、边框和圆角。
- `input[type="submit"]`样式设置了提交按钮的宽度、内边距、背景颜色、字体颜色和鼠标悬停效果。

## 4. 实践练习

现在你已经掌握了如何创建一个简单的登录表单，并为其添加基本样式。尝试以下练习：

1. **添加注册链接**：在表单下方添加一个“注册”链接，点击后跳转到注册页面。
2. **添加忘记密码链接**：在表单下方添加一个“忘记密码”链接，点击后跳转到密码重置页面。
3. **美化表单**：尝试使用不同的颜色和字体，使表单看起来更独特。

## 5. 总结

通过本教程，你学会了如何使用HTML和CSS创建一个简单的登录表单。你掌握了HTML表单的基本结构和常用输入元素，并学会了如何使用CSS为表单添加样式。希望你能继续探索更多HTML和CSS的功能，创建出更复杂的网页。

## 6. 进一步学习

- **HTML5新特性**：了解HTML5中新增的表单元素和属性，如`<input type="email">`、`<input type="date">`等。
- **CSS框架**：尝试使用Bootstrap或Tailwind CSS等CSS框架，快速构建美观的表单。
- **JavaScript集成**：学习如何使用JavaScript为表单添加交互功能，如表单验证、动态内容加载等。

希望你喜欢这个教程，并能在实践中不断提升你的编程技能！