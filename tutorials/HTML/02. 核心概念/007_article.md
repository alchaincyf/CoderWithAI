---
title: 表单和输入元素详解
date: 2023-10-05
description: 本课程详细讲解HTML表单和各种输入元素的使用方法，帮助你掌握创建交互式网页表单的技巧。
slug: forms-and-input-elements
tags:
  - HTML
  - 表单
  - 输入元素
category: 前端开发
keywords:
  - HTML表单
  - 输入元素
  - 表单验证
---

# 表单和输入元素

## 概述

表单是网页中用于收集用户输入的重要元素。无论是登录、注册、搜索还是反馈，表单都是实现这些功能的基础。HTML 提供了丰富的表单元素，使得开发者可以轻松创建各种交互式表单。

## 表单基础

### 1. `<form>` 标签

`<form>` 标签是表单的容器，所有的表单元素都应放在这个标签内。它有几个重要的属性：

- `action`: 指定表单提交后数据发送的 URL。
- `method`: 指定数据提交的方式，常用的有 `GET` 和 `POST`。

```html
<form action="/submit-form" method="POST">
  <!-- 表单元素将放在这里 -->
</form>
```

### 2. 输入元素 `<input>`

`<input>` 是最常用的表单元素，用于接收用户的输入。它有多种类型，如文本、密码、单选按钮、复选框等。

#### 文本输入

```html
<input type="text" name="username" placeholder="请输入用户名">
```

#### 密码输入

```html
<input type="password" name="password" placeholder="请输入密码">
```

#### 单选按钮

```html
<input type="radio" name="gender" value="male"> 男
<input type="radio" name="gender" value="female"> 女
```

#### 复选框

```html
<input type="checkbox" name="hobby" value="reading"> 阅读
<input type="checkbox" name="hobby" value="traveling"> 旅行
```

### 3. 下拉列表 `<select>`

`<select>` 标签用于创建下拉列表，用户可以从多个选项中选择一个或多个。

```html
<select name="country">
  <option value="china">中国</option>
  <option value="usa">美国</option>
  <option value="japan">日本</option>
</select>
```

### 4. 文本域 `<textarea>`

`<textarea>` 用于接收多行文本输入。

```html
<textarea name="message" rows="4" cols="50" placeholder="请输入您的留言"></textarea>
```

### 5. 提交按钮 `<button>`

`<button>` 标签用于创建提交按钮，用户点击后表单数据将被提交。

```html
<button type="submit">提交</button>
```

## 实践练习

### 练习 1: 创建一个简单的登录表单

创建一个包含用户名和密码输入框的登录表单，并添加一个提交按钮。

```html
<!DOCTYPE html>
<html lang="zh-CN">
<head>
  <meta charset="UTF-8">
  <title>登录表单</title>
</head>
<body>
  <form action="/login" method="POST">
    <label for="username">用户名:</label>
    <input type="text" id="username" name="username" placeholder="请输入用户名"><br><br>
    
    <label for="password">密码:</label>
    <input type="password" id="password" name="password" placeholder="请输入密码"><br><br>
    
    <button type="submit">登录</button>
  </form>
</body>
</html>
```

### 练习 2: 创建一个注册表单

创建一个包含用户名、密码、性别和兴趣爱好的注册表单。

```html
<!DOCTYPE html>
<html lang="zh-CN">
<head>
  <meta charset="UTF-8">
  <title>注册表单</title>
</head>
<body>
  <form action="/register" method="POST">
    <label for="username">用户名:</label>
    <input type="text" id="username" name="username" placeholder="请输入用户名"><br><br>
    
    <label for="password">密码:</label>
    <input type="password" id="password" name="password" placeholder="请输入密码"><br><br>
    
    <label>性别:</label><br>
    <input type="radio" id="male" name="gender" value="male">
    <label for="male">男</label><br>
    <input type="radio" id="female" name="gender" value="female">
    <label for="female">女</label><br><br>
    
    <label>兴趣爱好:</label><br>
    <input type="checkbox" id="reading" name="hobby" value="reading">
    <label for="reading">阅读</label><br>
    <input type="checkbox" id="traveling" name="hobby" value="traveling">
    <label for="traveling">旅行</label><br><br>
    
    <button type="submit">注册</button>
  </form>
</body>
</html>
```

## 总结

通过本教程，你学习了如何使用 HTML 创建表单和输入元素。表单是网页交互的核心，掌握这些基础知识将帮助你构建功能丰富的网页应用。继续练习和探索，你将能够创建更复杂的表单和交互功能。