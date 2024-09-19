---
title: HTML 和 CSS 基础教程
date: 2023-10-05
description: 本课程将带你从零开始学习HTML和CSS的基础知识，帮助你掌握网页开发的基本技能。
slug: html-css-basics
tags:
  - HTML
  - CSS
  - 网页开发
category: 前端开发
keywords:
  - HTML基础
  - CSS基础
  - 网页设计
---

# HTML, CSS 基础

## 概述

在现代Web开发中，HTML（超文本标记语言）和CSS（层叠样式表）是构建网页的基础。HTML用于定义网页的结构和内容，而CSS用于控制网页的样式和布局。本教程将带你从零开始学习HTML和CSS的基础知识，并通过实例和练习帮助你掌握这些技能。

## 1. HTML 基础

### 1.1 HTML 简介

HTML是一种标记语言，用于创建网页的结构。它使用标签（tags）来定义网页中的不同元素，如标题、段落、链接、图像等。

### 1.2 HTML 文档结构

一个基本的HTML文档包含以下几个部分：

```html
<!DOCTYPE html>
<html>
<head>
    <title>网页标题</title>
</head>
<body>
    <h1>这是一个标题</h1>
    <p>这是一个段落。</p>
</body>
</html>
```

- `<!DOCTYPE html>`: 声明文档类型为HTML5。
- `<html>`: 根元素，包含整个HTML文档。
- `<head>`: 包含文档的元数据，如标题、样式表链接等。
- `<title>`: 定义网页的标题，显示在浏览器的标签栏中。
- `<body>`: 包含网页的主体内容。

### 1.3 常用HTML标签

- `<h1>` 到 `<h6>`: 标题标签，`<h1>`是最高级别的标题，`<h6>`是最低级别的标题。
- `<p>`: 段落标签。
- `<a>`: 链接标签，用于创建超链接。
- `<img>`: 图像标签，用于插入图片。
- `<ul>` 和 `<li>`: 无序列表标签。
- `<ol>` 和 `<li>`: 有序列表标签。

**示例代码：**

```html
<h1>欢迎来到我的网站</h1>
<p>这是一个段落。</p>
<a href="https://www.example.com">访问示例网站</a>
<img src="image.jpg" alt="示例图片">
<ul>
    <li>列表项1</li>
    <li>列表项2</li>
</ul>
<ol>
    <li>列表项1</li>
    <li>列表项2</li>
</ol>
```

### 1.4 实践练习

创建一个简单的HTML页面，包含标题、段落、链接和图片。

## 2. CSS 基础

### 2.1 CSS 简介

CSS用于控制网页的样式和布局。通过CSS，你可以改变文本的颜色、字体、大小，设置元素的背景颜色、边框、间距等。

### 2.2 CSS 语法

CSS规则由选择器和声明块组成。选择器用于选择要应用样式的HTML元素，声明块包含一个或多个属性和值。

```css
选择器 {
    属性: 值;
    属性: 值;
}
```

**示例代码：**

```css
h1 {
    color: blue;
    font-size: 24px;
}

p {
    color: green;
    font-family: Arial, sans-serif;
}
```

### 2.3 常用CSS属性

- `color`: 设置文本颜色。
- `font-size`: 设置字体大小。
- `font-family`: 设置字体类型。
- `background-color`: 设置背景颜色。
- `border`: 设置边框。
- `margin`: 设置外边距。
- `padding`: 设置内边距。

### 2.4 实践练习

为你在练习1中创建的HTML页面添加CSS样式，改变标题和段落的样式。

## 3. HTML 和 CSS 结合使用

### 3.1 内联样式

你可以在HTML元素中直接使用`style`属性来应用CSS样式。

```html
<h1 style="color: red;">这是一个标题</h1>
```

### 3.2 内部样式表

你可以在HTML文档的`<head>`部分使用`<style>`标签来定义CSS样式。

```html
<!DOCTYPE html>
<html>
<head>
    <title>网页标题</title>
    <style>
        h1 {
            color: blue;
        }
        p {
            color: green;
        }
    </style>
</head>
<body>
    <h1>这是一个标题</h1>
    <p>这是一个段落。</p>
</body>
</html>
```

### 3.3 外部样式表

你可以将CSS样式放在一个单独的`.css`文件中，然后在HTML文档中通过`<link>`标签引入。

**style.css:**

```css
h1 {
    color: blue;
}

p {
    color: green;
}
```

**index.html:**

```html
<!DOCTYPE html>
<html>
<head>
    <title>网页标题</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <h1>这是一个标题</h1>
    <p>这是一个段落。</p>
</body>
</html>
```

### 3.4 实践练习

创建一个外部CSS文件，并将其链接到你的HTML页面，为页面添加样式。

## 4. 总结

通过本教程，你已经学习了HTML和CSS的基础知识，包括HTML文档结构、常用标签、CSS语法和常用属性。你还可以通过实践练习进一步巩固这些知识。继续探索和实践，你将能够创建出更加复杂和美观的网页。

## 5. 下一步

接下来，你可以学习如何使用JavaScript为网页添加交互功能，或者深入学习CSS的高级布局技术，如Flexbox和Grid布局。此外，你还可以开始学习Web开发框架，如Flask或Django，进一步扩展你的Web开发技能。