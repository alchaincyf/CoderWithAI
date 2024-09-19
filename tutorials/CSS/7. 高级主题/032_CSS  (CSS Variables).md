---
title: CSS 自定义属性 (CSS Variables) 教程
date: 2023-10-05
description: 本课程详细讲解CSS自定义属性（CSS Variables）的使用方法，帮助你提升网页设计的灵活性和效率。
slug: css-variables-tutorial
tags:
  - CSS
  - 前端开发
  - 网页设计
category: 前端开发
keywords:
  - CSS自定义属性
  - CSS Variables
  - 前端开发
---

# CSS 自定义属性 (CSS Variables) 教程

## 1. 简介

CSS 自定义属性，也称为 CSS 变量，是一种在 CSS 中定义和使用变量的方式。通过使用自定义属性，开发者可以更高效地管理和维护样式代码，尤其是在处理大型项目时。CSS 自定义属性允许你在 CSS 中定义一次值，然后在多个地方重复使用，从而减少代码冗余并提高可维护性。

## 2. 基本语法

### 2.1 定义自定义属性

自定义属性以 `--` 开头，并定义在某个选择器中。例如：

```css
:root {
  --primary-color: #4a90e2;
  --font-size: 16px;
}
```

在这个例子中，我们定义了两个自定义属性：`--primary-color` 和 `--font-size`。这些属性可以在整个文档中使用。

### 2.2 使用自定义属性

使用 `var()` 函数来引用自定义属性。例如：

```css
body {
  color: var(--primary-color);
  font-size: var(--font-size);
}
```

在这个例子中，`body` 元素的文本颜色和字体大小分别使用了我们之前定义的自定义属性。

## 3. 作用域

自定义属性具有作用域，这意味着它们只在定义它们的选择器及其子元素中可用。例如：

```css
:root {
  --global-color: #333;
}

.container {
  --container-color: #666;
}

body {
  color: var(--global-color);
}

.container p {
  color: var(--container-color);
}
```

在这个例子中，`--global-color` 在整个文档中可用，而 `--container-color` 只在 `.container` 及其子元素中可用。

## 4. 动态更新

自定义属性可以通过 JavaScript 动态更新。例如：

```html
<button id="theme-toggle">Toggle Theme</button>

<style>
  :root {
    --bg-color: #fff;
    --text-color: #000;
  }

  body {
    background-color: var(--bg-color);
    color: var(--text-color);
  }
</style>

<script>
  document.getElementById('theme-toggle').addEventListener('click', function() {
    const root = document.documentElement;
    root.style.setProperty('--bg-color', root.style.getPropertyValue('--bg-color') === '#fff' ? '#000' : '#fff');
    root.style.setProperty('--text-color', root.style.getPropertyValue('--text-color') === '#000' ? '#fff' : '#000');
  });
</script>
```

在这个例子中，点击按钮会切换页面的背景颜色和文本颜色。

## 5. 实践练习

### 5.1 练习目标

创建一个简单的网页，使用 CSS 自定义属性来定义主题颜色，并通过按钮动态切换主题。

### 5.2 代码示例

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>CSS Variables Practice</title>
  <style>
    :root {
      --primary-color: #4a90e2;
      --secondary-color: #f5a623;
      --bg-color: #fff;
      --text-color: #000;
    }

    body {
      background-color: var(--bg-color);
      color: var(--text-color);
      font-family: Arial, sans-serif;
      transition: background-color 0.3s, color 0.3s;
    }

    .container {
      padding: 20px;
      text-align: center;
    }

    button {
      background-color: var(--primary-color);
      color: #fff;
      border: none;
      padding: 10px 20px;
      cursor: pointer;
      margin-top: 20px;
    }

    button:hover {
      background-color: var(--secondary-color);
    }
  </style>
</head>
<body>
  <div class="container">
    <h1>Welcome to My Website</h1>
    <p>This is a simple example of using CSS Variables.</p>
    <button id="theme-toggle">Toggle Theme</button>
  </div>

  <script>
    document.getElementById('theme-toggle').addEventListener('click', function() {
      const root = document.documentElement;
      root.style.setProperty('--bg-color', root.style.getPropertyValue('--bg-color') === '#fff' ? '#333' : '#fff');
      root.style.setProperty('--text-color', root.style.getPropertyValue('--text-color') === '#000' ? '#fff' : '#000');
    });
  </script>
</body>
</html>
```

### 5.3 练习步骤

1. 复制上述代码到一个 HTML 文件中。
2. 打开浏览器，加载该文件。
3. 点击按钮，观察页面主题颜色的变化。

## 6. 总结

CSS 自定义属性是一种强大的工具，可以帮助开发者更高效地管理和维护样式代码。通过定义和使用自定义属性，你可以减少代码冗余，提高代码的可维护性，并实现动态样式更新。希望本教程能帮助你更好地理解和应用 CSS 自定义属性。

## 7. 进一步学习

- [MDN Web Docs: CSS Custom Properties](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_custom_properties)
- [CSS-Tricks: CSS Variables](https://css-tricks.com/a-complete-guide-to-custom-properties/)

通过这些资源，你可以深入学习 CSS 自定义属性的更多高级用法和最佳实践。