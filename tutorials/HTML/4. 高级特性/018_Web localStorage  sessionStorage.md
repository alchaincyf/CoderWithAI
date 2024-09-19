---
title: Web 存储详解：localStorage 与 sessionStorage
date: 2023-10-05
description: 本课程详细讲解了Web存储的两种主要方式：localStorage和sessionStorage，帮助开发者理解如何在浏览器中持久化数据。
slug: web-storage-localstorage-sessionstorage
tags:
  - Web开发
  - JavaScript
  - 前端技术
category: 前端开发
keywords:
  - localStorage
  - sessionStorage
  - Web存储
  - 浏览器存储
---

# Web 存储（localStorage, sessionStorage）

## 概述

在现代 Web 开发中，Web 存储是一种非常重要的技术，它允许我们在用户的浏览器中存储数据。这种存储方式比传统的 Cookies 更加灵活和强大。Web 存储主要分为两种类型：`localStorage` 和 `sessionStorage`。

- **localStorage**: 数据存储在用户的浏览器中，除非用户手动清除，否则数据会一直存在。
- **sessionStorage**: 数据存储在用户的浏览器中，但仅在当前会话期间有效。一旦用户关闭浏览器窗口或标签页，数据就会被清除。

## localStorage 和 sessionStorage 的基本用法

### 设置数据

你可以使用 `setItem` 方法来存储数据。`setItem` 接受两个参数：键（key）和值（value）。

```javascript
// 使用 localStorage 存储数据
localStorage.setItem('username', 'JohnDoe');

// 使用 sessionStorage 存储数据
sessionStorage.setItem('token', 'abc123');
```

### 获取数据

你可以使用 `getItem` 方法来获取存储的数据。`getItem` 接受一个参数：键（key）。

```javascript
// 使用 localStorage 获取数据
let username = localStorage.getItem('username');
console.log(username); // 输出: JohnDoe

// 使用 sessionStorage 获取数据
let token = sessionStorage.getItem('token');
console.log(token); // 输出: abc123
```

### 删除数据

你可以使用 `removeItem` 方法来删除存储的数据。`removeItem` 接受一个参数：键（key）。

```javascript
// 使用 localStorage 删除数据
localStorage.removeItem('username');

// 使用 sessionStorage 删除数据
sessionStorage.removeItem('token');
```

### 清除所有数据

你可以使用 `clear` 方法来清除所有存储的数据。

```javascript
// 使用 localStorage 清除所有数据
localStorage.clear();

// 使用 sessionStorage 清除所有数据
sessionStorage.clear();
```

## 实践练习

### 练习 1: 存储用户偏好

创建一个简单的 HTML 页面，允许用户选择主题颜色（如“浅色”或“深色”），并将选择存储在 `localStorage` 中。页面加载时，根据存储的偏好自动应用相应的主题颜色。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>主题选择</title>
    <style>
        body {
            font-family: Arial, sans-serif;
        }
        .light-theme {
            background-color: #f0f0f0;
            color: #000;
        }
        .dark-theme {
            background-color: #333;
            color: #fff;
        }
    </style>
</head>
<body>
    <h1>选择主题颜色</h1>
    <select id="theme-select">
        <option value="light">浅色</option>
        <option value="dark">深色</option>
    </select>
    <button id="save-button">保存</button>

    <script>
        // 加载时应用存储的主题
        document.addEventListener('DOMContentLoaded', () => {
            let theme = localStorage.getItem('theme');
            if (theme) {
                document.body.className = theme + '-theme';
            }
        });

        // 保存按钮点击事件
        document.getElementById('save-button').addEventListener('click', () => {
            let theme = document.getElementById('theme-select').value;
            localStorage.setItem('theme', theme);
            document.body.className = theme + '-theme';
        });
    </script>
</body>
</html>
```

### 练习 2: 会话存储购物车

创建一个简单的购物车页面，允许用户添加商品到购物车。使用 `sessionStorage` 存储购物车内容。页面刷新后，购物车内容应保持不变。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>购物车</title>
</head>
<body>
    <h1>购物车</h1>
    <ul id="cart-items"></ul>
    <button id="add-item">添加商品</button>

    <script>
        // 加载时显示购物车内容
        document.addEventListener('DOMContentLoaded', () => {
            let cart = sessionStorage.getItem('cart');
            if (cart) {
                cart = JSON.parse(cart);
                cart.forEach(item => {
                    addItemToCart(item);
                });
            }
        });

        // 添加商品到购物车
        document.getElementById('add-item').addEventListener('click', () => {
            let item = '商品' + (Math.random() * 100).toFixed(0);
            addItemToCart(item);
            saveCart();
        });

        // 添加商品到页面
        function addItemToCart(item) {
            let li = document.createElement('li');
            li.textContent = item;
            document.getElementById('cart-items').appendChild(li);
        }

        // 保存购物车内容
        function saveCart() {
            let cartItems = Array.from(document.querySelectorAll('#cart-items li')).map(li => li.textContent);
            sessionStorage.setItem('cart', JSON.stringify(cartItems));
        }
    </script>
</body>
</html>
```

## 总结

Web 存储（`localStorage` 和 `sessionStorage`）是现代 Web 开发中非常有用的工具，它们允许我们在用户的浏览器中存储数据。`localStorage` 适合存储长期数据，而 `sessionStorage` 适合存储临时数据。通过合理使用这两种存储方式，我们可以提升用户体验，并简化数据管理。

希望这篇教程能帮助你更好地理解和使用 Web 存储技术。继续探索和实践，你会发现它们在实际项目中的巨大潜力！