---
title: Web Components 简介
date: 2023-10-05
description: 本课程介绍Web Components的基本概念和使用方法，帮助开发者理解和创建可重用的自定义HTML元素。
slug: web-components-introduction
tags:
  - Web Components
  - 前端开发
  - HTML
category: 前端开发
keywords:
  - Web Components
  - 自定义元素
  - Shadow DOM
---

# Web Components 简介

## 概述

Web Components 是一种现代 Web 开发技术，允许开发者创建可重用的自定义元素，这些元素可以在任何 HTML 文档中使用，而无需担心与其他代码的冲突。Web Components 由四个主要技术组成：Custom Elements、Shadow DOM、HTML Templates 和 HTML Imports（虽然 HTML Imports 已被废弃，但其他三个技术仍然广泛使用）。

## 1. Custom Elements

Custom Elements 允许开发者定义自己的 HTML 元素。这些自定义元素可以像标准 HTML 元素一样使用，并且可以拥有自己的行为和样式。

### 1.1 创建自定义元素

要创建一个自定义元素，你需要使用 JavaScript 的 `customElements.define()` 方法。这个方法接受两个参数：自定义元素的名称和一个继承自 `HTMLElement` 的类。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Custom Elements Example</title>
</head>
<body>
    <my-element></my-element>

    <script>
        class MyElement extends HTMLElement {
            constructor() {
                super();
                this.innerHTML = `<p>Hello, I am a custom element!</p>`;
            }
        }

        customElements.define('my-element', MyElement);
    </script>
</body>
</html>
```

### 1.2 生命周期回调

自定义元素可以定义生命周期回调方法，这些方法会在元素的不同生命周期阶段被调用。

- `connectedCallback()`: 当元素被插入到 DOM 中时调用。
- `disconnectedCallback()`: 当元素从 DOM 中移除时调用。
- `attributeChangedCallback(name, oldValue, newValue)`: 当元素的属性发生变化时调用。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Custom Elements Lifecycle</title>
</head>
<body>
    <my-element name="Web Components"></my-element>

    <script>
        class MyElement extends HTMLElement {
            static get observedAttributes() {
                return ['name'];
            }

            constructor() {
                super();
                this.innerHTML = `<p>Hello, I am a custom element!</p>`;
            }

            connectedCallback() {
                console.log('Element added to the DOM');
            }

            disconnectedCallback() {
                console.log('Element removed from the DOM');
            }

            attributeChangedCallback(name, oldValue, newValue) {
                console.log(`Attribute ${name} changed from ${oldValue} to ${newValue}`);
                this.innerHTML = `<p>Hello, ${newValue}!</p>`;
            }
        }

        customElements.define('my-element', MyElement);
    </script>
</body>
</html>
```

## 2. Shadow DOM

Shadow DOM 允许开发者将一个隐藏的、独立的 DOM 树附加到元素上。这个 Shadow DOM 树与主文档的 DOM 树是分离的，因此可以避免样式和脚本的冲突。

### 2.1 创建 Shadow DOM

要创建一个 Shadow DOM，你可以使用 `attachShadow()` 方法。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Shadow DOM Example</title>
</head>
<body>
    <my-element></my-element>

    <script>
        class MyElement extends HTMLElement {
            constructor() {
                super();
                const shadow = this.attachShadow({ mode: 'open' });
                shadow.innerHTML = `
                    <style>
                        p {
                            color: red;
                        }
                    </style>
                    <p>Hello, I am a custom element with Shadow DOM!</p>
                `;
            }
        }

        customElements.define('my-element', MyElement);
    </script>
</body>
</html>
```

### 2.2 Shadow DOM 的模式

Shadow DOM 有两种模式：`open` 和 `closed`。

- `open`: 外部脚本可以通过 `element.shadowRoot` 访问 Shadow DOM。
- `closed`: 外部脚本无法访问 Shadow DOM。

## 3. HTML Templates

HTML Templates 允许你在 HTML 中定义可重用的模板。这些模板不会在页面加载时立即渲染，而是在需要时通过 JavaScript 实例化。

### 3.1 定义模板

你可以使用 `<template>` 标签来定义一个模板。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>HTML Templates Example</title>
</head>
<body>
    <template id="my-template">
        <p>Hello, I am a template!</p>
    </template>

    <my-element></my-element>

    <script>
        class MyElement extends HTMLElement {
            constructor() {
                super();
                const template = document.getElementById('my-template').content;
                const shadow = this.attachShadow({ mode: 'open' });
                shadow.appendChild(template.cloneNode(true));
            }
        }

        customElements.define('my-element', MyElement);
    </script>
</body>
</html>
```

## 4. 实践练习

### 4.1 创建一个自定义按钮

创建一个自定义按钮元素，当点击时显示一个消息。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Custom Button Example</title>
</head>
<body>
    <my-button></my-button>

    <script>
        class MyButton extends HTMLElement {
            constructor() {
                super();
                const shadow = this.attachShadow({ mode: 'open' });
                shadow.innerHTML = `
                    <style>
                        button {
                            background-color: blue;
                            color: white;
                            padding: 10px 20px;
                            border: none;
                            cursor: pointer;
                        }
                    </style>
                    <button>Click Me</button>
                `;

                const button = shadow.querySelector('button');
                button.addEventListener('click', () => {
                    alert('Button clicked!');
                });
            }
        }

        customElements.define('my-button', MyButton);
    </script>
</body>
</html>
```

### 4.2 创建一个可重用的卡片组件

创建一个可重用的卡片组件，显示标题和内容。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Custom Card Example</title>
</head>
<body>
    <my-card title="Web Components" content="Learn about Web Components and how to use them."></my-card>

    <script>
        class MyCard extends HTMLElement {
            constructor() {
                super();
                const shadow = this.attachShadow({ mode: 'open' });
                const title = this.getAttribute('title');
                const content = this.getAttribute('content');

                shadow.innerHTML = `
                    <style>
                        .card {
                            border: 1px solid #ccc;
                            border-radius: 5px;
                            padding: 10px;
                            width: 300px;
                        }
                        .card h2 {
                            margin: 0;
                        }
                        .card p {
                            margin: 10px 0 0 0;
                        }
                    </style>
                    <div class="card">
                        <h2>${title}</h2>
                        <p>${content}</p>
                    </div>
                `;
            }
        }

        customElements.define('my-card', MyCard);
    </script>
</body>
</html>
```

## 总结

Web Components 提供了一种强大的方式来创建可重用的、封装的 HTML 元素。通过 Custom Elements、Shadow DOM 和 HTML Templates，你可以构建复杂的、可维护的 Web 应用程序。希望这篇教程能帮助你理解并开始使用 Web Components。

## 进一步学习

- [MDN Web Components](https://developer.mozilla.org/en-US/docs/Web/Web_Components)
- [W3C Web Components Specifications](https://www.w3.org/TR/components-intro/)
- [Google Web Fundamentals: Web Components](https://developers.google.com/web/fundamentals/web-components)

通过这些资源，你可以深入学习 Web Components 的各个方面，并在实际项目中应用它们。