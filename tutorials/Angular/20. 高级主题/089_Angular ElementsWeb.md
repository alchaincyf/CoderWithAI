---
title: 深入理解Angular Elements：构建可重用的Web组件
date: 2023-10-05
description: 本课程将深入探讨Angular Elements的概念和应用，教你如何将Angular组件转换为自包含的Web组件，以便在非Angular应用中重用。
slug: angular-elements-tutorial
tags:
  - Angular
  - Web Components
  - Frontend Development
category: Web开发
keywords:
  - Angular Elements
  - Web组件
  - Angular教程
---

# Angular Elements 教程

## 1. 概述

Angular Elements 是 Angular 框架的一部分，允许开发者将 Angular 组件打包成自定义元素（Custom Elements），这些元素可以在任何支持 Web Components 标准的现代浏览器中使用。通过 Angular Elements，你可以将 Angular 组件嵌入到非 Angular 应用中，或者在同一应用中使用多个 Angular 版本。

### 1.1 什么是自定义元素？

自定义元素（Custom Elements）是 Web Components 标准的一部分，允许开发者创建新的 HTML 元素。这些元素可以像原生 HTML 元素一样使用，并且可以在任何支持 Web Components 的浏览器中运行。

### 1.2 Angular Elements 的优势

- **跨框架使用**：可以在非 Angular 应用中使用 Angular 组件。
- **渐进增强**：可以在现有应用中逐步引入 Angular 组件。
- **复用性**：可以将 Angular 组件打包成独立的库，供多个项目使用。

## 2. 环境准备

在开始之前，确保你已经安装了以下工具：

- Node.js（建议版本 14.x 或更高）
- npm（通常随 Node.js 一起安装）
- Angular CLI（通过 `npm install -g @angular/cli` 安装）

### 2.1 创建新项目

首先，使用 Angular CLI 创建一个新的 Angular 项目：

```bash
ng new angular-elements-demo
cd angular-elements-demo
```

### 2.2 安装 Angular Elements 依赖

接下来，安装 Angular Elements 和相关依赖：

```bash
ng add @angular/elements
```

这个命令会自动添加必要的依赖项，并配置项目以支持 Angular Elements。

## 3. 创建自定义元素

### 3.1 创建 Angular 组件

首先，创建一个新的 Angular 组件：

```bash
ng generate component hello-world
```

### 3.2 修改组件

打开 `hello-world.component.ts` 文件，修改组件的模板和逻辑：

```typescript
import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-hello-world',
  template: `<h1>Hello, {{name}}!</h1>`,
  styles: [`h1 { font-family: Lato; }`]
})
export class HelloWorldComponent {
  @Input() name: string = 'World';
}
```

### 3.3 注册自定义元素

在 `app.module.ts` 文件中，注册自定义元素：

```typescript
import { NgModule, Injector } from '@angular/core';
import { createCustomElement } from '@angular/elements';
import { BrowserModule } from '@angular/platform-browser';
import { HelloWorldComponent } from './hello-world/hello-world.component';

@NgModule({
  declarations: [HelloWorldComponent],
  imports: [BrowserModule],
  entryComponents: [HelloWorldComponent]
})
export class AppModule {
  constructor(private injector: Injector) {
    const el = createCustomElement(HelloWorldComponent, { injector });
    customElements.define('hello-world', el);
  }

  ngDoBootstrap() {}
}
```

### 3.4 构建项目

使用 Angular CLI 构建项目：

```bash
ng build --prod
```

### 3.5 使用自定义元素

将构建生成的文件（通常在 `dist/angular-elements-demo` 目录下）部署到服务器，然后在 HTML 文件中使用自定义元素：

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Angular Elements Demo</title>
  <script src="main.js"></script>
</head>
<body>
  <hello-world name="Angular"></hello-world>
</body>
</html>
```

## 4. 实践练习

### 4.1 创建更多自定义元素

尝试创建更多的 Angular 组件，并将它们注册为自定义元素。例如，创建一个 `counter` 组件，允许用户点击按钮增加计数。

### 4.2 集成到现有项目

将你创建的自定义元素集成到一个现有的非 Angular 项目中，观察它们如何与其他框架或原生 JavaScript 代码交互。

## 5. 总结

通过本教程，你学习了如何使用 Angular Elements 将 Angular 组件打包成自定义元素，并在不同的环境中使用它们。Angular Elements 提供了一种强大的方式来扩展 Angular 组件的复用性和可移植性，使你能够更灵活地构建现代 Web 应用。

### 5.1 下一步

- 探索更多 Angular Elements 的高级特性，如事件处理和属性绑定。
- 研究如何在 Angular Elements 中使用 Angular 的依赖注入和服务。
- 尝试将 Angular Elements 与 Web Components 的其他部分（如 Shadow DOM 和 HTML Templates）结合使用。

希望本教程能帮助你更好地理解和应用 Angular Elements，提升你的 Angular 开发技能！