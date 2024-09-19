---
title: Angular 简介 - 从零开始学习Angular框架
date: 2023-10-05
description: 本课程将带你从零开始学习Angular框架，涵盖基础概念、组件、服务、路由等核心内容，帮助你快速掌握Angular开发技能。
slug: angular-introduction
tags:
  - Angular
  - 前端开发
  - 框架
category: 前端开发
keywords:
  - Angular 教程
  - Angular 入门
  - Angular 框架
---

# Angular 简介

## 1. 什么是 Angular？

Angular 是一个由 Google 开发和维护的开源前端框架，用于构建动态的单页应用（SPA）。它基于 TypeScript，提供了强大的工具和功能，帮助开发者构建复杂、高效且易于维护的 Web 应用。

### 1.1 Angular 的历史

- **AngularJS (1.x)**: 最初的 Angular 版本，使用 JavaScript 编写，主要用于构建单页应用。
- **Angular (2+)**: 从 Angular 2 开始，框架进行了彻底的重写，使用 TypeScript 编写，提供了更强大的功能和更好的性能。

### 1.2 Angular 的核心概念

- **组件 (Components)**: 构建应用的基本单元，每个组件包含 HTML 模板、CSS 样式和 TypeScript 逻辑。
- **模块 (Modules)**: 组织应用的结构，将相关的组件、服务和其他代码分组。
- **服务 (Services)**: 提供可重用的功能，如数据获取、业务逻辑等。
- **依赖注入 (Dependency Injection)**: 管理组件和服务之间的依赖关系。
- **路由 (Routing)**: 管理应用的不同视图和导航。

## 2. 开发环境设置

### 2.1 安装 Node.js 和 npm

Angular 需要 Node.js 和 npm（Node 包管理器）来运行。首先，确保你已经安装了 Node.js。你可以通过以下命令检查是否安装成功：

```bash
node -v
npm -v
```

### 2.2 安装 Angular CLI

Angular CLI 是一个命令行工具，用于创建、管理和构建 Angular 项目。你可以通过以下命令安装 Angular CLI：

```bash
npm install -g @angular/cli
```

安装完成后，你可以通过以下命令检查版本：

```bash
ng version
```

## 3. 创建第一个 Angular 项目

### 3.1 使用 Angular CLI 创建项目

你可以使用 Angular CLI 快速创建一个新的 Angular 项目：

```bash
ng new my-first-angular-app
```

在创建过程中，CLI 会询问你是否要添加 Angular Routing 和选择 CSS 预处理器。你可以根据需要选择。

### 3.2 启动开发服务器

进入项目目录并启动开发服务器：

```bash
cd my-first-angular-app
ng serve
```

打开浏览器并访问 `http://localhost:4200`，你应该会看到 Angular 的默认欢迎页面。

## 4. Angular 项目结构

一个典型的 Angular 项目结构如下：

```
my-first-angular-app/
├── src/
│   ├── app/
│   │   ├── app.component.css
│   │   ├── app.component.html
│   │   ├── app.component.ts
│   │   └── app.module.ts
│   ├── assets/
│   ├── environments/
│   ├── index.html
│   ├── main.ts
│   └── styles.css
├── angular.json
├── package.json
└── tsconfig.json
```

### 4.1 主要文件和目录

- **src/app/**: 包含应用的主要代码，包括组件、模块和服务。
- **src/index.html**: 应用的主 HTML 文件。
- **src/main.ts**: 应用的入口文件，负责启动 Angular 应用。
- **angular.json**: Angular 项目的配置文件。
- **package.json**: 管理项目的依赖和脚本。
- **tsconfig.json**: TypeScript 编译器的配置文件。

## 5. 创建第一个 Angular 组件

### 5.1 使用 Angular CLI 创建组件

你可以使用 Angular CLI 快速创建一个新的组件：

```bash
ng generate component my-first-component
```

这将在 `src/app/` 目录下创建一个新的组件文件夹，包含以下文件：

- **my-first-component.component.css**: 组件的样式文件。
- **my-first-component.component.html**: 组件的模板文件。
- **my-first-component.component.ts**: 组件的逻辑文件。
- **my-first-component.component.spec.ts**: 组件的测试文件。

### 5.2 组件的基本结构

```typescript
// my-first-component.component.ts
import { Component } from '@angular/core';

@Component({
  selector: 'app-my-first-component',
  templateUrl: './my-first-component.component.html',
  styleUrls: ['./my-first-component.component.css']
})
export class MyFirstComponentComponent {
  title = 'My First Angular Component';
}
```

### 5.3 在模板中使用组件

在 `app.component.html` 中使用新创建的组件：

```html
<!-- app.component.html -->
<h1>Welcome to My First Angular App</h1>
<app-my-first-component></app-my-first-component>
```

## 6. 数据绑定和事件处理

### 6.1 数据绑定

Angular 支持多种数据绑定方式：

- **插值 (Interpolation)**: 在模板中显示组件的属性值。

```html
<!-- my-first-component.component.html -->
<p>{{ title }}</p>
```

- **属性绑定 (Property Binding)**: 将组件的属性绑定到 HTML 元素的属性。

```html
<!-- my-first-component.component.html -->
<img [src]="imageUrl" alt="Image">
```

- **事件绑定 (Event Binding)**: 响应用户的交互事件。

```html
<!-- my-first-component.component.html -->
<button (click)="onButtonClick()">Click Me</button>
```

### 6.2 事件处理

在组件中定义事件处理函数：

```typescript
// my-first-component.component.ts
export class MyFirstComponentComponent {
  title = 'My First Angular Component';

  onButtonClick() {
    alert('Button Clicked!');
  }
}
```

## 7. 实践练习

### 7.1 练习：创建一个简单的待办事项应用

1. 创建一个新的 Angular 项目。
2. 创建一个 `TodoListComponent` 组件。
3. 在组件中定义一个 `todos` 数组，包含一些待办事项。
4. 使用数据绑定在模板中显示待办事项列表。
5. 添加一个输入框和一个按钮，允许用户添加新的待办事项。
6. 添加一个按钮，允许用户删除已完成的待办事项。

### 7.2 提示

- 使用 `ngFor` 指令遍历 `todos` 数组。
- 使用 `ngModel` 指令实现双向数据绑定。
- 使用 `splice` 方法删除数组中的元素。

## 8. 总结

通过本教程，你已经了解了 Angular 的基本概念、开发环境设置、项目结构、组件创建、数据绑定和事件处理。Angular 是一个功能强大的框架，适合构建复杂的单页应用。继续学习和实践，你将能够掌握更多高级功能，如服务、路由、依赖注入等。

## 9. 下一步

- 学习 Angular 的路由系统，实现多视图应用。
- 探索 Angular 的服务和依赖注入，实现模块化和可重用的代码。
- 学习 Angular 的表单处理，实现复杂的用户输入验证。
- 深入研究 Angular 的测试框架，确保应用的稳定性和可靠性。

希望这篇教程能帮助你快速入门 Angular，并在未来的项目中应用所学知识。祝你编程愉快！