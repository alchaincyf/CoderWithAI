---
title: 第一个 Angular 应用
date: 2023-10-05
description: 本课程将指导您如何从零开始创建您的第一个 Angular 应用，涵盖从环境设置到基本组件开发的完整流程。
slug: first-angular-app
tags:
  - Angular
  - 前端开发
  - 初学者
category: 前端开发
keywords:
  - Angular 教程
  - 前端开发
  - Angular 初学者
---

# 第一个 Angular 应用

在本教程中，我们将带领你创建你的第一个 Angular 应用。通过这个项目，你将学习到 Angular 的基础知识，包括环境搭建、项目结构、组件创建和使用、数据绑定等。

## 1. Angular 简介和特性

Angular 是一个由 Google 开发的开源前端框架，用于构建动态的单页应用（SPA）。它使用 TypeScript 作为主要编程语言，并提供了强大的工具和库来简化开发过程。Angular 的主要特性包括：

- **组件化开发**：Angular 应用由多个组件组成，每个组件负责特定的功能。
- **双向数据绑定**：自动同步视图和模型之间的数据。
- **依赖注入**：简化组件和服务之间的依赖管理。
- **模块化设计**：通过模块组织代码，提高代码的可维护性。

## 2. TypeScript 基础

TypeScript 是 JavaScript 的超集，添加了静态类型检查和其他高级功能。在 Angular 中，TypeScript 是首选的编程语言。以下是一些基础概念：

- **类型注解**：`let name: string = 'Angular';`
- **接口**：定义对象的结构，如 `interface User { name: string; age: number; }`
- **类**：`class Person { constructor(public name: string) {} }`

## 3. 环境搭建

在开始编写 Angular 应用之前，我们需要安装一些必要的工具：

### 3.1 安装 Node.js 和 npm

Node.js 是一个基于 Chrome V8 引擎的 JavaScript 运行时，npm 是 Node.js 的包管理器。

1. 访问 [Node.js 官网](https://nodejs.org/) 下载并安装 Node.js。
2. 安装完成后，打开终端并运行 `node -v` 和 `npm -v` 确认安装成功。

### 3.2 安装 Angular CLI

Angular CLI 是一个命令行工具，用于快速生成 Angular 项目和组件。

```bash
npm install -g @angular/cli
```

安装完成后，运行 `ng version` 确认安装成功。

## 4. 创建第一个 Angular 应用

### 4.1 生成新项目

使用 Angular CLI 生成一个新的 Angular 项目：

```bash
ng new my-first-app
```

在生成过程中，CLI 会询问你是否要添加 Angular Routing 和选择样式表格式。你可以根据需要选择。

### 4.2 启动开发服务器

进入项目目录并启动开发服务器：

```bash
cd my-first-app
ng serve --open
```

`ng serve` 命令会启动开发服务器，`--open` 参数会自动打开浏览器并访问 `http://localhost:4200/`。

## 5. Angular 项目结构

Angular 项目的目录结构如下：

```
my-first-app/
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

- **src/app/**：包含应用的主要代码，包括组件、服务和模块。
- **src/assets/**：存放静态资源文件，如图片、字体等。
- **src/environments/**：包含不同环境的配置文件。
- **src/index.html**：应用的主 HTML 文件。
- **src/main.ts**：应用的入口文件。
- **angular.json**：Angular CLI 的配置文件。
- **package.json**：项目的依赖和脚本配置。
- **tsconfig.json**：TypeScript 编译器的配置文件。

## 6. 组件创建和使用

### 6.1 创建新组件

使用 Angular CLI 创建一个新的组件：

```bash
ng generate component hello-world
```

这会在 `src/app/` 目录下生成一个新的组件文件夹，包含四个文件：

- `hello-world.component.css`：组件的样式文件。
- `hello-world.component.html`：组件的模板文件。
- `hello-world.component.ts`：组件的 TypeScript 文件。
- `hello-world.component.spec.ts`：组件的测试文件。

### 6.2 在模板中使用组件

在 `app.component.html` 中使用新创建的组件：

```html
<app-hello-world></app-hello-world>
```

### 6.3 组件的 TypeScript 文件

在 `hello-world.component.ts` 中，我们可以定义组件的逻辑：

```typescript
import { Component } from '@angular/core';

@Component({
  selector: 'app-hello-world',
  templateUrl: './hello-world.component.html',
  styleUrls: ['./hello-world.component.css']
})
export class HelloWorldComponent {
  title = 'Hello, World!';
}
```

### 6.4 组件的模板文件

在 `hello-world.component.html` 中，我们可以定义组件的视图：

```html
<h1>{{ title }}</h1>
```

### 6.5 组件的样式文件

在 `hello-world.component.css` 中，我们可以定义组件的样式：

```css
h1 {
  color: blue;
}
```

## 7. 模板语法

Angular 的模板语法允许我们在 HTML 中嵌入动态内容和逻辑。

### 7.1 插值表达式

使用双大括号 `{{ }}` 进行插值：

```html
<p>{{ title }}</p>
```

### 7.2 属性绑定

使用方括号 `[]` 进行属性绑定：

```html
<img [src]="imageUrl">
```

### 7.3 事件绑定

使用圆括号 `()` 进行事件绑定：

```html
<button (click)="onClick()">Click me</button>
```

### 7.4 双向数据绑定

使用 `[(ngModel)]` 进行双向数据绑定：

```html
<input [(ngModel)]="name">
<p>Hello, {{ name }}!</p>
```

## 8. 数据绑定

数据绑定是 Angular 的核心特性之一，它允许视图和模型之间的数据自动同步。

### 8.1 单向数据绑定

单向数据绑定分为从模型到视图（插值表达式）和从视图到模型（事件绑定）。

### 8.2 双向数据绑定

双向数据绑定使用 `[(ngModel)]`，适用于表单元素。

## 9. 生命周期钩子

Angular 组件有多个生命周期钩子，允许我们在组件的不同阶段执行代码。

### 9.1 `ngOnInit`

在组件初始化时调用：

```typescript
ngOnInit(): void {
  console.log('Component initialized');
}
```

### 9.2 `ngOnChanges`

在输入属性变化时调用：

```typescript
ngOnChanges(changes: SimpleChanges): void {
  console.log('Input properties changed', changes);
}
```

### 9.3 `ngOnDestroy`

在组件销毁时调用：

```typescript
ngOnDestroy(): void {
  console.log('Component destroyed');
}
```

## 10. 组件通信

### 10.1 `@Input`

父组件向子组件传递数据：

```typescript
// 父组件
<app-child [message]="parentMessage"></app-child>

// 子组件
@Input() message: string;
```

### 10.2 `@Output`

子组件向父组件发送事件：

```typescript
// 子组件
@Output() messageEvent = new EventEmitter<string>();

sendMessage() {
  this.messageEvent.emit('Hello from child');
}

// 父组件
<app-child (messageEvent)="receiveMessage($event)"></app-child>

receiveMessage(message: string) {
  console.log(message);
}
```

## 11. 内置指令

### 11.1 `*ngIf`

条件渲染：

```html
<p *ngIf="isVisible">This is visible</p>
```

### 11.2 `*ngFor`

循环渲染：

```html
<ul>
  <li *ngFor="let item of items">{{ item }}</li>
</ul>
```

### 11.3 `ngSwitch`

条件渲染：

```html
<div [ngSwitch]="condition">
  <p *ngSwitchCase="'A'">Case A</p>
  <p *ngSwitchCase="'B'">Case B</p>
  <p *ngSwitchDefault>Default case</p>
</div>
```

## 12. 实践练习

### 12.1 创建一个简单的待办事项应用

1. 创建一个新的 Angular 项目。
2. 创建一个 `TodoListComponent` 组件。
3. 在组件中定义一个 `todos` 数组，包含一些待办事项。
4. 使用 `*ngFor` 指令在模板中循环渲染待办事项。
5. 添加一个输入框和一个按钮，允许用户添加新的待办事项。
6. 使用 `[(ngModel)]` 进行双向数据绑定，实现添加功能。

### 12.2 代码示例

```typescript
// todo-list.component.ts
import { Component } from '@angular/core';

@Component({
  selector: 'app-todo-list',
  templateUrl: './todo-list.component.html',
  styleUrls: ['./todo-list.component.css']
})
export class TodoListComponent {
  todos: string[] = ['Buy groceries', 'Walk the dog'];
  newTodo: string = '';

  addTodo() {
    if (this.newTodo) {
      this.todos.push(this.newTodo);
      this.newTodo = '';
    }
  }
}
```

```html
<!-- todo-list.component.html -->
<input [(ngModel)]="newTodo" placeholder="New todo">
<button (click)="addTodo()">Add</button>

<ul>
  <li *ngFor="let todo of todos">{{ todo }}</li>
</ul>
```

## 13. 总结

通过本教程，你已经学会了如何创建你的第一个 Angular 应用。你了解了 Angular 的基础知识，包括环境搭建、项目结构、组件创建和使用、数据绑定、生命周期钩子、组件通信和内置指令。接下来，你可以继续深入学习 Angular 的其他高级特性，如服务、路由、表单、HTTP 请求等。

希望你能享受 Angular 开发的过程，并不断探索和实践，提升你的前端开发技能！