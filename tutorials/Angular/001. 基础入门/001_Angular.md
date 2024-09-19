---
title: Angular 简介和特性
date: 2023-10-05
description: 本课程将介绍Angular框架的基本概念和主要特性，帮助初学者快速入门并掌握Angular的核心功能。
slug: angular-introduction-features
tags:
  - Angular
  - 前端开发
  - 框架
category: 前端开发
keywords:
  - Angular 简介
  - Angular 特性
  - 前端框架
---

# Angular 简介和特性

## 1. 什么是 Angular？

Angular 是一个由 Google 开发和维护的开源前端框架，用于构建动态的单页应用（SPA）。它基于 TypeScript，提供了强大的工具和库来简化前端开发过程。Angular 的核心特性包括组件化、依赖注入、模块化、双向数据绑定等。

### 1.1 Angular 的历史

Angular 最初由 Google 在 2010 年发布，名为 AngularJS。随着前端技术的快速发展，AngularJS 在 2016 年升级为 Angular 2，并采用了全新的架构和语言（TypeScript）。此后，Angular 经历了多个版本的迭代，目前最新版本为 Angular 14。

### 1.2 Angular 的主要特性

- **组件化**：Angular 应用由多个组件组成，每个组件负责特定的功能模块。
- **双向数据绑定**：数据模型和视图之间的自动同步。
- **依赖注入**：简化组件和服务之间的依赖管理。
- **模块化**：通过模块组织代码，提高代码的可维护性和可重用性。
- **强大的 CLI 工具**：提供命令行工具，简化开发流程。

## 2. 安装和环境搭建

在开始使用 Angular 之前，我们需要安装一些必要的工具和环境。

### 2.1 安装 Node.js 和 npm

Angular 依赖于 Node.js 和 npm（Node Package Manager）。首先，你需要安装 Node.js。安装完成后，npm 会自动安装。

```bash
# 检查 Node.js 和 npm 是否安装成功
node -v
npm -v
```

### 2.2 安装 Angular CLI

Angular CLI 是一个命令行工具，用于创建、构建和管理 Angular 项目。

```bash
# 全局安装 Angular CLI
npm install -g @angular/cli

# 检查 Angular CLI 是否安装成功
ng version
```

## 3. 创建第一个 Angular 应用

使用 Angular CLI 可以快速创建一个新的 Angular 项目。

```bash
# 创建一个新的 Angular 项目
ng new my-first-app

# 进入项目目录
cd my-first-app

# 启动开发服务器
ng serve
```

打开浏览器，访问 `http://localhost:4200`，你将看到一个简单的 Angular 应用。

## 4. Angular 项目结构

一个典型的 Angular 项目结构如下：

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

- `src/`：包含应用的所有源代码。
- `app/`：包含应用的主要组件和模块。
- `assets/`：存放静态资源文件。
- `environments/`：包含不同环境的配置文件。
- `index.html`：应用的主 HTML 文件。
- `main.ts`：应用的入口文件。
- `styles.css`：全局样式文件。

## 5. 组件创建和使用

组件是 Angular 应用的基本构建块。每个组件由一个 TypeScript 类、一个 HTML 模板和一个 CSS 文件组成。

### 5.1 创建新组件

使用 Angular CLI 可以快速创建新组件。

```bash
# 创建一个名为 'hello-world' 的新组件
ng generate component hello-world
```

### 5.2 组件的结构

```typescript
// hello-world.component.ts
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

```html
<!-- hello-world.component.html -->
<h1>{{ title }}</h1>
```

```css
/* hello-world.component.css */
h1 {
  color: blue;
}
```

### 5.3 使用组件

在 `app.component.html` 中使用新创建的组件：

```html
<!-- app.component.html -->
<app-hello-world></app-hello-world>
```

## 6. 模板语法

Angular 的模板语法允许你在 HTML 中嵌入动态内容。

### 6.1 插值表达式

使用双大括号 `{{ }}` 进行数据绑定。

```html
<p>{{ title }}</p>
```

### 6.2 属性绑定

使用方括号 `[]` 进行属性绑定。

```html
<img [src]="imageUrl">
```

### 6.3 事件绑定

使用圆括号 `()` 进行事件绑定。

```html
<button (click)="onClick()">Click me</button>
```

## 7. 数据绑定

数据绑定是 Angular 的核心特性之一，它允许数据在组件和视图之间自动同步。

### 7.1 单向数据绑定

从组件到视图的数据绑定。

```html
<p>{{ message }}</p>
```

### 7.2 双向数据绑定

使用 `[(ngModel)]` 实现双向数据绑定。

```html
<input [(ngModel)]="name">
<p>Hello, {{ name }}!</p>
```

## 8. 生命周期钩子

Angular 组件具有生命周期钩子，允许你在组件的不同阶段执行代码。

### 8.1 常用生命周期钩子

- `ngOnInit`：组件初始化时调用。
- `ngOnChanges`：输入属性变化时调用。
- `ngOnDestroy`：组件销毁时调用。

```typescript
import { Component, OnInit, OnDestroy } from '@angular/core';

@Component({
  selector: 'app-lifecycle',
  template: `<p>{{ message }}</p>`
})
export class LifecycleComponent implements OnInit, OnDestroy {
  message = 'Component is initializing...';

  ngOnInit() {
    this.message = 'Component is initialized!';
  }

  ngOnDestroy() {
    console.log('Component is destroyed!');
  }
}
```

## 9. 组件通信

组件之间可以通过 `@Input` 和 `@Output` 进行通信。

### 9.1 输入属性 (`@Input`)

父组件向子组件传递数据。

```typescript
// parent.component.ts
import { Component } from '@angular/core';

@Component({
  selector: 'app-parent',
  template: `<app-child [message]="parentMessage"></app-child>`
})
export class ParentComponent {
  parentMessage = 'Hello from Parent!';
}
```

```typescript
// child.component.ts
import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-child',
  template: `<p>{{ message }}</p>`
})
export class ChildComponent {
  @Input() message: string;
}
```

### 9.2 输出属性 (`@Output`)

子组件向父组件发送事件。

```typescript
// child.component.ts
import { Component, Output, EventEmitter } from '@angular/core';

@Component({
  selector: 'app-child',
  template: `<button (click)="sendMessage()">Send Message</button>`
})
export class ChildComponent {
  @Output() messageEvent = new EventEmitter<string>();

  sendMessage() {
    this.messageEvent.emit('Hello from Child!');
  }
}
```

```typescript
// parent.component.ts
import { Component } from '@angular/core';

@Component({
  selector: 'app-parent',
  template: `<app-child (messageEvent)="receiveMessage($event)"></app-child>`
})
export class ParentComponent {
  receiveMessage(message: string) {
    console.log(message);
  }
}
```

## 10. 内置指令

Angular 提供了一些内置指令，用于操作 DOM 元素。

### 10.1 `ngIf`

根据条件显示或隐藏元素。

```html
<p *ngIf="isVisible">This is visible!</p>
```

### 10.2 `ngFor`

循环渲染列表。

```html
<ul>
  <li *ngFor="let item of items">{{ item }}</li>
</ul>
```

### 10.3 `ngSwitch`

根据条件选择不同的模板。

```html
<div [ngSwitch]="condition">
  <p *ngSwitchCase="'A'">Condition A</p>
  <p *ngSwitchCase="'B'">Condition B</p>
  <p *ngSwitchDefault>Default Condition</p>
</div>
```

## 11. 属性指令

属性指令用于修改 DOM 元素的外观或行为。

### 11.1 创建自定义属性指令

```bash
# 创建一个名为 'highlight' 的属性指令
ng generate directive highlight
```

```typescript
// highlight.directive.ts
import { Directive, ElementRef, HostListener } from '@angular/core';

@Directive({
  selector: '[appHighlight]'
})
export class HighlightDirective {
  constructor(private el: ElementRef) {}

  @HostListener('mouseenter') onMouseEnter() {
    this.highlight('yellow');
  }

  @HostListener('mouseleave') onMouseLeave() {
    this.highlight(null);
  }

  private highlight(color: string) {
    this.el.nativeElement.style.backgroundColor = color;
  }
}
```

```html
<!-- 使用自定义属性指令 -->
<p appHighlight>Hover over me!</p>
```

## 12. 结构指令

结构指令用于修改 DOM 结构，如添加或移除元素。

### 12.1 创建自定义结构指令

```bash
# 创建一个名为 'unless' 的结构指令
ng generate directive unless
```

```typescript
// unless.directive.ts
import { Directive, Input, TemplateRef, ViewContainerRef } from '@angular/core';

@Directive({
  selector: '[appUnless]'
})
export class UnlessDirective {
  private hasView = false;

  constructor(
    private templateRef: TemplateRef<any>,
    private viewContainer: ViewContainerRef
  ) {}

  @Input() set appUnless(condition: boolean) {
    if (!condition && !this.hasView) {
      this.viewContainer.createEmbeddedView(this.templateRef);
      this.hasView = true;
    } else if (condition && this.hasView) {
      this.viewContainer.clear();
      this.hasView = false;
    }
  }
}
```

```html
<!-- 使用自定义结构指令 -->
<p *appUnless="condition">This is shown when condition is false.</p>
```

## 13. 自定义指令

自定义指令可以根据需求创建，用于扩展 Angular 的功能。

### 13.1 创建自定义指令

```bash
# 创建一个名为 'my-directive' 的自定义指令
ng generate directive my-directive
```

```typescript
// my-directive.directive.ts
import { Directive, ElementRef, Renderer2 } from '@angular/core';

@Directive({
  selector: '[appMyDirective]'
})
export class MyDirectiveDirective {
  constructor(private el: ElementRef, private renderer: Renderer2) {
    renderer.setStyle(el.nativeElement, 'color', 'red');
  }
}
```

```html
<!-- 使用自定义指令 -->
<p appMyDirective>This text is red!</p>
```

## 14. 服务创建和使用

服务是 Angular 中用于处理业务逻辑的类。它们通常用于数据获取、状态管理等。

### 14.1 创建服务

```bash
# 创建一个名为 'data' 的服务
ng generate service data
```

```typescript
// data.service.ts
import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class DataService {
  getData(): string {
    return 'Hello from DataService!';
  }
}
```

### 14.2 使用服务

```typescript
// app.component.ts
import { Component } from '@angular/core';
import { DataService } from './data.service';

@Component({
  selector: 'app-root',
  template: `<p>{{ message }}</p>`
})
export class AppComponent {
  message: string;

  constructor(private dataService: DataService) {
    this.message = this.dataService.getData();
  }
}
```

## 15. 依赖注入原理

依赖注入（DI）是 Angular 的核心特性之一，用于管理组件和服务之间的依赖关系。

### 15.1 提供者配置

在 Angular 中，服务可以通过 `@Injectable` 装饰器进行配置。

```typescript
// data.service.ts
import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class DataService {
  // ...
}
```

### 15.2 服务的作用域

服务的作用域可以通过 `providedIn` 属性进行配置。

- `'root'`：全局作用域，整个应用共享一个实例。
- `'any'`：每个模块共享一个实例。
- `'platform'`：平台级别的作用域。

## 16. 路由配置

Angular 提供了强大的路由功能，用于管理应用的导航。

### 16.1 配置路由

```typescript
// app-routing.module.ts
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { HomeComponent } from './home/home.component';
import { AboutComponent } from './about/about.component';

const routes: Routes = [
  { path: '', component: HomeComponent },
  { path: 'about', component: AboutComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
```

### 16.2 使用路由

```html
<!-- app.component.html -->
<nav>
  <a routerLink="/">Home</a>
  <a routerLink="/about">About</a>
</nav>
<router-outlet></router-outlet>
```

## 17. 路由参数

路由参数允许你在导航时传递数据。

### 17.1 配置带参数的路由

```typescript
// app-routing.module.ts
const routes: Routes = [
  { path: 'user/:id', component: UserComponent }
];
```

### 17.2 获取路由参数

```typescript
// user.component.ts
import { Component } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-user',
  template: `<p>User ID: {{ userId }}</p>`
})
export class UserComponent {
  userId: string;

  constructor(private route: ActivatedRoute) {
    this.userId = this.route.snapshot.paramMap.get('id');
  }
}
```

## 18. 子路由

子路由允许你在组件内部嵌套路由。

### 18.1 配置子路由

```typescript
// app-routing.module.ts
const routes: Routes = [
  { path: 'user/:id', component: UserComponent, children: [
    { path: 'profile', component: UserProfileComponent },
    { path: 'settings', component: UserSettingsComponent }
  ]}
];
```

### 18.2 使用子路由

```html
<!-- user.component.html -->
<nav>
  <a routerLink="profile">Profile</a>
  <a routerLink="settings">Settings</a>
</nav>
<router-outlet></router-outlet>
```

## 19. 路由守卫

路由守卫用于控制路由的访问权限。

### 19.1 创建守卫

```bash
# 创建一个名为 'auth' 的守卫
ng generate guard auth
```

```typescript
// auth.guard.ts
import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';

@Injectable({
  providedIn: 'root'
})