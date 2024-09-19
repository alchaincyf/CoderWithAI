---
title: 深入理解Angular中的结构指令
date: 2023-10-05
description: 本课程将详细介绍Angular中的结构指令，包括*ngIf、*ngFor和*ngSwitch的使用方法和最佳实践。
slug: angular-structural-directives
tags:
  - Angular
  - 结构指令
  - 前端开发
category: 前端开发
keywords:
  - Angular结构指令
  - *ngIf
  - *ngFor
  - *ngSwitch
---

# 结构指令

## 1. 什么是结构指令？

在 Angular 中，结构指令是一种特殊的指令，它们通过添加、删除或替换 DOM 元素来改变 DOM 的结构。常见的结构指令包括 `*ngIf`、`*ngFor` 和 `*ngSwitch`。这些指令允许你根据条件动态地控制页面的显示内容。

### 1.1 结构指令的语法

结构指令通常以星号（`*`）开头，例如 `*ngIf`。星号是一个语法糖，Angular 会将其转换为 `<ng-template>` 元素，从而实现 DOM 结构的动态变化。

```html
<div *ngIf="isVisible">这个元素是可见的</div>
```

在这个例子中，`*ngIf` 指令会根据 `isVisible` 变量的值来决定是否显示 `<div>` 元素。

## 2. 常见的结构指令

### 2.1 `*ngIf`

`*ngIf` 指令用于根据条件显示或隐藏元素。如果条件为 `true`，则元素会被添加到 DOM 中；如果条件为 `false`，则元素会被从 DOM 中移除。

```html
<div *ngIf="isLoggedIn">欢迎回来，用户！</div>
```

在这个例子中，只有当 `isLoggedIn` 为 `true` 时，才会显示欢迎信息。

### 2.2 `*ngFor`

`*ngFor` 指令用于循环遍历数组或集合，并为每个元素生成一个 DOM 元素。

```html
<ul>
  <li *ngFor="let item of items">{{ item }}</li>
</ul>
```

在这个例子中，`items` 数组中的每个元素都会生成一个 `<li>` 元素。

### 2.3 `*ngSwitch`

`*ngSwitch` 指令用于根据不同的条件显示不同的内容。它类似于 JavaScript 中的 `switch` 语句。

```html
<div [ngSwitch]="currentStatus">
  <p *ngSwitchCase="'active'">状态是活跃的</p>
  <p *ngSwitchCase="'inactive'">状态是非活跃的</p>
  <p *ngSwitchDefault>状态未知</p>
</div>
```

在这个例子中，根据 `currentStatus` 的值，会显示不同的 `<p>` 元素。

## 3. 自定义结构指令

除了内置的结构指令，你还可以创建自定义的结构指令。自定义结构指令可以帮助你实现更复杂的 DOM 操作。

### 3.1 创建自定义结构指令

要创建一个自定义结构指令，你需要使用 Angular CLI 生成一个新的指令，并实现 `TemplateRef` 和 `ViewContainerRef` 接口。

```bash
ng generate directive myStructuralDirective
```

在生成的指令文件中，你可以编写如下代码：

```typescript
import { Directive, Input, TemplateRef, ViewContainerRef } from '@angular/core';

@Directive({
  selector: '[myStructuralDirective]'
})
export class MyStructuralDirective {
  private hasView = false;

  constructor(
    private templateRef: TemplateRef<any>,
    private viewContainer: ViewContainerRef
  ) {}

  @Input() set myStructuralDirective(condition: boolean) {
    if (condition && !this.hasView) {
      this.viewContainer.createEmbeddedView(this.templateRef);
      this.hasView = true;
    } else if (!condition && this.hasView) {
      this.viewContainer.clear();
      this.hasView = false;
    }
  }
}
```

### 3.2 使用自定义结构指令

在你的模板中，你可以像使用内置指令一样使用自定义结构指令：

```html
<div *myStructuralDirective="showContent">这个内容是根据条件显示的</div>
```

在这个例子中，`showContent` 是一个布尔变量，控制 `<div>` 元素的显示和隐藏。

## 4. 实践练习

### 4.1 练习：使用 `*ngIf` 和 `*ngFor`

创建一个简单的 Angular 应用，显示一个用户列表。如果用户已登录，显示欢迎信息；如果用户未登录，显示登录按钮。

```typescript
// app.component.ts
import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
    <div *ngIf="isLoggedIn">欢迎回来，用户！</div>
    <button *ngIf="!isLoggedIn" (click)="login()">登录</button>

    <ul>
      <li *ngFor="let user of users">{{ user.name }}</li>
    </ul>
  `
})
export class AppComponent {
  isLoggedIn = false;
  users = [
    { name: 'Alice' },
    { name: 'Bob' },
    { name: 'Charlie' }
  ];

  login() {
    this.isLoggedIn = true;
  }
}
```

### 4.2 练习：自定义结构指令

创建一个自定义结构指令，根据条件显示或隐藏内容。在模板中使用该指令，并观察其效果。

```typescript
// my-structural-directive.directive.ts
import { Directive, Input, TemplateRef, ViewContainerRef } from '@angular/core';

@Directive({
  selector: '[appMyStructuralDirective]'
})
export class MyStructuralDirective {
  private hasView = false;

  constructor(
    private templateRef: TemplateRef<any>,
    private viewContainer: ViewContainerRef
  ) {}

  @Input() set appMyStructuralDirective(condition: boolean) {
    if (condition && !this.hasView) {
      this.viewContainer.createEmbeddedView(this.templateRef);
      this.hasView = true;
    } else if (!condition && this.hasView) {
      this.viewContainer.clear();
      this.hasView = false;
    }
  }
}
```

```html
<!-- app.component.html -->
<div *appMyStructuralDirective="showContent">这个内容是根据条件显示的</div>
<button (click)="toggleContent()">切换内容</button>
```

```typescript
// app.component.ts
import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html'
})
export class AppComponent {
  showContent = false;

  toggleContent() {
    this.showContent = !this.showContent;
  }
}
```

## 5. 总结

结构指令是 Angular 中非常强大的工具，它们允许你根据条件动态地控制 DOM 的结构。通过学习内置的结构指令（如 `*ngIf`、`*ngFor` 和 `*ngSwitch`）以及如何创建自定义结构指令，你可以更灵活地构建复杂的用户界面。

希望这篇教程能帮助你更好地理解和使用 Angular 中的结构指令。继续探索 Angular 的其他功能，你会发现更多有趣和强大的特性！