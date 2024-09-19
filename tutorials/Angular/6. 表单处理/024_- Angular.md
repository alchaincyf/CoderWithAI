---
title: 模板驱动表单教程 - 掌握Angular表单开发
date: 2023-10-05
description: 本课程详细讲解Angular中的模板驱动表单，帮助你掌握表单创建、验证和数据绑定的技巧。
slug: template-driven-forms-tutorial
tags:
  - Angular
  - 表单开发
  - 前端开发
category: 前端开发
keywords:
  - Angular表单
  - 模板驱动表单
  - 表单验证
---

# 模板驱动表单

## 1. 概述

在 Angular 中，表单是用户与应用程序交互的重要部分。Angular 提供了两种主要的表单处理方式：模板驱动表单（Template-Driven Forms）和响应式表单（Reactive Forms）。本教程将重点介绍模板驱动表单，这是一种基于模板语法的表单处理方式，适合初学者和简单的表单需求。

## 2. 模板驱动表单的基本概念

### 2.1 什么是模板驱动表单？

模板驱动表单是一种通过在 HTML 模板中直接使用 Angular 指令来处理表单数据的方式。它依赖于 Angular 的模板语法和双向数据绑定（Two-Way Data Binding）来实现表单的自动更新和验证。

### 2.2 主要特性

- **简单易用**：适合初学者，无需深入了解复杂的表单 API。
- **双向数据绑定**：通过 `ngModel` 指令实现表单控件与组件属性之间的双向数据绑定。
- **自动验证**：内置的验证指令（如 `required`、`minlength` 等）可以自动验证表单控件的输入。

## 3. 创建一个简单的模板驱动表单

### 3.1 环境准备

在开始之前，确保你已经安装了 Angular CLI，并且已经创建了一个新的 Angular 项目。如果还没有安装 Angular CLI，可以通过以下命令安装：

```bash
npm install -g @angular/cli
```

然后创建一个新的 Angular 项目：

```bash
ng new template-driven-forms-demo
cd template-driven-forms-demo
```

### 3.2 导入 `FormsModule`

要使用模板驱动表单，首先需要在应用的模块中导入 `FormsModule`。打开 `src/app/app.module.ts` 文件，并添加以下代码：

```typescript
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms'; // 导入 FormsModule

import { AppComponent } from './app.component';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    FormsModule // 添加 FormsModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

### 3.3 创建表单组件

接下来，我们创建一个简单的表单组件。打开 `src/app/app.component.html` 文件，并添加以下代码：

```html
<form #myForm="ngForm" (ngSubmit)="onSubmit(myForm.value)">
  <div>
    <label for="name">Name:</label>
    <input type="text" id="name" name="name" ngModel required>
  </div>
  <div>
    <label for="email">Email:</label>
    <input type="email" id="email" name="email" ngModel required email>
  </div>
  <button type="submit">Submit</button>
</form>
```

在 `src/app/app.component.ts` 文件中，添加以下代码：

```typescript
import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  onSubmit(formData: any) {
    console.log('Form Data:', formData);
  }
}
```

### 3.4 运行应用

现在，你可以运行应用并查看表单的效果：

```bash
ng serve
```

打开浏览器并访问 `http://localhost:4200`，你应该会看到一个简单的表单。当你填写表单并点击“Submit”按钮时，表单数据将被打印到控制台。

## 4. 表单验证

### 4.1 内置验证器

Angular 提供了一些内置的验证器，如 `required`、`minlength`、`maxlength`、`email` 等。在上面的示例中，我们已经使用了 `required` 和 `email` 验证器。

### 4.2 显示验证错误信息

你可以通过 Angular 的模板语法来显示验证错误信息。修改 `src/app/app.component.html` 文件，添加错误信息显示：

```html
<form #myForm="ngForm" (ngSubmit)="onSubmit(myForm.value)">
  <div>
    <label for="name">Name:</label>
    <input type="text" id="name" name="name" ngModel required>
    <div *ngIf="myForm.controls.name?.errors?.required">Name is required</div>
  </div>
  <div>
    <label for="email">Email:</label>
    <input type="email" id="email" name="email" ngModel required email>
    <div *ngIf="myForm.controls.email?.errors?.required">Email is required</div>
    <div *ngIf="myForm.controls.email?.errors?.email">Invalid email format</div>
  </div>
  <button type="submit" [disabled]="myForm.invalid">Submit</button>
</form>
```

### 4.3 自定义验证器

虽然 Angular 提供了许多内置验证器，但有时你可能需要自定义验证逻辑。你可以通过创建自定义指令来实现这一点。

## 5. 实践练习

### 5.1 练习：创建一个注册表单

创建一个包含以下字段的注册表单：

- **用户名**：必填，最小长度为 4 个字符。
- **密码**：必填，最小长度为 6 个字符。
- **确认密码**：必填，必须与密码字段匹配。
- **电子邮件**：必填，必须符合电子邮件格式。

### 5.2 练习：显示表单状态

在表单下方显示表单的状态（如 `valid`、`invalid`、`pristine`、`dirty` 等），并根据表单状态动态显示不同的消息。

## 6. 总结

模板驱动表单是 Angular 中处理表单的一种简单而强大的方式。它通过模板语法和双向数据绑定来实现表单的自动更新和验证。虽然它适合简单的表单需求，但对于复杂的表单逻辑，响应式表单可能更为合适。

通过本教程，你应该已经掌握了如何创建和验证模板驱动表单。继续探索 Angular 的其他特性，如响应式表单、服务、路由等，将帮助你构建更复杂的应用程序。

## 7. 下一步

- 学习响应式表单（Reactive Forms），了解如何通过代码控制表单。
- 深入了解 Angular 的路由配置和导航。
- 探索 Angular 的服务和依赖注入机制。

希望本教程对你有所帮助，祝你在 Angular 的学习旅程中取得成功！