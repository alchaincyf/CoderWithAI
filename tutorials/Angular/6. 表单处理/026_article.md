---
title: 表单验证：从基础到高级的全面指南
date: 2023-10-05
description: 本课程详细介绍了表单验证的基本概念、常用方法以及高级技巧，帮助开发者构建安全、用户友好的Web表单。
slug: form-validation-guide
tags:
  - 表单验证
  - 前端开发
  - JavaScript
category: Web开发
keywords:
  - 表单验证
  - 前端表单
  - JavaScript验证
---

# 表单验证

## 1. 概述

在现代 Web 应用中，表单是用户与应用交互的主要方式之一。为了确保用户输入的数据是有效的，表单验证是不可或缺的一部分。Angular 提供了强大的表单验证功能，支持模板驱动表单和响应式表单两种方式。本教程将详细介绍如何在 Angular 中进行表单验证。

## 2. 模板驱动表单验证

### 2.1 基本概念

模板驱动表单（Template-driven Forms）是一种基于模板的表单验证方式，适合简单的表单场景。在这种方式下，表单的验证逻辑主要写在模板文件中。

### 2.2 示例代码

```html
<!-- app.component.html -->
<form #myForm="ngForm" (ngSubmit)="onSubmit(myForm)">
  <label for="name">Name:</label>
  <input type="text" id="name" name="name" ngModel required>
  <div *ngIf="myForm.controls['name']?.invalid && (myForm.controls['name']?.dirty || myForm.controls['name']?.touched)">
    <div *ngIf="myForm.controls['name']?.errors?.['required']">
      Name is required.
    </div>
  </div>

  <button type="submit" [disabled]="myForm.invalid">Submit</button>
</form>
```

### 2.3 解释

- `ngModel`：用于双向数据绑定，同时也会将输入框注册到表单中。
- `required`：HTML5 的必填验证属性。
- `#myForm="ngForm"`：将表单绑定到一个模板引用变量 `myForm`。
- `(ngSubmit)="onSubmit(myForm)"`：表单提交时调用 `onSubmit` 方法，并传递表单对象。
- `[disabled]="myForm.invalid"`：禁用提交按钮，直到表单有效。

### 2.4 实践练习

1. 创建一个新的 Angular 项目。
2. 在 `app.component.html` 中添加上述表单代码。
3. 在 `app.component.ts` 中实现 `onSubmit` 方法。

## 3. 响应式表单验证

### 3.1 基本概念

响应式表单（Reactive Forms）是一种基于模型的表单验证方式，适合复杂的表单场景。在这种方式下，表单的验证逻辑主要写在组件类中。

### 3.2 示例代码

```typescript
// app.component.ts
import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  myForm: FormGroup;

  constructor(private fb: FormBuilder) {
    this.myForm = this.fb.group({
      name: ['', Validators.required]
    });
  }

  onSubmit() {
    if (this.myForm.valid) {
      console.log(this.myForm.value);
    }
  }
}
```

```html
<!-- app.component.html -->
<form [formGroup]="myForm" (ngSubmit)="onSubmit()">
  <label for="name">Name:</label>
  <input type="text" id="name" formControlName="name">
  <div *ngIf="myForm.get('name')?.invalid && (myForm.get('name')?.dirty || myForm.get('name')?.touched)">
    <div *ngIf="myForm.get('name')?.errors?.['required']">
      Name is required.
    </div>
  </div>

  <button type="submit" [disabled]="myForm.invalid">Submit</button>
</form>
```

### 3.3 解释

- `FormBuilder`：用于简化表单组的创建。
- `FormGroup`：表示表单的组，包含多个表单控件。
- `Validators.required`：必填验证器。
- `formControlName`：将输入框绑定到表单控件。
- `[formGroup]="myForm"`：将表单绑定到组件类中的表单组。

### 3.4 实践练习

1. 创建一个新的 Angular 项目。
2. 在 `app.component.ts` 中添加上述代码。
3. 在 `app.component.html` 中添加表单模板代码。

## 4. 自定义验证器

### 4.1 基本概念

有时内置的验证器无法满足需求，这时可以创建自定义验证器。自定义验证器可以是一个函数，也可以是一个指令。

### 4.2 示例代码

```typescript
// custom-validators.ts
import { AbstractControl, ValidationErrors } from '@angular/forms';

export function forbiddenNameValidator(nameRe: RegExp): (control: AbstractControl) => ValidationErrors | null {
  return (control: AbstractControl): ValidationErrors | null => {
    const forbidden = nameRe.test(control.value);
    return forbidden ? { forbiddenName: { value: control.value } } : null;
  };
}
```

```typescript
// app.component.ts
import { Component } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { forbiddenNameValidator } from './custom-validators';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  myForm: FormGroup;

  constructor(private fb: FormBuilder) {
    this.myForm = this.fb.group({
      name: ['', [Validators.required, forbiddenNameValidator(/admin/i)]]
    });
  }

  onSubmit() {
    if (this.myForm.valid) {
      console.log(this.myForm.value);
    }
  }
}
```

### 4.3 解释

- `forbiddenNameValidator`：自定义验证器函数，检查输入值是否包含禁止的名称。
- `Validators.required`：必填验证器。
- `forbiddenNameValidator(/admin/i)`：使用自定义验证器，检查输入值是否包含 "admin"。

### 4.4 实践练习

1. 创建一个新的 Angular 项目。
2. 在项目中创建 `custom-validators.ts` 文件，并添加自定义验证器代码。
3. 在 `app.component.ts` 中使用自定义验证器。

## 5. 动态表单

### 5.1 基本概念

动态表单允许根据数据动态生成表单控件。这在需要根据用户输入或外部数据动态调整表单结构时非常有用。

### 5.2 示例代码

```typescript
// app.component.ts
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, FormArray, Validators } from '@angular/forms';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit {
  myForm: FormGroup;

  constructor(private fb: FormBuilder) {}

  ngOnInit() {
    this.myForm = this.fb.group({
      emails: this.fb.array([
        this.fb.control('', [Validators.required, Validators.email])
      ])
    });
  }

  get emails() {
    return this.myForm.get('emails') as FormArray;
  }

  addEmail() {
    this.emails.push(this.fb.control('', [Validators.required, Validators.email]));
  }

  onSubmit() {
    if (this.myForm.valid) {
      console.log(this.myForm.value);
    }
  }
}
```

```html
<!-- app.component.html -->
<form [formGroup]="myForm" (ngSubmit)="onSubmit()">
  <div formArrayName="emails">
    <div *ngFor="let email of emails.controls; let i = index" [formGroupName]="i">
      <label for="email{{i}}">Email {{i + 1}}:</label>
      <input type="email" id="email{{i}}" formControlName="email">
      <div *ngIf="email.invalid && (email.dirty || email.touched)">
        <div *ngIf="email.errors?.['required']">
          Email is required.
        </div>
        <div *ngIf="email.errors?.['email']">
          Email is invalid.
        </div>
      </div>
    </div>
  </div>

  <button type="button" (click)="addEmail()">Add Email</button>
  <button type="submit" [disabled]="myForm.invalid">Submit</button>
</form>
```

### 5.3 解释

- `FormArray`：用于动态添加表单控件。
- `emails.push(this.fb.control('', [Validators.required, Validators.email]))`：动态添加新的电子邮件输入框。
- `formArrayName="emails"`：将表单数组绑定到模板。
- `[formGroupName]="i"`：将表单控件绑定到数组中的特定索引。

### 5.4 实践练习

1. 创建一个新的 Angular 项目。
2. 在 `app.component.ts` 中添加动态表单代码。
3. 在 `app.component.html` 中添加表单模板代码。

## 6. 总结

表单验证是 Angular 应用中非常重要的一部分。通过模板驱动表单和响应式表单，我们可以轻松地实现各种复杂的验证逻辑。自定义验证器和动态表单进一步扩展了表单验证的能力，使得我们可以应对各种复杂的业务需求。

通过本教程的学习，你应该能够掌握 Angular 中的表单验证技术，并能够在实际项目中应用这些知识。