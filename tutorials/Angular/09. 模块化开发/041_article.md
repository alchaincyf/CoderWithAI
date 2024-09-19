---
title: 共享模块编程教程
date: 2023-10-05
description: 本课程详细讲解如何在编程中创建和使用共享模块，提高代码复用性和项目维护性。
slug: shared-modules-programming-tutorial
tags:
  - 模块化编程
  - 代码复用
  - 软件工程
category: 编程基础
keywords:
  - 共享模块
  - 模块化编程
  - 代码复用
---

# 共享模块

在Angular应用中，模块（NgModule）是组织代码的基本单位。共享模块（Shared Module）是一种特殊的模块，用于封装和共享常用的组件、指令、管道和服务。通过共享模块，我们可以避免代码重复，提高代码的可维护性和可重用性。

## 1. 什么是共享模块？

共享模块是一个Angular模块，通常包含一组常用的组件、指令、管道和服务。这些资源可以在多个特性模块（Feature Modules）中共享使用。共享模块的主要目的是减少代码重复，提高代码的可重用性。

### 1.1 为什么需要共享模块？

在大型Angular应用中，多个特性模块可能需要使用相同的组件、指令或管道。如果每个特性模块都单独定义这些资源，会导致代码重复，增加维护成本。通过将这些常用资源封装在共享模块中，并在需要时导入共享模块，可以有效减少代码重复，提高代码的可维护性。

## 2. 创建共享模块

### 2.1 使用Angular CLI创建共享模块

我们可以使用Angular CLI快速创建一个共享模块。打开终端并运行以下命令：

```bash
ng generate module shared
```

这将在`src/app`目录下生成一个名为`shared`的模块文件夹，并在其中创建一个`shared.module.ts`文件。

### 2.2 定义共享模块

打开`shared.module.ts`文件，定义共享模块的内容。通常，共享模块会包含以下内容：

- `declarations`：声明共享的组件、指令和管道。
- `exports`：导出共享的组件、指令和管道，以便其他模块可以使用。
- `imports`：导入其他模块或库，如Angular的内置模块或其他第三方库。

以下是一个简单的共享模块示例：

```typescript
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { SharedComponent } from './shared.component';
import { HighlightDirective } from './highlight.directive';
import { CapitalizePipe } from './capitalize.pipe';

@NgModule({
  declarations: [
    SharedComponent,
    HighlightDirective,
    CapitalizePipe
  ],
  imports: [
    CommonModule,
    FormsModule
  ],
  exports: [
    SharedComponent,
    HighlightDirective,
    CapitalizePipe
  ]
})
export class SharedModule { }
```

在这个示例中，我们定义了一个名为`SharedModule`的模块，并在其中声明了一个组件`SharedComponent`、一个指令`HighlightDirective`和一个管道`CapitalizePipe`。我们还导出了这些资源，以便其他模块可以使用它们。

## 3. 使用共享模块

### 3.1 在特性模块中导入共享模块

要在特性模块中使用共享模块中的组件、指令或管道，只需在特性模块的`imports`数组中导入共享模块即可。

例如，假设我们有一个名为`FeatureModule`的特性模块，我们可以在其中导入`SharedModule`：

```typescript
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FeatureComponent } from './feature.component';
import { SharedModule } from '../shared/shared.module';

@NgModule({
  declarations: [
    FeatureComponent
  ],
  imports: [
    CommonModule,
    SharedModule
  ]
})
export class FeatureModule { }
```

在这个示例中，`FeatureModule`导入了`SharedModule`，因此可以在`FeatureComponent`中使用`SharedModule`中导出的组件、指令和管道。

### 3.2 在组件中使用共享资源

在特性模块的组件中，可以直接使用共享模块中导出的组件、指令和管道。例如，在`FeatureComponent`中使用`SharedComponent`：

```html
<app-shared-component></app-shared-component>
```

## 4. 实践练习

### 4.1 创建一个共享模块

1. 使用Angular CLI创建一个名为`shared`的模块。
2. 在`shared`模块中创建一个组件`SharedComponent`、一个指令`HighlightDirective`和一个管道`CapitalizePipe`。
3. 在`shared.module.ts`中声明并导出这些资源。

### 4.2 在特性模块中使用共享模块

1. 创建一个名为`FeatureModule`的特性模块。
2. 在`FeatureModule`中导入`SharedModule`。
3. 在`FeatureModule`的组件中使用`SharedComponent`、`HighlightDirective`和`CapitalizePipe`。

### 4.3 验证共享模块的使用

1. 启动Angular应用，并导航到`FeatureModule`的组件。
2. 验证`SharedComponent`、`HighlightDirective`和`CapitalizePipe`是否正常工作。

## 5. 总结

共享模块是Angular应用中组织和重用代码的重要工具。通过将常用的组件、指令、管道和服务封装在共享模块中，并在需要时导入共享模块，可以有效减少代码重复，提高代码的可维护性和可重用性。希望本教程能帮助你更好地理解和使用Angular中的共享模块。