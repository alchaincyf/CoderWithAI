---
title: NgModule 详解 - Angular 模块化编程
date: 2023-10-05
description: 本课程详细讲解 Angular 中的 NgModule 概念，帮助开发者理解模块化编程的核心原理和实践应用。
slug: ngmodule-detailed-explanation
tags:
  - Angular
  - NgModule
  - 模块化编程
category: 前端开发
keywords:
  - Angular NgModule
  - 模块化编程
  - Angular 模块
---

# NgModule 详解

## 1. 概述

在 Angular 中，`NgModule` 是一个非常重要的概念。它用于组织应用的不同部分，如组件、服务、指令等，并将它们打包成一个可重用的单元。每个 Angular 应用至少有一个 `NgModule`，通常是根模块 `AppModule`。

## 2. NgModule 的基本结构

一个典型的 `NgModule` 定义如下：

```typescript
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { AppComponent } from './app.component';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

### 2.1 `declarations`

`declarations` 数组用于声明属于该模块的组件、指令和管道。这些声明的元素只能在当前模块中使用。

```typescript
declarations: [
  AppComponent,
  MyComponent,
  MyDirective,
  MyPipe
]
```

### 2.2 `imports`

`imports` 数组用于导入其他模块，以便当前模块可以使用这些模块中声明的组件、指令和管道。

```typescript
imports: [
  BrowserModule,
  FormsModule,
  HttpClientModule
]
```

### 2.3 `providers`

`providers` 数组用于注册服务。这些服务可以在整个应用中使用，除非它们被限定在某个模块或组件的作用域内。

```typescript
providers: [
  MyService,
  { provide: MyToken, useValue: 'Hello' }
]
```

### 2.4 `bootstrap`

`bootstrap` 数组用于指定应用的根组件。通常是 `AppComponent`。

```typescript
bootstrap: [AppComponent]
```

## 3. 特性模块

特性模块是用于组织应用功能的模块。它们可以包含一组相关的组件、服务和指令。特性模块可以被导入到根模块或其他特性模块中。

### 3.1 创建特性模块

```typescript
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FeatureComponent } from './feature.component';

@NgModule({
  declarations: [FeatureComponent],
  imports: [CommonModule],
  exports: [FeatureComponent]
})
export class FeatureModule { }
```

### 3.2 导入特性模块

在根模块中导入特性模块：

```typescript
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { AppComponent } from './app.component';
import { FeatureModule } from './feature/feature.module';

@NgModule({
  declarations: [AppComponent],
  imports: [BrowserModule, FeatureModule],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

## 4. 共享模块

共享模块用于存放那些在多个特性模块中都会用到的组件、指令和管道。共享模块通常不包含服务，因为服务应该在根模块中注册。

### 4.1 创建共享模块

```typescript
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedComponent } from './shared.component';

@NgModule({
  declarations: [SharedComponent],
  imports: [CommonModule],
  exports: [SharedComponent]
})
export class SharedModule { }
```

### 4.2 使用共享模块

在特性模块中导入共享模块：

```typescript
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FeatureComponent } from './feature.component';
import { SharedModule } from '../shared/shared.module';

@NgModule({
  declarations: [FeatureComponent],
  imports: [CommonModule, SharedModule],
  exports: [FeatureComponent]
})
export class FeatureModule { }
```

## 5. 核心模块

核心模块用于存放那些在整个应用中只应该实例化一次的组件和服务。通常，核心模块在根模块中导入一次，并且在应用的其他地方不会被再次导入。

### 5.1 创建核心模块

```typescript
import { NgModule, Optional, SkipSelf } from '@angular/core';
import { CommonModule } from '@angular/common';
import { CoreService } from './core.service';

@NgModule({
  providers: [CoreService]
})
export class CoreModule {
  constructor(@Optional() @SkipSelf() parentModule: CoreModule) {
    if (parentModule) {
      throw new Error('CoreModule is already loaded. Import it in the AppModule only.');
    }
  }
}
```

### 5.2 使用核心模块

在根模块中导入核心模块：

```typescript
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { AppComponent } from './app.component';
import { CoreModule } from './core/core.module';

@NgModule({
  declarations: [AppComponent],
  imports: [BrowserModule, CoreModule],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

## 6. 模块懒加载

懒加载是一种优化技术，它允许在需要时才加载模块。这样可以减少应用的初始加载时间。

### 6.1 配置路由懒加载

```typescript
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

const routes: Routes = [
  { path: 'lazy', loadChildren: () => import('./lazy/lazy.module').then(m => m.LazyModule) }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
```

### 6.2 创建懒加载模块

```typescript
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from '@angular/router';
import { LazyComponent } from './lazy.component';

const routes: Routes = [
  { path: '', component: LazyComponent }
];

@NgModule({
  declarations: [LazyComponent],
  imports: [CommonModule, RouterModule.forChild(routes)]
})
export class LazyModule { }
```

## 7. 实践练习

### 7.1 创建一个特性模块

1. 创建一个新的特性模块 `UserModule`，包含一个组件 `UserComponent`。
2. 在 `UserModule` 中声明 `UserComponent`。
3. 将 `UserModule` 导入到根模块 `AppModule` 中。

### 7.2 创建一个共享模块

1. 创建一个新的共享模块 `SharedModule`，包含一个组件 `SharedComponent`。
2. 在 `SharedModule` 中声明 `SharedComponent`，并将其导出。
3. 在 `UserModule` 中导入 `SharedModule`。

### 7.3 创建一个核心模块

1. 创建一个新的核心模块 `CoreModule`，包含一个服务 `CoreService`。
2. 在 `CoreModule` 中注册 `CoreService`。
3. 在根模块 `AppModule` 中导入 `CoreModule`。

### 7.4 配置懒加载

1. 创建一个新的懒加载模块 `LazyModule`，包含一个组件 `LazyComponent`。
2. 在路由配置中配置懒加载 `LazyModule`。
3. 访问 `/lazy` 路径时，验证模块是否被懒加载。

## 8. 总结

通过本教程，我们详细介绍了 `NgModule` 的基本结构、特性模块、共享模块、核心模块以及懒加载的概念和实践。理解并正确使用 `NgModule` 是构建复杂 Angular 应用的关键。希望本教程能帮助你更好地掌握 Angular 模块化的开发方式。