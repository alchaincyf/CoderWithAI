---
title: 核心模块编程课程
date: 2023-10-05
description: 本课程深入探讨编程中的核心模块，涵盖模块化编程、代码重用和模块间通信等关键概念。
slug: core-module-programming-course
tags:
  - 编程基础
  - 模块化编程
  - 代码重用
category: 编程教程
keywords:
  - 核心模块
  - 模块化编程
  - 代码重用
---

# 核心模块

## 概述

在Angular应用中，模块（Module）是组织代码的基本单位。它们帮助我们将应用划分为不同的功能块，每个模块可以包含组件、服务、指令等。核心模块（Core Module）是一个特殊的模块，通常用于存放应用的全局服务和单例组件。

## 为什么需要核心模块？

核心模块的主要目的是减少根模块（AppModule）的复杂性，并将全局服务和单例组件集中管理。通过将这些内容移到核心模块中，我们可以使根模块更加简洁，同时也便于维护和扩展。

## 创建核心模块

### 1. 使用Angular CLI创建核心模块

首先，我们需要使用Angular CLI来创建一个新的模块。打开终端并运行以下命令：

```bash
ng generate module core
```

这将在`src/app`目录下生成一个名为`core`的文件夹，并在其中创建一个`core.module.ts`文件。

### 2. 定义核心模块

打开`core.module.ts`文件，并定义核心模块：

```typescript
import { NgModule, Optional, SkipSelf } from '@angular/core';
import { CommonModule } from '@angular/common';

@NgModule({
  declarations: [],
  imports: [
    CommonModule
  ],
  providers: []
})
export class CoreModule {
  constructor(@Optional() @SkipSelf() parentModule?: CoreModule) {
    if (parentModule) {
      throw new Error(
        'CoreModule is already loaded. Import it in the AppModule only.'
      );
    }
  }
}
```

### 3. 添加全局服务

假设我们有一个全局的服务`LoggerService`，我们可以将其添加到核心模块中：

```typescript
import { NgModule, Optional, SkipSelf } from '@angular/core';
import { CommonModule } from '@angular/common';
import { LoggerService } from './services/logger.service';

@NgModule({
  declarations: [],
  imports: [
    CommonModule
  ],
  providers: [LoggerService]
})
export class CoreModule {
  constructor(@Optional() @SkipSelf() parentModule?: CoreModule) {
    if (parentModule) {
      throw new Error(
        'CoreModule is already loaded. Import it in the AppModule only.'
      );
    }
  }
}
```

### 4. 在根模块中导入核心模块

最后，我们需要在根模块（`AppModule`）中导入核心模块：

```typescript
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { AppComponent } from './app.component';
import { CoreModule } from './core/core.module';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    CoreModule // 导入核心模块
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

## 实践练习

### 练习1：创建一个全局服务

1. 使用Angular CLI创建一个新的服务：

   ```bash
   ng generate service core/services/auth
   ```

2. 在`auth.service.ts`中实现一个简单的认证服务：

   ```typescript
   import { Injectable } from '@angular/core';

   @Injectable({
     providedIn: 'root'
   })
   export class AuthService {
     isAuthenticated(): boolean {
       // 这里可以实现实际的认证逻辑
       return true;
     }
   }
   ```

3. 将`AuthService`添加到核心模块的`providers`数组中。

### 练习2：使用核心模块中的服务

1. 在`AppComponent`中注入`AuthService`并使用它：

   ```typescript
   import { Component } from '@angular/core';
   import { AuthService } from './core/services/auth.service';

   @Component({
     selector: 'app-root',
     template: `
       <div *ngIf="isAuthenticated">Welcome!</div>
       <div *ngIf="!isAuthenticated">Please log in.</div>
     `
   })
   export class AppComponent {
     isAuthenticated: boolean;

     constructor(private authService: AuthService) {
       this.isAuthenticated = this.authService.isAuthenticated();
     }
   }
   ```

2. 运行应用并观察结果。

## 总结

核心模块是Angular应用中一个重要的组织结构，它帮助我们管理全局服务和单例组件，使应用更加模块化和易于维护。通过本教程，你应该已经掌握了如何创建和使用核心模块，并能够在实际项目中应用这些知识。

## 下一步

接下来，你可以继续学习Angular的其他高级主题，如路由配置、表单处理、状态管理等。这些主题将进一步帮助你构建复杂且功能强大的Angular应用。