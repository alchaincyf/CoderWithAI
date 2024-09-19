---
title: 深入理解编程中的效果 (Effects)
date: 2023-10-05
description: 本课程将深入探讨编程中的效果（Effects）概念，包括如何在不同编程语言中实现和管理效果，以及效果在现代编程中的应用。
slug: understanding-programming-effects
tags:
  - 编程效果
  - 函数式编程
  - 副作用管理
category: 编程基础
keywords:
  - 编程效果
  - 副作用
  - 函数式编程
---

# Angular 效果 (Effects) 教程

## 概述

在 Angular 应用中，`Effects` 是 NgRx 状态管理库中的一个重要概念。它们用于处理与外部世界（如 API 调用、WebSocket 连接等）的交互，并将这些交互的结果反馈到应用的状态中。`Effects` 是纯函数，它们监听特定的动作（Actions），执行副作用（Side Effects），并根据结果分发新的动作。

## 理论解释

### 什么是副作用？

在编程中，副作用是指函数或操作对外部环境产生的影响。例如，修改全局变量、进行网络请求、读写文件等都是副作用。在 Angular 应用中，副作用通常包括与后端 API 的交互、本地存储的读写等。

### 为什么需要 Effects？

在 NgRx 中，Reducers 是纯函数，它们不应该包含任何副作用。为了处理这些副作用，NgRx 引入了 `Effects`。`Effects` 允许我们在不污染 Reducers 的情况下处理这些副作用，并确保状态管理的纯净性。

### Effects 的工作原理

1. **监听动作**：`Effects` 通过 `Actions` 流监听特定的动作。
2. **执行副作用**：当监听到特定动作时，`Effects` 执行相应的副作用操作（如 API 调用）。
3. **分发新动作**：副作用操作完成后，`Effects` 分发新的动作，这些动作通常用于更新应用的状态。

## 代码示例

### 安装 NgRx

首先，确保你已经安装了 NgRx 相关的包：

```bash
npm install @ngrx/store @ngrx/effects
```

### 创建一个简单的 Effect

假设我们有一个简单的应用，需要从 API 获取用户列表。我们将创建一个 `Effect` 来处理这个 API 调用。

#### 1. 定义动作

```typescript
// user.actions.ts
import { createAction, props } from '@ngrx/store';
import { User } from './user.model';

export const loadUsers = createAction('[User] Load Users');
export const loadUsersSuccess = createAction(
  '[User] Load Users Success',
  props<{ users: User[] }>()
);
export const loadUsersFailure = createAction(
  '[User] Load Users Failure',
  props<{ error: any }>()
);
```

#### 2. 创建 Effect

```typescript
// user.effects.ts
import { Injectable } from '@angular/core';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { of } from 'rxjs';
import { catchError, map, mergeMap } from 'rxjs/operators';
import { UserService } from './user.service';
import * as UserActions from './user.actions';

@Injectable()
export class UserEffects {
  constructor(private actions$: Actions, private userService: UserService) {}

  loadUsers$ = createEffect(() =>
    this.actions$.pipe(
      ofType(UserActions.loadUsers),
      mergeMap(() =>
        this.userService.getUsers().pipe(
          map(users => UserActions.loadUsersSuccess({ users })),
          catchError(error => of(UserActions.loadUsersFailure({ error })))
        )
      )
    )
  );
}
```

#### 3. 注册 Effect

在 `AppModule` 中注册 `Effects`：

```typescript
// app.module.ts
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { StoreModule } from '@ngrx/store';
import { EffectsModule } from '@ngrx/effects';
import { UserEffects } from './user.effects';
import { AppComponent } from './app.component';

@NgModule({
  declarations: [AppComponent],
  imports: [
    BrowserModule,
    StoreModule.forRoot({}),
    EffectsModule.forRoot([UserEffects])
  ],
  bootstrap: [AppComponent]
})
export class AppModule {}
```

### 实践练习

#### 练习 1：创建一个简单的 Effect

1. 创建一个新的 Angular 项目。
2. 安装 NgRx 相关的包。
3. 创建一个简单的 `Effect`，模拟从 API 获取数据并更新状态。

#### 练习 2：处理多个动作

1. 扩展你的 `Effect`，使其能够处理多个动作（如加载用户、加载产品等）。
2. 确保每个动作都有相应的成功和失败处理。

## 总结

`Effects` 是 NgRx 中处理副作用的重要工具。通过使用 `Effects`，我们可以在不污染 Reducers 的情况下处理与外部世界的交互，并确保状态管理的纯净性。通过本教程，你应该已经掌握了如何创建和使用 `Effects`，并能够在实际项目中应用这些知识。

## 进一步学习

- 深入学习 NgRx 的 `Actions`、`Reducers` 和 `Selectors`。
- 探索其他状态管理库，如 NGXS 和 Akita。
- 学习如何使用 `Effects` 处理更复杂的副作用，如 WebSocket 连接和本地存储操作。

希望本教程对你理解 Angular 中的 `Effects` 有所帮助！