---
title: NGXS 和 Akita 简介：现代状态管理库
date: 2023-10-05
description: 本课程将介绍如何使用NGXS和Akita这两个现代状态管理库来管理Angular应用程序中的状态。
slug: introduction-to-ngxs-and-akita
tags:
  - Angular
  - 状态管理
  - NGXS
  - Akita
category: 前端开发
keywords:
  - Angular状态管理
  - NGXS教程
  - Akita教程
  - 现代状态管理
---

# NGXS 和 Akita 简介

## 概述

在本教程中，我们将深入探讨 Angular 应用中的两种流行的状态管理库：NGXS 和 Akita。状态管理是现代前端开发中的一个关键概念，尤其是在构建大型、复杂的应用时。通过使用这些库，你可以更有效地管理应用的状态，提高代码的可维护性和可扩展性。

## 什么是状态管理？

在 Angular 应用中，状态管理指的是管理应用的数据和状态。状态可以是用户输入、API 响应、UI 状态等。随着应用的复杂性增加，手动管理这些状态变得困难。状态管理库通过提供一种集中式的方式来管理状态，使得状态的更新和访问更加可预测和易于维护。

## NGXS 简介

### 什么是 NGXS？

NGXS 是一个基于 Redux 设计模式的状态管理库，专门为 Angular 应用设计。它通过使用 TypeScript 的装饰器和类来简化状态管理。NGXS 的核心概念包括：

- **Store**: 存储应用的全局状态。
- **Actions**: 描述状态变化的意图。
- **State**: 定义状态的结构和初始值。
- **Selectors**: 从状态中选择数据。
- **Reducers**: 处理状态的更新逻辑。

### 安装 NGXS

首先，你需要安装 NGXS 库。你可以通过 npm 来安装：

```bash
npm install @ngxs/store --save
```

### 创建一个简单的 NGXS 应用

1. **定义状态**：

   创建一个状态类，定义状态的结构和初始值。

   ```typescript
   import { State, Action, StateContext } from '@ngxs/store';

   export class Increment {
     static readonly type = '[Counter] Increment';
   }

   @State<number>({
     name: 'counter',
     defaults: 0
   })
   export class CounterState {
     @Action(Increment)
     increment(ctx: StateContext<number>) {
       const state = ctx.getState();
       ctx.setState(state + 1);
     }
   }
   ```

2. **配置 Store**：

   在 `app.module.ts` 中配置 NGXS Store。

   ```typescript
   import { NgModule } from '@angular/core';
   import { NgxsModule } from '@ngxs/store';
   import { CounterState } from './counter.state';

   @NgModule({
     imports: [
       NgxsModule.forRoot([CounterState])
     ],
     // other configurations
   })
   export class AppModule {}
   ```

3. **使用 Store**：

   在你的组件中使用 Store 来分发动作和选择状态。

   ```typescript
   import { Component } from '@angular/core';
   import { Store, Select } from '@ngxs/store';
   import { Increment } from './counter.state';
   import { Observable } from 'rxjs';

   @Component({
     selector: 'app-counter',
     template: `
       <div>Counter: {{ counter$ | async }}</div>
       <button (click)="increment()">Increment</button>
     `
   })
   export class CounterComponent {
     @Select(state => state.counter) counter$: Observable<number>;

     constructor(private store: Store) {}

     increment() {
       this.store.dispatch(new Increment());
     }
   }
   ```

## Akita 简介

### 什么是 Akita？

Akita 是另一个流行的状态管理库，它借鉴了 Redux 和 Angular 的设计理念。Akita 强调简单性和可预测性，通过使用 TypeScript 的强类型和 RxJS 的响应式编程来管理状态。Akita 的核心概念包括：

- **Store**: 存储应用的状态。
- **Entity Store**: 存储实体（如用户、产品等）的状态。
- **Query**: 从 Store 中选择数据。
- **Entity Query**: 从 Entity Store 中选择数据。
- **Entity Service**: 处理实体的 CRUD 操作。

### 安装 Akita

你可以通过 npm 来安装 Akita：

```bash
npm install @datorama/akita --save
```

### 创建一个简单的 Akita 应用

1. **定义 Store**：

   创建一个 Store 类，定义状态的结构和初始值。

   ```typescript
   import { Store, StoreConfig } from '@datorama/akita';

   export interface CounterState {
     count: number;
   }

   @StoreConfig({ name: 'counter' })
   export class CounterStore extends Store<CounterState> {
     constructor() {
       super({ count: 0 });
     }
   }
   ```

2. **定义 Query**：

   创建一个 Query 类，从 Store 中选择数据。

   ```typescript
   import { Query } from '@datorama/akita';
   import { CounterStore, CounterState } from './counter.store';

   export class CounterQuery extends Query<CounterState> {
     constructor(protected store: CounterStore) {
       super(store);
     }

     selectCount$ = this.select(state => state.count);
   }
   ```

3. **使用 Store 和 Query**：

   在你的组件中使用 Store 和 Query 来管理状态。

   ```typescript
   import { Component } from '@angular/core';
   import { CounterStore } from './counter.store';
   import { CounterQuery } from './counter.query';
   import { Observable } from 'rxjs';

   @Component({
     selector: 'app-counter',
     template: `
       <div>Counter: {{ count$ | async }}</div>
       <button (click)="increment()">Increment</button>
     `
   })
   export class CounterComponent {
     count$: Observable<number>;

     constructor(private counterStore: CounterStore, private counterQuery: CounterQuery) {
       this.count$ = this.counterQuery.selectCount$;
     }

     increment() {
       this.counterStore.update(state => ({
         count: state.count + 1
       }));
     }
   }
   ```

## 实践练习

### 练习 1：使用 NGXS 实现一个简单的计数器应用

1. 创建一个新的 Angular 项目。
2. 安装 NGXS 库。
3. 定义一个状态类 `CounterState`，包含一个 `Increment` 动作。
4. 在 `app.module.ts` 中配置 NGXS Store。
5. 创建一个组件 `CounterComponent`，使用 Store 来分发 `Increment` 动作并显示计数器的值。

### 练习 2：使用 Akita 实现一个简单的计数器应用

1. 创建一个新的 Angular 项目。
2. 安装 Akita 库。
3. 定义一个 Store 类 `CounterStore`，包含一个 `count` 状态。
4. 定义一个 Query 类 `CounterQuery`，从 Store 中选择 `count` 状态。
5. 创建一个组件 `CounterComponent`，使用 Store 和 Query 来管理计数器的值。

## 总结

在本教程中，我们介绍了 NGXS 和 Akita 这两种流行的状态管理库，并通过简单的示例展示了它们的基本用法。状态管理是现代前端开发中的一个重要主题，掌握这些工具可以帮助你构建更复杂、更可维护的应用。希望你能通过本教程对 NGXS 和 Akita 有一个初步的了解，并在实际项目中应用这些知识。