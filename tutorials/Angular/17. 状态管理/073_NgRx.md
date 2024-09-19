---
title: NgRx 基础教程
date: 2023-10-05
description: 本课程将带你深入了解NgRx的基础知识，包括状态管理、动作、选择器和效果，帮助你构建高效、可维护的Angular应用。
slug: ngrx-fundamentals
tags:
  - Angular
  - NgRx
  - 状态管理
category: 前端开发
keywords:
  - NgRx
  - Angular状态管理
  - 前端框架
---

# NgRx 基础教程

## 1. 概述

NgRx 是一个用于 Angular 应用的状态管理库，它基于 Redux 的设计模式。NgRx 提供了一种集中式的方式来管理应用的状态，使得状态的变化更加可预测和可追踪。本教程将带你了解 NgRx 的基础知识，包括操作（Actions）、减速器（Reducers）、选择器（Selectors）和效果（Effects）。

## 2. 安装 NgRx

首先，我们需要在 Angular 项目中安装 NgRx。你可以通过以下命令来安装 NgRx：

```bash
npm install @ngrx/store @ngrx/effects @ngrx/entity @ngrx/store-devtools
```

## 3. 创建状态管理文件

在 Angular 项目中，我们通常会在 `src/app` 目录下创建一个新的文件夹 `store`，并在其中创建以下文件：

- `actions.ts`
- `reducer.ts`
- `selectors.ts`
- `effects.ts`

### 3.1 定义操作（Actions）

操作是 NgRx 中用于描述状态变化的指令。每个操作都有一个类型（type）和一个可选的有效载荷（payload）。

```typescript
// src/app/store/actions.ts
import { createAction, props } from '@ngrx/store';

export const increment = createAction('[Counter] Increment');
export const decrement = createAction('[Counter] Decrement');
export const reset = createAction('[Counter] Reset');
```

### 3.2 定义减速器（Reducers）

减速器是纯函数，它接收当前状态和一个操作，并返回新的状态。

```typescript
// src/app/store/reducer.ts
import { createReducer, on } from '@ngrx/store';
import { increment, decrement, reset } from './actions';

export const initialState = 0;

const _counterReducer = createReducer(
  initialState,
  on(increment, (state) => state + 1),
  on(decrement, (state) => state - 1),
  on(reset, (state) => 0)
);

export function counterReducer(state, action) {
  return _counterReducer(state, action);
}
```

### 3.3 定义选择器（Selectors）

选择器用于从状态中提取特定的数据片段。

```typescript
// src/app/store/selectors.ts
import { createSelector } from '@ngrx/store';

export const selectCounter = (state: any) => state.counter;
```

### 3.4 定义效果（Effects）

效果用于处理副作用，例如异步操作。

```typescript
// src/app/store/effects.ts
import { Injectable } from '@angular/core';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { EMPTY } from 'rxjs';
import { map, mergeMap, catchError } from 'rxjs/operators';
import { increment, decrement } from './actions';

@Injectable()
export class CounterEffects {
  constructor(private actions$: Actions) {}

  increment$ = createEffect(() => this.actions$.pipe(
    ofType(increment),
    mergeMap(() => EMPTY)
  ));

  decrement$ = createEffect(() => this.actions$.pipe(
    ofType(decrement),
    mergeMap(() => EMPTY)
  ));
}
```

## 4. 在 Angular 应用中使用 NgRx

### 4.1 注册 Store 和 Effects

在 `AppModule` 中注册 Store 和 Effects：

```typescript
// src/app/app.module.ts
import { StoreModule } from '@ngrx/store';
import { EffectsModule } from '@ngrx/effects';
import { counterReducer } from './store/reducer';
import { CounterEffects } from './store/effects';

@NgModule({
  imports: [
    StoreModule.forRoot({ counter: counterReducer }),
    EffectsModule.forRoot([CounterEffects])
  ],
  // 其他模块和组件
})
export class AppModule {}
```

### 4.2 在组件中使用 Store

在组件中注入 Store，并使用它来分发操作和选择状态。

```typescript
// src/app/app.component.ts
import { Component } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { increment, decrement, reset } from './store/actions';
import { selectCounter } from './store/selectors';

@Component({
  selector: 'app-root',
  template: `
    <div>
      <h1>{{ counter$ | async }}</h1>
      <button (click)="increment()">Increment</button>
      <button (click)="decrement()">Decrement</button>
      <button (click)="reset()">Reset</button>
    </div>
  `
})
export class AppComponent {
  counter$ = this.store.pipe(select(selectCounter));

  constructor(private store: Store) {}

  increment() {
    this.store.dispatch(increment());
  }

  decrement() {
    this.store.dispatch(decrement());
  }

  reset() {
    this.store.dispatch(reset());
  }
}
```

## 5. 实践练习

### 5.1 练习目标

创建一个简单的计数器应用，使用 NgRx 来管理计数器的状态。

### 5.2 步骤

1. 创建一个新的 Angular 项目。
2. 安装 NgRx 依赖。
3. 创建 `actions.ts`、`reducer.ts`、`selectors.ts` 和 `effects.ts` 文件。
4. 在 `AppModule` 中注册 Store 和 Effects。
5. 在 `AppComponent` 中使用 Store 来分发操作和选择状态。

### 5.3 预期结果

你应该能够看到一个计数器，并且可以通过按钮来增加、减少和重置计数器的值。

## 6. 总结

通过本教程，你已经学习了 NgRx 的基础知识，包括操作、减速器、选择器和效果。NgRx 提供了一种强大的方式来管理 Angular 应用的状态，使得状态的变化更加可预测和可追踪。希望你能继续深入学习 NgRx 的高级特性，并在实际项目中应用这些知识。

## 7. 下一步

- 学习 NgRx 的高级特性，如实体（Entity）和元数据（Meta-Reducers）。
- 探索其他状态管理库，如 NGXS 和 Akita。
- 在实际项目中应用 NgRx，体验其强大的状态管理能力。

希望本教程对你有所帮助，祝你在 Angular 和 NgRx 的学习旅程中取得成功！