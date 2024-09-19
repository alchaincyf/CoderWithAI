---
title: 操作、减速器和选择器：深入理解与应用
date: 2023-10-05
description: 本课程深入探讨编程中的操作、减速器和选择器，帮助学员掌握这些核心概念及其在实际项目中的应用。
slug: operations-reducers-selectors
tags:
  - 编程基础
  - 数据处理
  - 函数式编程
category: 编程教程
keywords:
  - 操作
  - 减速器
  - 选择器
  - 函数式编程
  - 数据处理
---

# 操作、减速器和选择器

在 Angular 应用中，状态管理是一个非常重要的主题。为了有效地管理应用的状态，我们通常会使用状态管理库，如 NgRx。NgRx 提供了一套强大的工具来管理应用的状态，其中包括操作（Actions）、减速器（Reducers）和选择器（Selectors）。本教程将详细介绍这些概念，并通过代码示例和实践练习帮助你理解和掌握它们。

## 1. 操作（Actions）

### 1.1 理论解释

操作（Actions）是 NgRx 中的基本构建块之一。它们是描述应用中发生的各种事件的对象。每个操作都有一个类型（type）和一个可选的有效负载（payload）。操作的类型通常是一个字符串，用于标识该操作的唯一性。

### 1.2 代码示例

```typescript
import { createAction, props } from '@ngrx/store';

// 定义一个简单的操作
export const increment = createAction('[Counter] Increment');

// 定义一个带有有效负载的操作
export const addTodo = createAction(
  '[Todo] Add Todo',
  props<{ text: string }>()
);
```

### 1.3 实践练习

在你的 Angular 项目中，创建一个新的操作来表示用户登录事件。操作应该包含用户的用户名和密码。

```typescript
import { createAction, props } from '@ngrx/store';

export const login = createAction(
  '[Auth] Login',
  props<{ username: string, password: string }>()
);
```

## 2. 减速器（Reducers）

### 2.1 理论解释

减速器（Reducers）是纯函数，它们接收当前状态和一个操作，并返回一个新的状态。减速器的作用是根据操作来更新应用的状态。它们是不可变的，这意味着它们不会直接修改当前状态，而是返回一个新的状态对象。

### 2.2 代码示例

```typescript
import { createReducer, on } from '@ngrx/store';
import { increment, addTodo } from './actions';

export interface AppState {
  counter: number;
  todos: string[];
}

export const initialState: AppState = {
  counter: 0,
  todos: []
};

export const appReducer = createReducer(
  initialState,
  on(increment, (state) => ({ ...state, counter: state.counter + 1 })),
  on(addTodo, (state, { text }) => ({ ...state, todos: [...state.todos, text] }))
);
```

### 2.3 实践练习

在你的 Angular 项目中，创建一个新的减速器来处理用户登录操作。减速器应该将用户信息存储在状态中。

```typescript
import { createReducer, on } from '@ngrx/store';
import { login } from './actions';

export interface AuthState {
  username: string;
  password: string;
}

export const initialAuthState: AuthState = {
  username: '',
  password: ''
};

export const authReducer = createReducer(
  initialAuthState,
  on(login, (state, { username, password }) => ({ ...state, username, password }))
);
```

## 3. 选择器（Selectors）

### 3.1 理论解释

选择器（Selectors）是用于从状态中提取特定部分的函数。它们可以帮助你避免在组件中直接访问状态，从而提高代码的可维护性和可测试性。选择器是纯函数，它们接收状态作为参数，并返回状态的某个部分。

### 3.2 代码示例

```typescript
import { createSelector } from '@ngrx/store';

export interface AppState {
  counter: number;
  todos: string[];
}

export const selectCounter = (state: AppState) => state.counter;

export const selectTodos = (state: AppState) => state.todos;

export const selectTodoCount = createSelector(
  selectTodos,
  (todos) => todos.length
);
```

### 3.3 实践练习

在你的 Angular 项目中，创建一个新的选择器来从状态中提取用户信息。

```typescript
import { createSelector } from '@ngrx/store';

export interface AuthState {
  username: string;
  password: string;
}

export const selectAuthState = (state: { auth: AuthState }) => state.auth;

export const selectUsername = createSelector(
  selectAuthState,
  (auth) => auth.username
);

export const selectPassword = createSelector(
  selectAuthState,
  (auth) => auth.password
);
```

## 4. 总结

通过本教程，你应该已经掌握了 NgRx 中的操作、减速器和选择器的基本概念和使用方法。这些工具是构建复杂 Angular 应用的关键，能够帮助你有效地管理应用的状态。

### 4.1 下一步

接下来，你可以尝试将这些概念应用到你的 Angular 项目中，并探索更多高级的状态管理技术，如效果（Effects）和状态管理库（如 NGXS 和 Akita）。

### 4.2 参考资源

- [NgRx 官方文档](https://ngrx.io/)
- [Angular 官方文档](https://angular.io/)

希望本教程对你有所帮助，祝你在 Angular 开发中取得成功！