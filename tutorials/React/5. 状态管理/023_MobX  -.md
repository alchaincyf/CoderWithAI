---
title: MobX 简介 - 掌握现代状态管理
date: 2023-10-05
description: 本课程将带你深入了解MobX，一个简单、可扩展的状态管理库，适用于React和其他JavaScript框架。学习如何高效地管理应用状态，提升开发效率。
slug: mobx-introduction
tags:
  - MobX
  - 状态管理
  - React
category: 前端开发
keywords:
  - MobX 简介
  - 状态管理
  - React状态管理
---

# MobX 简介

## 概述

MobX 是一个简单、可扩展的状态管理库，广泛用于 React 应用程序中。它通过透明的函数响应式编程（TFRP）使得状态管理变得简单和高效。MobX 的核心思想是通过观察者模式自动追踪状态的变化，并自动更新相关的视图。

## 核心概念

### 1. Observable State（可观察状态）

在 MobX 中，状态是可观察的。这意味着当状态发生变化时，所有依赖于该状态的视图都会自动更新。

```javascript
import { observable } from 'mobx';

const store = observable({
  count: 0,
});
```

### 2. Actions（动作）

动作是用来修改状态的函数。MobX 建议所有的状态修改都应该通过动作来进行，以确保状态的一致性和可追踪性。

```javascript
import { action } from 'mobx';

const increment = action(() => {
  store.count += 1;
});
```

### 3. Computed Values（计算值）

计算值是基于现有状态自动计算得出的值。它们是惰性的，只有在依赖的状态发生变化时才会重新计算。

```javascript
import { computed } from 'mobx';

const doubleCount = computed(() => {
  return store.count * 2;
});
```

### 4. Reactions（反应）

反应是当状态发生变化时自动执行的副作用。常见的反应包括更新视图或发送网络请求。

```javascript
import { reaction } from 'mobx';

reaction(
  () => store.count,
  (count) => {
    console.log(`Count changed to ${count}`);
  }
);
```

## 在 React 中使用 MobX

### 1. 安装 MobX 和 MobX React

首先，你需要安装 MobX 和 MobX React 库。

```bash
npm install mobx mobx-react
```

### 2. 创建 Store

创建一个 Store 来管理应用的状态。

```javascript
import { observable, action, computed } from 'mobx';

class CounterStore {
  @observable count = 0;

  @action increment() {
    this.count += 1;
  }

  @computed get doubleCount() {
    return this.count * 2;
  }
}

const counterStore = new CounterStore();
export default counterStore;
```

### 3. 使用 Provider 和 inject

在 React 应用中，使用 `Provider` 将 Store 注入到组件树中，并使用 `inject` 将 Store 注入到组件中。

```javascript
import React from 'react';
import ReactDOM from 'react-dom';
import { Provider, inject } from 'mobx-react';
import counterStore from './stores/CounterStore';

const Counter = inject('counterStore')(
  ({ counterStore }) => (
    <div>
      <p>Count: {counterStore.count}</p>
      <p>Double Count: {counterStore.doubleCount}</p>
      <button onClick={counterStore.increment}>Increment</button>
    </div>
  )
);

const App = () => (
  <Provider counterStore={counterStore}>
    <Counter />
  </Provider>
);

ReactDOM.render(<App />, document.getElementById('root'));
```

## 实践练习

### 练习 1：创建一个简单的计数器应用

1. 创建一个 `CounterStore`，包含 `count` 状态、`increment` 动作和 `doubleCount` 计算值。
2. 在 React 组件中使用 `CounterStore`，显示 `count` 和 `doubleCount`，并提供一个按钮来增加 `count`。

### 练习 2：创建一个 Todo 列表应用

1. 创建一个 `TodoStore`，包含 `todos` 状态、`addTodo` 动作和 `completedTodos` 计算值。
2. 在 React 组件中显示 `todos` 列表，并提供一个输入框和按钮来添加新的 `todo`。
3. 显示已完成 `todo` 的数量。

## 总结

MobX 提供了一种简单而强大的方式来管理 React 应用的状态。通过理解 Observable State、Actions、Computed Values 和 Reactions，你可以构建出高效且易于维护的应用。通过实践练习，你可以更好地掌握 MobX 的使用，并将其应用于实际项目中。

## 下一步

在掌握了 MobX 的基础之后，你可以继续学习更高级的主题，如：

- **Recoil 入门**：了解 Facebook 推出的另一种状态管理库。
- **状态管理最佳实践**：学习如何在大型应用中有效地管理状态。
- **React Router 基础**：掌握如何在 React 应用中进行路由管理。

通过不断学习和实践，你将能够构建出更加复杂和高效的 React 应用。