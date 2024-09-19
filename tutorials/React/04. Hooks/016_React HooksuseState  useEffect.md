---
title: 掌握React Hooks：useState 和 useEffect 详解
date: 2023-10-05
description: 本课程深入讲解React中的useState和useEffect Hooks，帮助你理解如何在函数组件中管理状态和副作用。
slug: react-hooks-useState-useEffect
tags:
  - React
  - Hooks
  - JavaScript
category: 前端开发
keywords:
  - useState
  - useEffect
  - React Hooks
---

# useState 和 useEffect 教程

## 1. 概述

在 React 中，`useState` 和 `useEffect` 是两个最常用的 Hooks。它们分别用于管理组件的状态和处理副作用。理解这两个 Hooks 是掌握 React 函数组件的关键。

## 2. useState

### 2.1 什么是 useState？

`useState` 是一个 Hook，用于在函数组件中添加状态。它允许你在不使用类组件的情况下管理组件的状态。

### 2.2 基本用法

```jsx
import React, { useState } from 'react';

function Example() {
  // 声明一个名为 "count" 的状态变量，初始值为 0
  const [count, setCount] = useState(0);

  return (
    <div>
      <p>You clicked {count} times</p>
      <button onClick={() => setCount(count + 1)}>
        Click me
      </button>
    </div>
  );
}
```

### 2.3 解释

- `useState(0)`：初始化一个状态变量 `count`，初始值为 `0`。
- `const [count, setCount] = useState(0)`：`count` 是状态变量，`setCount` 是更新状态的函数。
- `setCount(count + 1)`：每次点击按钮时，调用 `setCount` 更新 `count` 的值。

### 2.4 实践练习

创建一个简单的计数器组件，用户可以点击按钮增加或减少计数器的值。

```jsx
import React, { useState } from 'react';

function Counter() {
  const [count, setCount] = useState(0);

  return (
    <div>
      <p>Count: {count}</p>
      <button onClick={() => setCount(count + 1)}>Increment</button>
      <button onClick={() => setCount(count - 1)}>Decrement</button>
    </div>
  );
}

export default Counter;
```

## 3. useEffect

### 3.1 什么是 useEffect？

`useEffect` 是一个 Hook，用于在函数组件中执行副作用操作。副作用包括数据获取、订阅、手动 DOM 操作等。

### 3.2 基本用法

```jsx
import React, { useState, useEffect } from 'react';

function Example() {
  const [count, setCount] = useState(0);

  // 类似于 componentDidMount 和 componentDidUpdate
  useEffect(() => {
    // 更新文档的标题
    document.title = `You clicked ${count} times`;
  });

  return (
    <div>
      <p>You clicked {count} times</p>
      <button onClick={() => setCount(count + 1)}>
        Click me
      </button>
    </div>
  );
}
```

### 3.3 解释

- `useEffect(() => { ... })`：每次组件渲染后都会执行这个函数。
- `document.title = `You clicked ${count} times``：更新文档的标题。

### 3.4 清理副作用

有时副作用需要清理，比如订阅和取消订阅。

```jsx
useEffect(() => {
  const subscription = props.source.subscribe();
  return () => {
    // 清理订阅
    subscription.unsubscribe();
  };
}, [props.source]);
```

### 3.5 实践练习

创建一个组件，当组件挂载时从 API 获取数据，并在组件卸载时清理资源。

```jsx
import React, { useState, useEffect } from 'react';

function DataFetcher() {
  const [data, setData] = useState(null);

  useEffect(() => {
    async function fetchData() {
      const response = await fetch('https://api.example.com/data');
      const result = await response.json();
      setData(result);
    }

    fetchData();

    return () => {
      // 清理操作
      console.log('Component unmounted');
    };
  }, []);

  return (
    <div>
      {data ? <p>{data.message}</p> : <p>Loading...</p>}
    </div>
  );
}

export default DataFetcher;
```

## 4. 总结

- `useState` 用于在函数组件中管理状态。
- `useEffect` 用于在函数组件中执行副作用操作。
- 通过实践练习，你可以更好地理解这两个 Hooks 的使用场景和方法。

## 5. 下一步

接下来，你可以学习 `useContext` 和 `useReducer`，进一步掌握 React 的状态管理。