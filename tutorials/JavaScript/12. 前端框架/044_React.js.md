---
title: React.js 入门教程
date: 2023-10-05
description: 本课程将带你从零开始学习React.js，掌握组件化开发、状态管理以及React生态系统中的关键工具。
slug: react-js-introduction
tags:
  - React.js
  - 前端开发
  - JavaScript
category: 前端开发
keywords:
  - React.js 入门
  - React 教程
  - 前端框架
---

# React.js 入门

## 1. 简介

React.js 是一个用于构建用户界面的 JavaScript 库。它由 Facebook 开发并维护，因其高效的组件化设计和虚拟 DOM 机制而广受欢迎。React 允许开发者将 UI 拆分为独立的、可重用的组件，从而简化复杂应用的开发和维护。

### 1.1 React 的历史

React 最初由 Jordan Walke 在 2011 年开发，并于 2013 年开源。自那时起，React 迅速成为前端开发中最受欢迎的库之一，被广泛应用于构建各种规模的 Web 应用。

### 1.2 React 的核心概念

- **组件化**: 将 UI 拆分为独立的组件，每个组件负责特定的功能。
- **虚拟 DOM**: 通过虚拟 DOM 提高渲染效率，减少直接操作真实 DOM 的开销。
- **单向数据流**: 数据从父组件流向子组件，确保数据流的清晰和可预测。

## 2. 开发环境设置

在开始 React 开发之前，我们需要设置一个合适的开发环境。

### 2.1 安装 Node.js

React 应用通常需要使用 Node.js 来管理依赖和运行开发服务器。你可以从 [Node.js 官网](https://nodejs.org/) 下载并安装 Node.js。

安装完成后，打开终端并运行以下命令来验证安装是否成功：

```bash
node -v
npm -v
```

### 2.2 创建 React 应用

使用 `create-react-app` 工具可以快速创建一个新的 React 项目。首先，全局安装 `create-react-app`：

```bash
npm install -g create-react-app
```

然后，创建一个新的 React 应用：

```bash
npx create-react-app my-react-app
cd my-react-app
npm start
```

这将启动一个开发服务器，并在浏览器中打开一个新的 React 应用。

## 3. 基本概念

### 3.1 组件

组件是 React 应用的基本构建块。一个组件可以是一个简单的按钮，也可以是一个复杂的页面。React 组件可以是函数组件或类组件。

#### 3.1.1 函数组件

函数组件是一个简单的 JavaScript 函数，返回一个 React 元素。

```jsx
function Welcome(props) {
  return <h1>Hello, {props.name}</h1>;
}
```

#### 3.1.2 类组件

类组件是一个继承自 `React.Component` 的 JavaScript 类，必须实现 `render` 方法。

```jsx
class Welcome extends React.Component {
  render() {
    return <h1>Hello, {this.props.name}</h1>;
  }
}
```

### 3.2 JSX

JSX 是一种 JavaScript 的语法扩展，允许你在 JavaScript 代码中编写类似 HTML 的标记。

```jsx
const element = <h1>Hello, world!</h1>;
```

### 3.3 状态和属性

- **状态 (State)**: 组件的内部数据，可以通过 `setState` 方法更新。
- **属性 (Props)**: 从父组件传递给子组件的数据。

```jsx
class Clock extends React.Component {
  constructor(props) {
    super(props);
    this.state = { date: new Date() };
  }

  componentDidMount() {
    this.timerID = setInterval(() => this.tick(), 1000);
  }

  componentWillUnmount() {
    clearInterval(this.timerID);
  }

  tick() {
    this.setState({
      date: new Date()
    });
  }

  render() {
    return (
      <div>
        <h1>Hello, world!</h1>
        <h2>It is {this.state.date.toLocaleTimeString()}.</h2>
      </div>
    );
  }
}
```

### 3.4 事件处理

React 中的事件处理与原生 DOM 事件处理类似，但有一些语法上的差异。

```jsx
function ActionButton() {
  function handleClick() {
    alert('Button clicked!');
  }

  return (
    <button onClick={handleClick}>
      Click me
    </button>
  );
}
```

## 4. 实践练习

### 4.1 创建一个简单的计数器

我们将创建一个简单的计数器应用，用户可以点击按钮增加或减少计数器的值。

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

### 4.2 创建一个简单的 Todo 列表

我们将创建一个简单的 Todo 列表应用，用户可以添加和删除任务。

```jsx
import React, { useState } from 'react';

function TodoList() {
  const [todos, setTodos] = useState([]);
  const [inputValue, setInputValue] = useState('');

  const addTodo = () => {
    if (inputValue.trim()) {
      setTodos([...todos, inputValue]);
      setInputValue('');
    }
  };

  const removeTodo = (index) => {
    const newTodos = todos.filter((_, i) => i !== index);
    setTodos(newTodos);
  };

  return (
    <div>
      <input
        type="text"
        value={inputValue}
        onChange={(e) => setInputValue(e.target.value)}
      />
      <button onClick={addTodo}>Add Todo</button>
      <ul>
        {todos.map((todo, index) => (
          <li key={index}>
            {todo}
            <button onClick={() => removeTodo(index)}>Remove</button>
          </li>
        ))}
      </ul>
    </div>
  );
}

export default TodoList;
```

## 5. 总结

通过本教程，你已经学习了 React.js 的基本概念和开发环境设置。你了解了组件、JSX、状态和属性、事件处理等核心概念，并通过实践练习创建了简单的计数器和 Todo 列表应用。

React 是一个功能强大且灵活的库，适合构建各种规模的 Web 应用。继续学习和实践，你将能够掌握更多高级的 React 技巧和最佳实践。

## 6. 下一步

- 学习 React 路由 (React Router) 以构建单页应用 (SPA)。
- 探索 React 的状态管理库，如 Redux 或 Context API。
- 学习如何使用 React 与后端 API 进行交互。
- 深入了解 React 的性能优化技巧。

希望本教程能帮助你顺利入门 React.js，并在未来的开发中取得成功！