---
title: 组件基础：函数组件与类组件
date: 2023-10-05
description: 本课程深入探讨React中的组件基础，重点介绍函数组件和类组件的创建、使用及区别。
slug: react-component-basics
tags:
  - React
  - 函数组件
  - 类组件
category: 前端开发
keywords:
  - React组件
  - 函数组件
  - 类组件
  - 前端开发
---

# 组件基础 (函数组件和类组件)

在 React 中，组件是构建用户界面的基本单元。React 提供了两种主要的组件类型：函数组件和类组件。理解这两种组件的工作原理和使用场景是掌握 React 的基础。

## 1. 函数组件

函数组件是 React 中最简单的组件形式。它是一个纯函数，接受 `props` 作为参数，并返回一个 React 元素（通常是 JSX）。

### 1.1 基本语法

```jsx
function Welcome(props) {
  return <h1>Hello, {props.name}</h1>;
}
```

### 1.2 使用示例

```jsx
import React from 'react';
import ReactDOM from 'react-dom';

function Welcome(props) {
  return <h1>Hello, {props.name}</h1>;
}

const element = <Welcome name="Sara" />;
ReactDOM.render(element, document.getElementById('root'));
```

### 1.3 实践练习

创建一个名为 `Greeting` 的函数组件，接受 `name` 和 `message` 作为 `props`，并显示一条问候信息。

```jsx
function Greeting(props) {
  return (
    <div>
      <h1>Hello, {props.name}!</h1>
      <p>{props.message}</p>
    </div>
  );
}

const element = <Greeting name="Alice" message="Welcome to our website!" />;
ReactDOM.render(element, document.getElementById('root'));
```

## 2. 类组件

类组件是 React 中另一种常见的组件形式。它是一个 ES6 类，继承自 `React.Component`，并实现 `render` 方法来返回 JSX。

### 2.1 基本语法

```jsx
class Welcome extends React.Component {
  render() {
    return <h1>Hello, {this.props.name}</h1>;
  }
}
```

### 2.2 使用示例

```jsx
import React from 'react';
import ReactDOM from 'react-dom';

class Welcome extends React.Component {
  render() {
    return <h1>Hello, {this.props.name}</h1>;
  }
}

const element = <Welcome name="Sara" />;
ReactDOM.render(element, document.getElementById('root'));
```

### 2.3 实践练习

创建一个名为 `Greeting` 的类组件，接受 `name` 和 `message` 作为 `props`，并显示一条问候信息。

```jsx
class Greeting extends React.Component {
  render() {
    return (
      <div>
        <h1>Hello, {this.props.name}!</h1>
        <p>{this.props.message}</p>
      </div>
    );
  }
}

const element = <Greeting name="Alice" message="Welcome to our website!" />;
ReactDOM.render(element, document.getElementById('root'));
```

## 3. 函数组件 vs 类组件

### 3.1 选择哪种组件？

- **函数组件**：适用于简单的 UI 逻辑，不需要状态管理和生命周期方法。React 16.8 引入了 Hooks，使得函数组件可以管理状态和副作用。
- **类组件**：适用于复杂的 UI 逻辑，需要状态管理和生命周期方法。

### 3.2 性能和简洁性

- **函数组件**：通常更简洁，代码量更少，更容易理解和维护。
- **类组件**：虽然功能更强大，但代码量可能更多，且需要理解 `this` 关键字的使用。

## 4. 总结

函数组件和类组件是 React 中构建用户界面的两种基本方式。函数组件简洁易懂，适合简单的 UI 逻辑；类组件功能强大，适合复杂的 UI 逻辑。随着 React Hooks 的引入，函数组件的功能得到了极大的扩展，使得它们在许多场景下成为首选。

## 5. 下一步

在掌握了函数组件和类组件的基础后，你可以继续学习 React 中的 `Props` 和 `State`，这是构建动态和交互式用户界面的关键概念。

---

通过本教程，你应该已经掌握了 React 中函数组件和类组件的基本概念和使用方法。接下来，你可以尝试构建一些简单的 React 应用，进一步巩固这些知识。