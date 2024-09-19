---
title: React 简介和特性
date: 2023-10-05
description: 本课程介绍React的基本概念和核心特性，包括组件化、虚拟DOM、状态管理和React生态系统。
slug: react-introduction-features
tags:
  - React
  - 前端开发
  - JavaScript
category: 前端开发
keywords:
  - React简介
  - React特性
  - 虚拟DOM
  - 组件化
  - React状态管理
---

# React 简介和特性

## 1. React 简介

React 是一个用于构建用户界面的 JavaScript 库。它由 Facebook 开发并维护，广泛应用于构建单页应用（SPA）和移动应用。React 的核心思想是通过组件化的方式来构建用户界面，使得代码更易于维护和扩展。

### 1.1 React 的主要特性

- **组件化**：React 将用户界面拆分为多个独立的组件，每个组件负责特定的功能。
- **虚拟 DOM**：React 使用虚拟 DOM 来提高性能，减少对实际 DOM 的操作。
- **声明式编程**：React 允许开发者以声明式的方式描述 UI，使得代码更直观和易于理解。
- **单向数据流**：React 遵循单向数据流的原则，使得数据流动更加可预测和易于调试。

## 2. 环境搭建

在开始编写 React 应用之前，我们需要搭建开发环境。以下是搭建环境的步骤：

### 2.1 安装 Node.js 和 npm

Node.js 是一个基于 Chrome V8 引擎的 JavaScript 运行时，npm 是 Node.js 的包管理工具。

1. 访问 [Node.js 官网](https://nodejs.org/) 下载并安装 Node.js。
2. 安装完成后，打开终端并运行以下命令，验证安装是否成功：

   ```bash
   node -v
   npm -v
   ```

   如果安装成功，会显示 Node.js 和 npm 的版本号。

### 2.2 使用 Create React App 创建项目

Create React App 是一个官方推荐的工具，用于快速创建 React 项目。

1. 打开终端并运行以下命令，创建一个新的 React 项目：

   ```bash
   npx create-react-app my-react-app
   ```

   其中 `my-react-app` 是项目的名称，可以根据需要修改。

2. 进入项目目录：

   ```bash
   cd my-react-app
   ```

3. 启动开发服务器：

   ```bash
   npm start
   ```

   项目启动后，浏览器会自动打开 `http://localhost:3000`，显示 React 的欢迎页面。

## 3. JSX 语法

JSX 是 JavaScript 的语法扩展，允许在 JavaScript 代码中编写类似 HTML 的标记。

### 3.1 JSX 示例

```jsx
import React from 'react';

function App() {
  return (
    <div>
      <h1>Hello, React!</h1>
      <p>This is a JSX example.</p>
    </div>
  );
}

export default App;
```

### 3.2 JSX 的特点

- **嵌入表达式**：可以在 JSX 中嵌入 JavaScript 表达式，使用花括号 `{}` 包裹。
- **属性**：可以使用 HTML 属性，如 `className` 代替 `class`，`htmlFor` 代替 `for`。
- **条件渲染**：可以使用三元运算符或 `&&` 运算符进行条件渲染。

## 4. 组件基础

React 应用由多个组件组成，组件可以是函数组件或类组件。

### 4.1 函数组件

函数组件是一个简单的 JavaScript 函数，返回 JSX 元素。

```jsx
import React from 'react';

function Welcome(props) {
  return <h1>Hello, {props.name}</h1>;
}

export default Welcome;
```

### 4.2 类组件

类组件是一个继承自 `React.Component` 的 JavaScript 类，必须实现 `render` 方法。

```jsx
import React, { Component } from 'react';

class Welcome extends Component {
  render() {
    return <h1>Hello, {this.props.name}</h1>;
  }
}

export default Welcome;
```

## 5. Props 和 State

### 5.1 Props

Props 是组件的输入参数，用于从父组件向子组件传递数据。

```jsx
import React from 'react';
import Welcome from './Welcome';

function App() {
  return (
    <div>
      <Welcome name="Alice" />
      <Welcome name="Bob" />
    </div>
  );
}

export default App;
```

### 5.2 State

State 是组件的内部数据，用于管理组件的状态。

```jsx
import React, { Component } from 'react';

class Counter extends Component {
  constructor(props) {
    super(props);
    this.state = { count: 0 };
  }

  increment = () => {
    this.setState({ count: this.state.count + 1 });
  };

  render() {
    return (
      <div>
        <p>Count: {this.state.count}</p>
        <button onClick={this.increment}>Increment</button>
      </div>
    );
  }
}

export default Counter;
```

## 6. 生命周期方法

React 类组件有一系列生命周期方法，用于在组件的不同阶段执行代码。

### 6.1 常用生命周期方法

- `componentDidMount`：组件挂载后调用。
- `componentDidUpdate`：组件更新后调用。
- `componentWillUnmount`：组件卸载前调用。

```jsx
import React, { Component } from 'react';

class LifecycleDemo extends Component {
  componentDidMount() {
    console.log('Component mounted');
  }

  componentDidUpdate() {
    console.log('Component updated');
  }

  componentWillUnmount() {
    console.log('Component will unmount');
  }

  render() {
    return <div>Lifecycle Demo</div>;
  }
}

export default LifecycleDemo;
```

## 7. 事件处理

React 使用合成事件来处理用户交互。

```jsx
import React, { Component } from 'react';

class EventDemo extends Component {
  handleClick = () => {
    alert('Button clicked');
  };

  render() {
    return (
      <button onClick={this.handleClick}>Click me</button>
    );
  }
}

export default EventDemo;
```

## 8. 条件渲染

React 支持多种条件渲染方式，如三元运算符、`&&` 运算符等。

```jsx
import React from 'react';

function Greeting(props) {
  return (
    <div>
      {props.isLoggedIn ? (
        <h1>Welcome back!</h1>
      ) : (
        <h1>Please sign up.</h1>
      )}
    </div>
  );
}

export default Greeting;
```

## 9. 列表和 Keys

在 React 中，渲染列表时需要为每个列表项指定一个唯一的 `key`。

```jsx
import React from 'react';

function ListDemo(props) {
  const items = props.items.map((item) =>
    <li key={item.id}>{item.text}</li>
  );

  return (
    <ul>{items}</ul>
  );
}

export default ListDemo;
```

## 10. 表单处理

React 提供了受控组件和非受控组件两种方式来处理表单。

### 10.1 受控组件

受控组件通过 `state` 来管理表单数据。

```jsx
import React, { Component } from 'react';

class ControlledForm extends Component {
  constructor(props) {
    super(props);
    this.state = { value: '' };
  }

  handleChange = (event) => {
    this.setState({ value: event.target.value });
  };

  handleSubmit = (event) => {
    alert('A name was submitted: ' + this.state.value);
    event.preventDefault();
  };

  render() {
    return (
      <form onSubmit={this.handleSubmit}>
        <label>
          Name:
          <input type="text" value={this.state.value} onChange={this.handleChange} />
        </label>
        <input type="submit" value="Submit" />
      </form>
    );
  }
}

export default ControlledForm;
```

### 10.2 非受控组件

非受控组件通过 `ref` 来直接访问表单元素。

```jsx
import React, { Component } from 'react';

class UncontrolledForm extends Component {
  constructor(props) {
    super(props);
    this.input = React.createRef();
  }

  handleSubmit = (event) => {
    alert('A name was submitted: ' + this.input.current.value);
    event.preventDefault();
  };

  render() {
    return (
      <form onSubmit={this.handleSubmit}>
        <label>
          Name:
          <input type="text" ref={this.input} />
        </label>
        <input type="submit" value="Submit" />
      </form>
    );
  }
}

export default UncontrolledForm;
```

## 11. 组合 vs 继承

React 推荐使用组合而非继承来实现组件的复用。

```jsx
import React from 'react';

function Dialog(props) {
  return (
    <div className="dialog">
      <h1>{props.title}</h1>
      <p>{props.message}</p>
      {props.children}
    </div>
  );
}

function WelcomeDialog() {
  return (
    <Dialog
      title="Welcome"
      message="Thank you for visiting our spacecraft!" />
  );
}

export default WelcomeDialog;
```

## 12. 高阶组件 (HOC)

高阶组件是一个函数，接收一个组件并返回一个新的组件。

```jsx
import React from 'react';

function withLoading(Component) {
  return function WithLoadingComponent({ isLoading, ...props }) {
    if (!isLoading) return <Component {...props} />;
    return <p>Loading...</p>;
  }
}

function Hello(props) {
  return <h1>Hello, {props.name}</h1>;
}

const HelloWithLoading = withLoading(Hello);

export default HelloWithLoading;
```

## 13. Render Props

Render Props 是一种通过函数 prop 来共享代码的技术。

```jsx
import React from 'react';

class MouseTracker extends React.Component {
  constructor(props) {
    super(props);
    this.state = { x: 0, y: 0 };
  }

  handleMouseMove = (event) => {
    this.setState({
      x: event.clientX,
      y: event.clientY
    });
  };

  render() {
    return (
      <div style={{ height: '100vh' }} onMouseMove={this.handleMouseMove}>
        {this.props.render(this.state)}
      </div>
    );
  }
}

function App() {
  return (
    <MouseTracker render={({ x, y }) => (
      <h1>The mouse position is ({x}, {y})</h1>
    )} />
  );
}

export default App;
```

## 14. Context API

Context API 提供了一种在组件树中共享数据的方式，避免通过 props 层层传递。

```jsx
import React, { createContext, useContext } from 'react';

const ThemeContext = createContext('light');

function ThemedButton() {
  const theme = useContext(ThemeContext);
  return (
    <button style={{ background: theme === 'light' ? 'white' : 'black', color: theme === 'light' ? 'black' : 'white' }}>
      I am styled by theme context!
    </button>
  );
}

function Toolbar() {
  return (
    <div>
      <ThemedButton />
    </div>
  );
}

function App() {
  return (
    <ThemeContext.Provider value="dark">
      <Toolbar />
    </ThemeContext.Provider>
  );
}

export default App;
```

## 15. Refs 和 DOM

Refs 提供了一种访问 DOM 节点或 React 元素的方式。

```jsx
import React, { useRef } from 'react';

function TextInputWithFocusButton() {
  const inputEl = useRef(null);
  const onButtonClick = () => {
    inputEl.current.focus();
  };

  return (
    <>
      <input ref={inputEl} type="text" />
      <button onClick={onButtonClick}>Focus the input</button>
    </>
  );
}

export default TextInputWithFocusButton;
```

## 16. useState 和 useEffect

`useState` 和 `useEffect` 是 React 提供的两个最常用的 Hooks。

### 16.1 useState

`useState` 用于在函数组件中添加状态。

```jsx
import React, { useState } from 'react';

function Counter() {
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

export default Counter;
```

### 16.2 useEffect

`useEffect` 用于在函数组件中执行副作用操作。

```jsx
import React, { useState, useEffect } from 'react';

function Example() {
  const [count, setCount] = useState(0);

  useEffect(() => {
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

export default Example;
```

## 17. useContext

`useContext` 用于在函数组件中访问 Context。

```jsx
import React, { useContext } from 'react';

const ThemeContext = React.createContext('light');

function ThemedButton() {
  const theme = useContext(ThemeContext);
  return (
    <button style={{ background: theme === 'light' ? 'white' : 'black', color: theme === 'light' ? 'black' : 'white' }}>
      I am styled by theme context!
    </button>
  );
}

function App() {
  return (
    <ThemeContext.Provider value="dark">
      <ThemedButton />
    </ThemeContext.Provider>
  );
}

export default App;
```

## 18. useReducer

`useReducer` 是 `useState` 的替代方案，适用于复杂的状态逻辑。

```jsx
import React, { useReducer } from 'react';

const initialState = { count: 0 };

function reducer(state, action) {
  switch (action.type) {
    case 'increment':
      return { count: state.count + 1 };
    case 'decrement':
      return { count: state.count - 1 };
    default:
      throw new Error();
  }
}

function Counter() {
  const [state, dispatch] = useReducer(reducer, initialState);

  return (
    <>
      Count: {state.count}
      <button onClick={() => dispatch({ type: 'increment' })}>+</button>
      <button onClick={() => dispatch({ type: 'decrement' })}>-</button>
    </>
  );
}

export default Counter;
```

## 19. 自定义 Hooks

自定义 Hooks 允许你提取组件逻辑并在多个组件中复用。

```jsx
import { useState, useEffect } from 'react';

function useWindowSize() {
  const [size, setSize] = useState({ width: window.innerWidth, height: window.innerHeight });

  useEffect(() => {
    const handleResize = () => {
      setSize({ width: window.innerWidth, height: window.innerHeight });
    };

    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  return size;
}

function WindowSize() {
  const size = useWindowSize();

  return (
    <div>
      <p>Width: {size.width}</p>
      <p>Height: {size.height}</p>
    </div>
  );