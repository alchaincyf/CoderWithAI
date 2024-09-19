---
title: React Native 简介
date: 2023-10-05
description: 本课程将介绍React Native的基础知识，帮助你快速入门移动应用开发。学习如何使用React Native构建跨平台应用。
slug: react-native-introduction
tags:
  - React Native
  - 移动开发
  - 跨平台开发
category: 编程教程
keywords:
  - React Native
  - 移动应用开发
  - 跨平台应用
---

# React Native 简介

## 概述

React Native 是一个由 Facebook 开发的开源框架，允许开发者使用 JavaScript 和 React 构建跨平台的移动应用程序。与传统的原生开发不同，React Native 提供了一种高效的方式来编写一次代码，然后在 iOS 和 Android 平台上运行。

## 为什么选择 React Native？

- **跨平台开发**：一次编写，多平台运行。
- **性能优越**：使用原生组件，性能接近原生应用。
- **热重载**：快速开发和调试，实时查看代码更改。
- **社区支持**：庞大的社区和丰富的第三方库。

## 环境搭建

### 安装 Node.js 和 npm

首先，确保你的系统上安装了 Node.js 和 npm。你可以通过以下命令检查是否已安装：

```bash
node -v
npm -v
```

如果没有安装，可以从 [Node.js 官网](https://nodejs.org/) 下载并安装。

### 安装 React Native CLI

使用 npm 安装 React Native CLI：

```bash
npm install -g react-native-cli
```

### 创建新项目

使用 React Native CLI 创建一个新的项目：

```bash
npx react-native init MyFirstApp
```

### 运行项目

进入项目目录并启动开发服务器：

```bash
cd MyFirstApp
npx react-native run-android
# 或者
npx react-native run-ios
```

## JSX 语法

JSX 是 JavaScript 的语法扩展，允许你在 JavaScript 中编写类似 HTML 的代码。

```jsx
import React from 'react';
import { Text, View } from 'react-native';

const App = () => {
  return (
    <View>
      <Text>Hello, React Native!</Text>
    </View>
  );
};

export default App;
```

## 组件基础

### 函数组件

函数组件是 React 中最简单的组件形式，使用函数定义。

```jsx
import React from 'react';
import { Text, View } from 'react-native';

const Greeting = () => {
  return (
    <View>
      <Text>Hello, World!</Text>
    </View>
  );
};

export default Greeting;
```

### 类组件

类组件使用 ES6 类定义，通常用于需要状态管理的情况。

```jsx
import React, { Component } from 'react';
import { Text, View } from 'react-native';

class Greeting extends Component {
  render() {
    return (
      <View>
        <Text>Hello, World!</Text>
      </View>
    );
  }
}

export default Greeting;
```

## Props 和 State

### Props

Props 是组件的输入参数，用于传递数据。

```jsx
import React from 'react';
import { Text, View } from 'react-native';

const Greeting = (props) => {
  return (
    <View>
      <Text>Hello, {props.name}!</Text>
    </View>
  );
};

export default Greeting;
```

### State

State 是组件的内部数据，用于管理组件的状态。

```jsx
import React, { useState } from 'react';
import { Text, View, Button } from 'react-native';

const Counter = () => {
  const [count, setCount] = useState(0);

  return (
    <View>
      <Text>Count: {count}</Text>
      <Button title="Increment" onPress={() => setCount(count + 1)} />
    </View>
  );
};

export default Counter;
```

## 生命周期方法

React Native 组件的生命周期方法与 React 类似，包括 `componentDidMount`、`componentDidUpdate` 和 `componentWillUnmount` 等。

```jsx
import React, { Component } from 'react';
import { Text, View } from 'react-native';

class LifecycleExample extends Component {
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
    return (
      <View>
        <Text>Lifecycle Example</Text>
      </View>
    );
  }
}

export default LifecycleExample;
```

## 事件处理

React Native 使用 `onPress`、`onChangeText` 等事件处理程序来响应用户交互。

```jsx
import React from 'react';
import { Text, View, Button } from 'react-native';

const EventHandler = () => {
  const handlePress = () => {
    alert('Button Pressed!');
  };

  return (
    <View>
      <Button title="Press Me" onPress={handlePress} />
    </View>
  );
};

export default EventHandler;
```

## 条件渲染

条件渲染允许你根据某些条件显示不同的内容。

```jsx
import React from 'react';
import { Text, View, Button } from 'react-native';

const ConditionalRendering = () => {
  const [isLoggedIn, setIsLoggedIn] = React.useState(false);

  return (
    <View>
      {isLoggedIn ? (
        <Text>Welcome back!</Text>
      ) : (
        <Button title="Login" onPress={() => setIsLoggedIn(true)} />
      )}
    </View>
  );
};

export default ConditionalRendering;
```

## 列表和 Keys

在 React Native 中，使用 `FlatList` 组件来渲染列表，并使用 `key` 属性来唯一标识每个列表项。

```jsx
import React from 'react';
import { FlatList, Text, View } from 'react-native';

const data = [
  { id: '1', title: 'Item 1' },
  { id: '2', title: 'Item 2' },
  { id: '3', title: 'Item 3' },
];

const ListExample = () => {
  return (
    <FlatList
      data={data}
      renderItem={({ item }) => (
        <View>
          <Text>{item.title}</Text>
        </View>
      )}
      keyExtractor={(item) => item.id}
    />
  );
};

export default ListExample;
```

## 表单处理

React Native 提供了多种表单组件，如 `TextInput`、`Picker` 等。

```jsx
import React, { useState } from 'react';
import { Text, View, TextInput, Button } from 'react-native';

const FormExample = () => {
  const [name, setName] = useState('');

  const handleSubmit = () => {
    alert(`Hello, ${name}!`);
  };

  return (
    <View>
      <TextInput
        placeholder="Enter your name"
        value={name}
        onChangeText={setName}
      />
      <Button title="Submit" onPress={handleSubmit} />
    </View>
  );
};

export default FormExample;
```

## 组合 vs 继承

在 React Native 中，推荐使用组合而不是继承来构建组件。

```jsx
import React from 'react';
import { Text, View } from 'react-native';

const Greeting = ({ name }) => {
  return (
    <View>
      <Text>Hello, {name}!</Text>
    </View>
  );
};

const App = () => {
  return (
    <View>
      <Greeting name="Alice" />
      <Greeting name="Bob" />
    </View>
  );
};

export default App;
```

## 高阶组件 (HOC)

高阶组件是一个函数，接受一个组件并返回一个新的组件。

```jsx
import React from 'react';
import { Text, View } from 'react-native';

const withUpperCase = (WrappedComponent) => {
  return (props) => {
    return <WrappedComponent {...props} text={props.text.toUpperCase()} />;
  };
};

const MyText = ({ text }) => {
  return (
    <View>
      <Text>{text}</Text>
    </View>
  );
};

const MyUpperCaseText = withUpperCase(MyText);

const App = () => {
  return (
    <View>
      <MyUpperCaseText text="hello, world!" />
    </View>
  );
};

export default App;
```

## Render Props

Render Props 是一种通过函数传递组件内容的技术。

```jsx
import React from 'react';
import { Text, View } from 'react-native';

const MouseTracker = ({ render }) => {
  const [position, setPosition] = React.useState({ x: 0, y: 0 });

  const handleMouseMove = (event) => {
    setPosition({ x: event.nativeEvent.pageX, y: event.nativeEvent.pageY });
  };

  return (
    <View onMouseMove={handleMouseMove}>
      {render(position)}
    </View>
  );
};

const App = () => {
  return (
    <MouseTracker
      render={(position) => (
        <Text>Mouse position: {position.x}, {position.y}</Text>
      )}
    />
  );
};

export default App;
```

## Context API

Context API 提供了一种在组件树中共享数据的方式，避免通过 props 逐层传递。

```jsx
import React, { createContext, useContext } from 'react';
import { Text, View } from 'react-native';

const ThemeContext = createContext('light');

const ThemedText = () => {
  const theme = useContext(ThemeContext);
  return (
    <View>
      <Text>Current theme: {theme}</Text>
    </View>
  );
};

const App = () => {
  return (
    <ThemeContext.Provider value="dark">
      <ThemedText />
    </ThemeContext.Provider>
  );
};

export default App;
```

## Refs 和 DOM

Refs 提供了一种访问 DOM 节点或 React 元素的方式。

```jsx
import React, { useRef } from 'react';
import { TextInput, Button, View } from 'react-native';

const RefExample = () => {
  const inputRef = useRef(null);

  const focusInput = () => {
    inputRef.current.focus();
  };

  return (
    <View>
      <TextInput ref={inputRef} />
      <Button title="Focus Input" onPress={focusInput} />
    </View>
  );
};

export default RefExample;
```

## useState 和 useEffect

`useState` 和 `useEffect` 是 React Hooks 中最常用的两个 Hook。

```jsx
import React, { useState, useEffect } from 'react';
import { Text, View } from 'react-native';

const Counter = () => {
  const [count, setCount] = useState(0);

  useEffect(() => {
    document.title = `Count: ${count}`;
  }, [count]);

  return (
    <View>
      <Text>Count: {count}</Text>
      <Button title="Increment" onPress={() => setCount(count + 1)} />
    </View>
  );
};

export default Counter;
```

## useContext

`useContext` 允许你在函数组件中使用 Context API。

```jsx
import React, { createContext, useContext } from 'react';
import { Text, View } from 'react-native';

const ThemeContext = createContext('light');

const ThemedText = () => {
  const theme = useContext(ThemeContext);
  return (
    <View>
      <Text>Current theme: {theme}</Text>
    </View>
  );
};

const App = () => {
  return (
    <ThemeContext.Provider value="dark">
      <ThemedText />
    </ThemeContext.Provider>
  );
};

export default App;
```

## useReducer

`useReducer` 是 `useState` 的替代方案，适用于复杂的 state 逻辑。

```jsx
import React, { useReducer } from 'react';
import { Text, View, Button } from 'react-native';

const initialState = { count: 0 };

const reducer = (state, action) => {
  switch (action.type) {
    case 'increment':
      return { count: state.count + 1 };
    case 'decrement':
      return { count: state.count - 1 };
    default:
      throw new Error();
  }
};

const Counter = () => {
  const [state, dispatch] = useReducer(reducer, initialState);

  return (
    <View>
      <Text>Count: {state.count}</Text>
      <Button title="Increment" onPress={() => dispatch({ type: 'increment' })} />
      <Button title="Decrement" onPress={() => dispatch({ type: 'decrement' })} />
    </View>
  );
};

export default Counter;
```

## 自定义 Hooks

自定义 Hooks 允许你提取组件逻辑并在多个组件中复用。

```jsx
import React, { useState, useEffect } from 'react';
import { Text, View } from 'react-native';

const useWindowSize = () => {
  const [size, setSize] = useState({ width: 0, height: 0 });

  useEffect(() => {
    const handleResize = () => {
      setSize({ width: window.innerWidth, height: window.innerHeight });
    };

    window.addEventListener('resize', handleResize);
    handleResize();

    return () => window.removeEventListener('resize', handleResize);
  }, []);

  return size;
};

const WindowSize = () => {
  const size = useWindowSize();

  return (
    <View>
      <Text>Window size: {size.width} x {size.height}</Text>
    </View>
  );
};

export default WindowSize;
```

## Hooks 规则和最佳实践

- **只在顶层调用 Hooks**：不要在循环、条件或嵌套函数中调用 Hooks。
- **只在函数组件中调用 Hooks**：不要在普通 JavaScript 函数中调用 Hooks。

## Redux 基础

Redux 是一个用于管理应用状态的库。

```jsx
import React from 'react';
import { createStore } from 'redux';
import { Provider, useSelector, useDispatch } from 'react-redux';
import { Text, View, Button } from 'react-native';

const initialState = { count: 0 };

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case 'increment':
      return { count: state.count + 1 };
    case 'decrement':
      return { count: state.count - 1 };
    default:
      return state;
  }
};

const store = createStore(reducer);

const Counter = () => {
  const count = useSelector((state) => state.count);
  const dispatch = useDispatch();

  return (
    <View>
      <Text>Count: {count}</Text>
      <Button title="Increment" onPress={() => dispatch({ type: 'increment' })} />
      <Button title="Decrement" onPress={() => dispatch({ type: 'decrement' })} />
    </View>
  );
};

const App = () => {
  return (
    <Provider store={store}>
      <Counter />
    </Provider>
  );
};

export default App;
```

## Redux Toolkit

Redux Toolkit 是 Redux 的官方工具集，简化了 Redux 的使用。

```jsx
import React from 'react';
import { configureStore, createSlice } from '@reduxjs/toolkit';
import { Provider, useSelector, useDispatch } from 'react-redux';
import { Text, View, Button } from 'react-native';

const counterSlice = createSlice({
  name: 'counter',
  initialState: { count: 0 },
  reducers: {
    increment: (state) => {
      state.count += 1;
    },
    decrement: (state) => {
      state.count -= 1;
    },
  },
});

const { increment, decrement } = counterSlice.actions;

const store = configureStore({
  reducer: counterSlice.reducer,
});

const