---
title: React Router 基础教程
date: 2023-10-05
description: 本课程将带你深入了解React Router的基础知识，包括路由配置、导航、嵌套路由和动态路由等核心概念。
slug: react-router-basics
tags:
  - React
  - React Router
  - 前端开发
category: 前端开发
keywords:
  - React Router
  - 路由配置
  - 前端路由
---

# React Router 基础

## 概述

React Router 是 React 应用中最常用的路由库，它允许你在单页应用（SPA）中实现页面导航。通过 React Router，你可以轻松地管理不同 URL 路径与组件之间的映射关系，从而实现动态导航和页面切换。

## 安装 React Router

首先，你需要在你的 React 项目中安装 React Router。你可以使用 npm 或 yarn 来安装：

```bash
npm install react-router-dom
```

或者

```bash
yarn add react-router-dom
```

## 基本概念

### 1. BrowserRouter

`BrowserRouter` 是 React Router 的核心组件之一，它使用 HTML5 的 history API 来保持 UI 与 URL 的同步。通常，你会将整个应用包裹在 `BrowserRouter` 中。

```jsx
import { BrowserRouter } from 'react-router-dom';

function App() {
  return (
    <BrowserRouter>
      {/* 你的应用内容 */}
    </BrowserRouter>
  );
}
```

### 2. Route

`Route` 组件用于定义 URL 路径与组件之间的映射关系。你可以指定一个 `path` 属性和一个 `component` 属性，当 URL 匹配 `path` 时，对应的组件会被渲染。

```jsx
import { Route } from 'react-router-dom';
import Home from './Home';
import About from './About';

function App() {
  return (
    <BrowserRouter>
      <Route path="/" component={Home} />
      <Route path="/about" component={About} />
    </BrowserRouter>
  );
}
```

### 3. Link

`Link` 组件用于在应用内部进行导航，它类似于 HTML 中的 `<a>` 标签，但不会导致页面刷新。

```jsx
import { Link } from 'react-router-dom';

function Navigation() {
  return (
    <nav>
      <Link to="/">Home</Link>
      <Link to="/about">About</Link>
    </nav>
  );
}
```

### 4. Switch

`Switch` 组件用于确保只有一个 `Route` 会被渲染。如果没有 `Switch`，多个匹配的 `Route` 都会被渲染。

```jsx
import { Switch, Route } from 'react-router-dom';
import Home from './Home';
import About from './About';

function App() {
  return (
    <BrowserRouter>
      <Switch>
        <Route exact path="/" component={Home} />
        <Route path="/about" component={About} />
      </Switch>
    </BrowserRouter>
  );
}
```

## 实践练习

### 练习 1：创建一个简单的导航应用

1. 创建一个 React 应用。
2. 安装 `react-router-dom`。
3. 创建两个组件：`Home` 和 `About`。
4. 使用 `BrowserRouter`、`Route` 和 `Link` 组件来实现导航。

```jsx
// Home.js
import React from 'react';

function Home() {
  return <h1>Home Page</h1>;
}

export default Home;

// About.js
import React from 'react';

function About() {
  return <h1>About Page</h1>;
}

export default About;

// App.js
import React from 'react';
import { BrowserRouter, Switch, Route, Link } from 'react-router-dom';
import Home from './Home';
import About from './About';

function App() {
  return (
    <BrowserRouter>
      <nav>
        <Link to="/">Home</Link>
        <Link to="/about">About</Link>
      </nav>
      <Switch>
        <Route exact path="/" component={Home} />
        <Route path="/about" component={About} />
      </Switch>
    </BrowserRouter>
  );
}

export default App;
```

### 练习 2：动态路由

1. 创建一个 `User` 组件，用于显示用户信息。
2. 使用动态路由来显示不同用户的页面。

```jsx
// User.js
import React from 'react';

function User({ match }) {
  return <h1>User Profile: {match.params.id}</h1>;
}

export default User;

// App.js
import React from 'react';
import { BrowserRouter, Switch, Route, Link } from 'react-router-dom';
import Home from './Home';
import About from './About';
import User from './User';

function App() {
  return (
    <BrowserRouter>
      <nav>
        <Link to="/">Home</Link>
        <Link to="/about">About</Link>
        <Link to="/user/1">User 1</Link>
        <Link to="/user/2">User 2</Link>
      </nav>
      <Switch>
        <Route exact path="/" component={Home} />
        <Route path="/about" component={About} />
        <Route path="/user/:id" component={User} />
      </Switch>
    </BrowserRouter>
  );
}

export default App;
```

## 总结

通过本教程，你已经学习了 React Router 的基础知识，包括 `BrowserRouter`、`Route`、`Link` 和 `Switch` 组件的使用。你还通过实践练习创建了一个简单的导航应用和动态路由应用。继续深入学习 React Router，你将能够构建更复杂的单页应用。