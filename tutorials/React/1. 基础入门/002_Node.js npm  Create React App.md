---
title: 环境搭建：Node.js, npm, 和 Create React App
date: 2023-10-05
description: 本课程将指导您如何搭建Node.js、npm以及使用Create React App创建React应用的开发环境。
slug: environment-setup-nodejs-npm-create-react-app
tags:
  - Node.js
  - npm
  - React
category: 前端开发
keywords:
  - Node.js环境搭建
  - npm安装
  - Create React App
  - React开发环境
---

# 环境搭建 (Node.js, npm, Create React App)

## 1. 概述

在开始学习React之前，我们需要搭建一个适合开发React应用的环境。这个环境包括Node.js、npm（Node Package Manager）以及Create React App工具。本教程将详细介绍如何安装和配置这些工具，并创建你的第一个React应用。

## 2. 安装 Node.js 和 npm

### 2.1 什么是 Node.js 和 npm？

- **Node.js** 是一个基于Chrome V8引擎的JavaScript运行时环境，它允许你在服务器端运行JavaScript代码。
- **npm** 是Node.js的包管理器，用于安装和管理JavaScript库和工具。

### 2.2 安装 Node.js

1. **下载 Node.js**：访问 [Node.js官网](https://nodejs.org/)，下载适合你操作系统的安装包。
2. **安装 Node.js**：运行下载的安装包，按照提示完成安装。
3. **验证安装**：打开终端（Windows上是命令提示符或PowerShell，macOS和Linux上是终端），输入以下命令来验证Node.js和npm是否安装成功：

   ```bash
   node -v
   npm -v
   ```

   如果安装成功，你应该能看到Node.js和npm的版本号。

## 3. 使用 Create React App 创建项目

### 3.1 什么是 Create React App？

**Create React App** 是一个官方推荐的工具，用于快速创建React应用。它集成了Babel、Webpack等工具，并提供了一个零配置的开发环境。

### 3.2 创建 React 项目

1. **安装 Create React App**：如果你还没有安装Create React App，可以使用npm全局安装：

   ```bash
   npm install -g create-react-app
   ```

2. **创建新项目**：使用Create React App创建一个新的React项目：

   ```bash
   npx create-react-app my-first-react-app
   ```

   这里的 `my-first-react-app` 是你的项目名称，你可以根据需要更改。

3. **进入项目目录**：创建项目后，进入项目目录：

   ```bash
   cd my-first-react-app
   ```

4. **启动开发服务器**：使用以下命令启动开发服务器：

   ```bash
   npm start
   ```

   这将启动一个本地开发服务器，并在浏览器中打开你的React应用。默认情况下，应用会在 `http://localhost:3000` 上运行。

## 4. 项目结构

### 4.1 项目目录结构

Create React App 生成的项目结构如下：

```
my-first-react-app/
├── node_modules/
├── public/
│   ├── index.html
│   └── ...
├── src/
│   ├── App.css
│   ├── App.js
│   ├── App.test.js
│   ├── index.css
│   ├── index.js
│   ├── logo.svg
│   └── ...
├── .gitignore
├── package.json
├── README.md
└── ...
```

- **node_modules/**：包含项目依赖的所有库。
- **public/**：包含静态文件，如 `index.html`。
- **src/**：包含React应用的源代码。
- **package.json**：定义项目的依赖和脚本。

### 4.2 主要文件

- **public/index.html**：应用的入口HTML文件。
- **src/index.js**：React应用的入口文件，负责渲染应用到 `index.html` 中的 `div#root`。
- **src/App.js**：默认的React组件，通常是应用的主组件。

## 5. 实践练习

### 5.1 修改默认组件

1. 打开 `src/App.js` 文件。
2. 修改 `App` 组件的代码，例如：

   ```javascript
   import React from 'react';
   import './App.css';

   function App() {
     return (
       <div className="App">
         <header className="App-header">
           <h1>Hello, React!</h1>
           <p>This is my first React app.</p>
         </header>
       </div>
     );
   }

   export default App;
   ```

3. 保存文件后，浏览器会自动刷新，显示你修改后的内容。

### 5.2 添加新组件

1. 在 `src` 目录下创建一个新的文件 `MyComponent.js`。
2. 编写一个简单的组件：

   ```javascript
   import React from 'react';

   function MyComponent() {
     return <p>This is a new component!</p>;
   }

   export default MyComponent;
   ```

3. 在 `App.js` 中引入并使用这个新组件：

   ```javascript
   import React from 'react';
   import './App.css';
   import MyComponent from './MyComponent';

   function App() {
     return (
       <div className="App">
         <header className="App-header">
           <h1>Hello, React!</h1>
           <p>This is my first React app.</p>
           <MyComponent />
         </header>
       </div>
     );
   }

   export default App;
   ```

4. 保存文件后，浏览器会自动刷新，显示新添加的组件。

## 6. 总结

通过本教程，你已经成功搭建了React开发环境，并创建了你的第一个React应用。接下来，你可以继续学习React的其他特性，如JSX语法、组件基础、状态管理等。希望你能享受React开发的乐趣！

---

**下一步**：学习 [JSX 语法](https://reactjs.org/docs/introducing-jsx.html)，了解如何使用JSX编写React组件。