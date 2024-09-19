---
title: 创建 React 项目 (Create React App with TypeScript)
date: 2023-10-05
description: 本课程将指导您如何使用 TypeScript 创建一个 React 项目，涵盖从项目初始化到部署的全过程。
slug: create-react-app-with-typescript
tags:
  - React
  - TypeScript
  - 前端开发
category: 前端开发
keywords:
  - React 项目
  - TypeScript
  - Create React App
---

# 创建 React 项目 (Create React App with TypeScript)

## 1. 概述

在本教程中，我们将学习如何使用 TypeScript 创建一个 React 项目。TypeScript 是 JavaScript 的超集，提供了静态类型检查，使得代码更加健壮和易于维护。我们将使用 `create-react-app` 工具来快速搭建一个 TypeScript 版本的 React 项目。

## 2. 准备工作

在开始之前，请确保你已经安装了以下工具：

- **Node.js**: 用于运行 JavaScript 代码。你可以从 [Node.js 官网](https://nodejs.org/) 下载并安装。
- **npm** 或 **yarn**: 用于管理项目依赖。npm 通常随 Node.js 一起安装，你也可以选择使用 yarn。

### 2.1 安装 Node.js 和 npm

如果你还没有安装 Node.js，请访问 [Node.js 官网](https://nodejs.org/) 下载并安装。安装完成后，打开终端并运行以下命令来验证安装是否成功：

```bash
node -v
npm -v
```

你应该会看到 Node.js 和 npm 的版本号。

### 2.2 安装 yarn（可选）

如果你更喜欢使用 yarn 而不是 npm，可以通过以下命令安装 yarn：

```bash
npm install -g yarn
```

安装完成后，运行以下命令来验证安装是否成功：

```bash
yarn -v
```

## 3. 创建 React 项目

我们将使用 `create-react-app` 工具来创建一个新的 React 项目，并选择 TypeScript 作为项目的语言。

### 3.1 使用 npx 创建项目

打开终端并运行以下命令来创建一个新的 React 项目：

```bash
npx create-react-app my-react-app --template typescript
```

这里的 `my-react-app` 是你的项目名称，你可以根据需要更改。`--template typescript` 选项告诉 `create-react-app` 使用 TypeScript 模板。

### 3.2 项目结构

创建完成后，进入项目目录并查看项目结构：

```bash
cd my-react-app
```

项目结构应该类似于以下内容：

```
my-react-app/
├── node_modules/
├── public/
├── src/
│   ├── App.css
│   ├── App.test.tsx
│   ├── App.tsx
│   ├── index.css
│   ├── index.tsx
│   ├── react-app-env.d.ts
│   ├── reportWebVitals.ts
│   └── setupTests.ts
├── .gitignore
├── package.json
├── README.md
├── tsconfig.json
└── yarn.lock / package-lock.json
```

### 3.3 运行项目

在项目目录中运行以下命令来启动开发服务器：

```bash
npm start
```

或者使用 yarn：

```bash
yarn start
```

开发服务器启动后，浏览器会自动打开并显示 React 的默认页面。

## 4. 项目配置

### 4.1 tsconfig.json

`tsconfig.json` 文件是 TypeScript 项目的配置文件。`create-react-app` 已经为我们生成了一个默认的 `tsconfig.json` 文件，通常不需要修改。如果你需要自定义配置，可以参考 [TypeScript 官方文档](https://www.typescriptlang.org/docs/handbook/tsconfig-json.html)。

### 4.2 添加 TypeScript 支持

如果你在创建项目时没有选择 TypeScript 模板，可以通过以下步骤手动添加 TypeScript 支持：

1. 安装 TypeScript 和相关依赖：

   ```bash
   npm install --save typescript @types/node @types/react @types/react-dom @types/jest
   ```

2. 将 `src` 目录下的 `.js` 文件重命名为 `.tsx` 或 `.ts`。

3. 创建 `tsconfig.json` 文件并添加以下内容：

   ```json
   {
     "compilerOptions": {
       "target": "es5",
       "lib": ["dom", "dom.iterable", "esnext"],
       "allowJs": true,
       "skipLibCheck": true,
       "esModuleInterop": true,
       "allowSyntheticDefaultImports": true,
       "strict": true,
       "forceConsistentCasingInFileNames": true,
       "noFallthroughCasesInSwitch": true,
       "module": "esnext",
       "moduleResolution": "node",
       "resolveJsonModule": true,
       "isolatedModules": true,
       "noEmit": true,
       "jsx": "react-jsx"
     },
     "include": ["src"]
   }
   ```

## 5. 编写第一个 TypeScript React 组件

### 5.1 创建一个简单的组件

在 `src` 目录下创建一个新的文件 `HelloWorld.tsx`，并添加以下代码：

```tsx
import React from 'react';

interface HelloWorldProps {
  name: string;
}

const HelloWorld: React.FC<HelloWorldProps> = ({ name }) => {
  return <div>Hello, {name}!</div>;
};

export default HelloWorld;
```

### 5.2 使用组件

在 `src/App.tsx` 文件中引入并使用 `HelloWorld` 组件：

```tsx
import React from 'react';
import HelloWorld from './HelloWorld';

const App: React.FC = () => {
  return (
    <div className="App">
      <header className="App-header">
        <HelloWorld name="TypeScript" />
      </header>
    </div>
  );
};

export default App;
```

### 5.3 运行项目

保存文件后，开发服务器会自动重新加载页面。你应该会看到页面显示 "Hello, TypeScript!"。

## 6. 实践练习

### 6.1 练习：创建一个计数器组件

1. 在 `src` 目录下创建一个新的文件 `Counter.tsx`。
2. 编写一个计数器组件，包含增加和减少按钮，并显示当前计数值。
3. 在 `App.tsx` 中引入并使用 `Counter` 组件。

### 6.2 练习：使用 useState 和 useEffect

1. 在 `Counter.tsx` 中使用 `useState` 和 `useEffect` Hooks。
2. 在 `useEffect` 中添加一个副作用，例如在控制台打印计数值的变化。

## 7. 总结

通过本教程，你已经学会了如何使用 TypeScript 创建一个 React 项目，并编写了一个简单的 React 组件。TypeScript 的静态类型检查可以帮助你减少错误并提高代码的可维护性。继续探索 TypeScript 和 React 的更多功能，你将能够构建更加复杂和健壮的应用程序。

## 8. 进一步学习

- [TypeScript 官方文档](https://www.typescriptlang.org/docs/)
- [React 官方文档](https://reactjs.org/docs/getting-started.html)
- [Create React App 官方文档](https://create-react-app.dev/docs/getting-started)

希望本教程对你有所帮助，祝你在 TypeScript 和 React 的学习旅程中取得成功！