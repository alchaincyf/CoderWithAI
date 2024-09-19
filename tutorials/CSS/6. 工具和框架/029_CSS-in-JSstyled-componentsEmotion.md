---
title: 深入理解CSS-in-JS库：styled-components与Emotion
date: 2023-10-05
description: 本课程将深入探讨CSS-in-JS的概念，重点介绍两个流行的库：styled-components和Emotion。学习如何使用这些工具在React应用中实现动态和模块化的样式。
slug: css-in-js-libraries
tags:
  - CSS-in-JS
  - styled-components
  - Emotion
category: 前端开发
keywords:
  - CSS-in-JS
  - styled-components
  - Emotion
  - React样式
---

# CSS-in-JS 库 (styled-components, Emotion) 教程

## 1. 简介

CSS-in-JS 是一种将 CSS 样式直接嵌入到 JavaScript 代码中的技术。它允许开发者使用 JavaScript 的强大功能来动态生成和应用样式，从而提高开发效率和代码的可维护性。本教程将详细介绍两个流行的 CSS-in-JS 库：`styled-components` 和 `Emotion`。

## 2. 为什么使用 CSS-in-JS？

### 2.1 优势

- **Scoped Styles**: 样式作用域仅限于组件内部，避免全局样式冲突。
- **动态样式**: 可以根据组件的状态动态生成样式。
- **代码复用**: 可以轻松地在组件之间共享样式。
- **类型安全**: 结合 TypeScript 可以实现类型安全的样式定义。

### 2.2 劣势

- **性能开销**: 动态生成样式可能会带来一定的性能开销。
- **学习曲线**: 需要熟悉 JavaScript 和 CSS 的结合使用。

## 3. styled-components

### 3.1 安装

首先，你需要安装 `styled-components` 库：

```bash
npm install styled-components
```

### 3.2 基本用法

`styled-components` 允许你使用模板字符串来定义样式，并将其应用于 React 组件。

```jsx
import React from 'react';
import styled from 'styled-components';

// 定义一个带样式的按钮
const StyledButton = styled.button`
  background-color: #4CAF50;
  border: none;
  color: white;
  padding: 15px 32px;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
`;

// 使用带样式的按钮
function App() {
  return (
    <div>
      <StyledButton>Click Me</StyledButton>
    </div>
  );
}

export default App;
```

### 3.3 动态样式

你可以通过传递 props 来动态改变样式：

```jsx
const StyledButton = styled.button`
  background-color: ${props => props.primary ? "#4CAF50" : "#f44336"};
  color: white;
  padding: 15px 32px;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
`;

function App() {
  return (
    <div>
      <StyledButton primary>Primary Button</StyledButton>
      <StyledButton>Secondary Button</StyledButton>
    </div>
  );
}
```

### 3.4 扩展样式

你可以通过扩展已有的样式组件来创建新的组件：

```jsx
const ExtendedButton = styled(StyledButton)`
  border: 2px solid black;
`;

function App() {
  return (
    <div>
      <ExtendedButton primary>Extended Button</ExtendedButton>
    </div>
  );
}
```

## 4. Emotion

### 4.1 安装

首先，你需要安装 `@emotion/react` 和 `@emotion/styled` 库：

```bash
npm install @emotion/react @emotion/styled
```

### 4.2 基本用法

`Emotion` 提供了类似于 `styled-components` 的 API，但也有一些独特的功能。

```jsx
import React from 'react';
import styled from '@emotion/styled';

const EmotionButton = styled.button`
  background-color: #4CAF50;
  border: none;
  color: white;
  padding: 15px 32px;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
`;

function App() {
  return (
    <div>
      <EmotionButton>Click Me</EmotionButton>
    </div>
  );
}

export default App;
```

### 4.3 动态样式

与 `styled-components` 类似，`Emotion` 也支持通过 props 动态改变样式：

```jsx
const EmotionButton = styled.button`
  background-color: ${props => props.primary ? "#4CAF50" : "#f44336"};
  color: white;
  padding: 15px 32px;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
`;

function App() {
  return (
    <div>
      <EmotionButton primary>Primary Button</EmotionButton>
      <EmotionButton>Secondary Button</EmotionButton>
    </div>
  );
}
```

### 4.4 使用 `css` 属性

`Emotion` 还提供了一个 `css` 属性，可以直接在组件上应用样式：

```jsx
import { css } from '@emotion/react';

const buttonStyle = css`
  background-color: #4CAF50;
  border: none;
  color: white;
  padding: 15px 32px;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
`;

function App() {
  return (
    <div>
      <button css={buttonStyle}>Click Me</button>
    </div>
  );
}

export default App;
```

## 5. 实践练习

### 5.1 练习目标

创建一个简单的 React 应用，包含以下功能：

- 一个带有动态样式的按钮组件。
- 一个扩展的按钮组件，具有不同的样式。

### 5.2 代码实现

```jsx
import React from 'react';
import styled from 'styled-components';

const StyledButton = styled.button`
  background-color: ${props => props.primary ? "#4CAF50" : "#f44336"};
  color: white;
  padding: 15px 32px;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
`;

const ExtendedButton = styled(StyledButton)`
  border: 2px solid black;
`;

function App() {
  return (
    <div>
      <StyledButton primary>Primary Button</StyledButton>
      <StyledButton>Secondary Button</StyledButton>
      <ExtendedButton primary>Extended Button</ExtendedButton>
    </div>
  );
}

export default App;
```

### 5.3 运行和测试

将上述代码保存为 `App.js`，并在你的 React 项目中运行。你应该能够看到三个按钮，每个按钮都有不同的样式。

## 6. 总结

CSS-in-JS 是一种强大的技术，它结合了 CSS 和 JavaScript 的优势，使得样式定义更加灵活和可维护。`styled-components` 和 `Emotion` 是两个流行的 CSS-in-JS 库，它们都提供了丰富的功能来帮助你构建现代化的 Web 应用。通过本教程的学习，你应该已经掌握了如何使用这两个库来创建动态和可扩展的样式组件。

## 7. 进一步学习

- **官方文档**: 阅读 `styled-components` 和 `Emotion` 的官方文档，了解更多高级功能和最佳实践。
- **社区资源**: 参与 CSS-in-JS 相关的社区讨论，学习其他开发者的经验和技巧。
- **性能优化**: 研究如何优化 CSS-in-JS 的性能，特别是在大型应用中。

希望本教程对你有所帮助，祝你在 CSS-in-JS 的学习和实践中取得成功！