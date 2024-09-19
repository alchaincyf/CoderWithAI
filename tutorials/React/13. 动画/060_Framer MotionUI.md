---
title: 使用Framer Motion创建动态UI动画
date: 2023-10-05
description: 本课程将教你如何使用Framer Motion库在React应用中创建流畅且响应式的UI动画。
slug: framer-motion-ui-animation
tags:
  - React
  - UI动画
  - Framer Motion
category: 前端开发
keywords:
  - Framer Motion
  - React动画
  - UI动画
---

# Framer Motion 教程

## 1. 简介

Framer Motion 是一个强大的动画库，专为 React 设计。它提供了简单易用的 API，使得在 React 应用中创建复杂的动画变得非常容易。本教程将带你从基础开始，逐步深入了解 Framer Motion 的各种功能和用法。

## 2. 环境搭建

在开始之前，确保你已经安装了 Node.js 和 npm。然后，使用 Create React App 创建一个新的 React 项目：

```bash
npx create-react-app framer-motion-demo
cd framer-motion-demo
```

接下来，安装 Framer Motion：

```bash
npm install framer-motion
```

## 3. 基础动画

### 3.1 引入 Framer Motion

在你的 React 组件中引入 Framer Motion：

```jsx
import { motion } from 'framer-motion';
```

### 3.2 简单的动画

让我们从一个简单的动画开始。假设我们有一个按钮，当点击时，它会放大并改变颜色。

```jsx
import React from 'react';
import { motion } from 'framer-motion';

function App() {
  return (
    <motion.button
      whileHover={{ scale: 1.1, backgroundColor: '#ff0000' }}
      whileTap={{ scale: 0.9 }}
    >
      Click Me
    </motion.button>
  );
}

export default App;
```

在这个例子中，`whileHover` 和 `whileTap` 是 Framer Motion 提供的两个属性，分别用于定义鼠标悬停和点击时的动画效果。

## 4. 进阶动画

### 4.1 过渡效果

Framer Motion 支持多种过渡效果，如 `spring`、`tween` 等。让我们来看一个使用 `spring` 过渡的例子：

```jsx
import React from 'react';
import { motion } from 'framer-motion';

function App() {
  return (
    <motion.div
      initial={{ opacity: 0, y: -100 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ type: 'spring', stiffness: 100 }}
    >
      Hello, Framer Motion!
    </motion.div>
  );
}

export default App;
```

在这个例子中，`initial` 定义了组件的初始状态，`animate` 定义了组件的目标状态，`transition` 定义了过渡效果。

### 4.2 关键帧动画

Framer Motion 还支持关键帧动画，允许你定义多个动画状态：

```jsx
import React from 'react';
import { motion } from 'framer-motion';

function App() {
  return (
    <motion.div
      animate={{
        scale: [1, 2, 2, 1, 1],
        rotate: [0, 0, 270, 270, 0],
        borderRadius: ["20%", "20%", "50%", "50%", "20%"],
      }}
      transition={{
        duration: 2,
        ease: "easeInOut",
        times: [0, 0.2, 0.5, 0.8, 1],
        repeat: Infinity,
        repeatDelay: 1
      }}
    >
      Animate Me!
    </motion.div>
  );
}

export default App;
```

在这个例子中，`animate` 属性定义了多个动画状态，`transition` 属性定义了动画的持续时间、缓动函数、时间点和重复次数。

## 5. 实践练习

### 5.1 练习：创建一个简单的卡片翻转动画

目标：创建一个卡片组件，当点击时，卡片会翻转并显示背面内容。

提示：使用 `motion.div` 和 `animate` 属性来实现翻转效果。

### 5.2 练习：创建一个带有过渡效果的导航栏

目标：创建一个导航栏，当鼠标悬停在某个导航项上时，该项会放大并改变颜色。

提示：使用 `whileHover` 和 `whileTap` 属性来实现悬停和点击效果。

## 6. 总结

Framer Motion 是一个功能强大且易于使用的动画库，适用于 React 应用。通过本教程，你已经学会了如何使用 Framer Motion 创建基础和进阶的动画效果。希望你能继续探索 Framer Motion 的更多功能，并在你的项目中应用这些知识。

## 7. 参考资源

- [Framer Motion 官方文档](https://www.framer.com/motion/)
- [React 官方文档](https://reactjs.org/)
- [Create React App 官方文档](https://create-react-app.dev/)

通过这些资源，你可以进一步深入学习和掌握 Framer Motion 和 React 的相关知识。