---
title: 掌握React Transition Group：动画与过渡的完美结合
date: 2023-10-05
description: 本课程深入讲解React Transition Group库，帮助你轻松实现React应用中的动画和过渡效果。
slug: mastering-react-transition-group
tags:
  - React
  - 动画
  - 前端开发
category: 前端开发
keywords:
  - React Transition Group
  - React动画
  - 前端动画
---

# React Transition Group 教程

## 概述

React Transition Group 是一个用于处理组件在不同状态之间转换的库。它提供了一种简单的方式来管理组件的进入和离开动画，使得开发者可以轻松地为应用添加动画效果。本教程将详细介绍 React Transition Group 的基本概念、使用方法以及一些最佳实践。

## 安装 React Transition Group

首先，我们需要安装 React Transition Group 库。你可以通过 npm 或 yarn 来安装：

```bash
npm install react-transition-group
```

或者

```bash
yarn add react-transition-group
```

## 核心组件

React Transition Group 提供了几个核心组件，用于处理不同的动画场景：

1. **Transition**：用于在组件进入和离开时应用动画。
2. **CSSTransition**：基于 CSS 类名的动画组件。
3. **TransitionGroup**：用于管理一组 Transition 或 CSSTransition 组件。

### Transition 组件

`Transition` 组件允许你在组件进入和离开时应用动画。它提供了几个生命周期钩子函数，如 `onEnter`、`onEntering`、`onEntered`、`onExit`、`onExiting` 和 `onExited`，你可以在这些钩子函数中定义动画逻辑。

```jsx
import React, { useState } from 'react';
import { Transition } from 'react-transition-group';

const duration = 300;

const defaultStyle = {
  transition: `opacity ${duration}ms ease-in-out`,
  opacity: 0,
};

const transitionStyles = {
  entering: { opacity: 1 },
  entered: { opacity: 1 },
  exiting: { opacity: 0 },
  exited: { opacity: 0 },
};

const Fade = ({ in: inProp }) => (
  <Transition in={inProp} timeout={duration}>
    {(state) => (
      <div style={{
        ...defaultStyle,
        ...transitionStyles[state]
      }}>
        I'm a fade Transition!
      </div>
    )}
  </Transition>
);

const App = () => {
  const [inProp, setInProp] = useState(false);
  return (
    <div>
      <button onClick={() => setInProp(!inProp)}>
        Click to Toggle
      </button>
      <Fade in={inProp} />
    </div>
  );
};

export default App;
```

### CSSTransition 组件

`CSSTransition` 组件是 `Transition` 组件的扩展，它基于 CSS 类名来应用动画。你可以通过 `classNames` 属性来指定 CSS 类名的前缀。

```jsx
import React, { useState } from 'react';
import { CSSTransition } from 'react-transition-group';
import './styles.css';

const App = () => {
  const [inProp, setInProp] = useState(false);
  return (
    <div>
      <button onClick={() => setInProp(!inProp)}>
        Click to Toggle
      </button>
      <CSSTransition
        in={inProp}
        timeout={300}
        classNames="fade"
        unmountOnExit
      >
        <div>I'm a fade CSSTransition!</div>
      </CSSTransition>
    </div>
  );
};

export default App;
```

对应的 CSS 文件 `styles.css`：

```css
.fade-enter {
  opacity: 0;
}
.fade-enter-active {
  opacity: 1;
  transition: opacity 300ms;
}
.fade-exit {
  opacity: 1;
}
.fade-exit-active {
  opacity: 0;
  transition: opacity 300ms;
}
```

### TransitionGroup 组件

`TransitionGroup` 组件用于管理一组 `Transition` 或 `CSSTransition` 组件。它可以帮助你处理列表中的元素的添加和删除动画。

```jsx
import React, { useState } from 'react';
import { TransitionGroup, CSSTransition } from 'react-transition-group';
import './styles.css';

const App = () => {
  const [items, setItems] = useState(['Item 1', 'Item 2']);

  const addItem = () => {
    const newItems = [...items, `Item ${items.length + 1}`];
    setItems(newItems);
  };

  const removeItem = (index) => {
    const newItems = items.filter((_, i) => i !== index);
    setItems(newItems);
  };

  return (
    <div>
      <button onClick={addItem}>Add Item</button>
      <TransitionGroup>
        {items.map((item, index) => (
          <CSSTransition
            key={item}
            timeout={500}
            classNames="item"
          >
            <div>
              {item}
              <button onClick={() => removeItem(index)}>
                Remove
              </button>
            </div>
          </CSSTransition>
        ))}
      </TransitionGroup>
    </div>
  );
};

export default App;
```

对应的 CSS 文件 `styles.css`：

```css
.item-enter {
  opacity: 0;
  transform: translateX(-100%);
}
.item-enter-active {
  opacity: 1;
  transform: translateX(0%);
  transition: opacity 500ms, transform 500ms;
}
.item-exit {
  opacity: 1;
  transform: translateX(0%);
}
.item-exit-active {
  opacity: 0;
  transform: translateX(100%);
  transition: opacity 500ms, transform 500ms;
}
```

## 实践练习

### 练习 1：淡入淡出动画

创建一个简单的按钮，点击按钮时，一个文本框会淡入淡出。使用 `CSSTransition` 组件来实现这个效果。

### 练习 2：列表动画

创建一个列表，点击按钮时，列表中的项目会依次淡入。使用 `TransitionGroup` 和 `CSSTransition` 组件来实现这个效果。

### 练习 3：自定义动画

尝试使用 `Transition` 组件来实现一个自定义的动画效果，比如旋转或缩放。

## 总结

React Transition Group 是一个强大的工具，可以帮助你在 React 应用中轻松实现动画效果。通过本教程，你应该已经掌握了如何使用 `Transition`、`CSSTransition` 和 `TransitionGroup` 组件来创建各种动画效果。继续实践和探索，你将能够为你的应用添加更多生动和有趣的动画。