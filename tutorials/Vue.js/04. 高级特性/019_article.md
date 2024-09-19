---
title: 自定义指令：深入理解与应用
date: 2023-10-05
description: 本课程将深入探讨如何在编程中创建和使用自定义指令，涵盖从基础概念到高级应用的全面内容。
slug: custom-directives-programming
tags:
  - 自定义指令
  - 编程技巧
  - 高级编程
category: 编程技术
keywords:
  - 自定义指令
  - 编程教程
  - 编程技术
---

# 自定义指令

## 概述

在 Vue.js 中，除了内置的指令（如 `v-if`、`v-for`、`v-bind` 等），开发者还可以创建自定义指令来扩展 Vue 的功能。自定义指令允许你在 DOM 元素上应用特定的行为，这些行为可以是任何你需要的操作，比如聚焦输入框、拖拽元素、自动调整大小等。

## 自定义指令的基本语法

Vue.js 提供了两种方式来定义自定义指令：全局指令和局部指令。

### 全局指令

全局指令可以在整个应用中使用。你可以通过 `Vue.directive` 方法来注册一个全局指令。

```javascript
Vue.directive('focus', {
  // 当被绑定的元素插入到 DOM 中时……
  inserted: function (el) {
    // 聚焦元素
    el.focus();
  }
});
```

### 局部指令

局部指令只能在定义它的组件内部使用。你可以在组件的 `directives` 选项中注册一个局部指令。

```javascript
export default {
  directives: {
    focus: {
      // 指令的定义
      inserted: function (el) {
        el.focus();
      }
    }
  }
};
```

## 指令钩子函数

自定义指令可以定义多个钩子函数，这些钩子函数会在不同的生命周期阶段被调用。以下是常用的钩子函数：

- `bind`: 只调用一次，指令第一次绑定到元素时调用。
- `inserted`: 被绑定元素插入父节点时调用（仅保证父节点存在，但不一定已被插入文档中）。
- `update`: 所在组件的 VNode 更新时调用，但是可能发生在其子 VNode 更新之前。
- `componentUpdated`: 指令所在组件的 VNode 及其子 VNode 全部更新后调用。
- `unbind`: 只调用一次，指令与元素解绑时调用。

每个钩子函数都可以接收以下参数：

- `el`: 指令所绑定的元素，可以用来直接操作 DOM。
- `binding`: 一个对象，包含以下属性：
  - `name`: 指令名，不包括 `v-` 前缀。
  - `value`: 指令的绑定值，例如 `v-my-directive="1 + 1"` 中，绑定值为 `2`。
  - `oldValue`: 指令绑定的前一个值，仅在 `update` 和 `componentUpdated` 钩子中可用。
  - `expression`: 字符串形式的指令表达式，例如 `v-my-directive="1 + 1"` 中，表达式为 `"1 + 1"`。
  - `arg`: 传给指令的参数，例如 `v-my-directive:foo` 中，参数为 `"foo"`。
  - `modifiers`: 一个包含修饰符的对象，例如 `v-my-directive.foo.bar` 中，修饰符对象为 `{ foo: true, bar: true }`。
- `vnode`: Vue 编译生成的虚拟节点。
- `oldVnode`: 上一个虚拟节点，仅在 `update` 和 `componentUpdated` 钩子中可用。

## 示例：自动聚焦输入框

让我们通过一个简单的示例来理解如何使用自定义指令。我们将创建一个自定义指令 `v-focus`，当输入框被插入到 DOM 中时自动聚焦。

### 全局指令

```javascript
Vue.directive('focus', {
  inserted: function (el) {
    el.focus();
  }
});
```

### 局部指令

```javascript
export default {
  directives: {
    focus: {
      inserted: function (el) {
        el.focus();
      }
    }
  }
};
```

### 使用指令

```html
<template>
  <div>
    <input v-focus type="text" placeholder="自动聚焦">
  </div>
</template>
```

## 实践练习

### 练习 1：创建一个自定义指令 `v-color`

创建一个自定义指令 `v-color`，它可以根据传入的颜色值来改变元素的背景颜色。

#### 全局指令

```javascript
Vue.directive('color', {
  bind: function (el, binding) {
    el.style.backgroundColor = binding.value;
  }
});
```

#### 局部指令

```javascript
export default {
  directives: {
    color: {
      bind: function (el, binding) {
        el.style.backgroundColor = binding.value;
      }
    }
  }
};
```

#### 使用指令

```html
<template>
  <div>
    <div v-color="'red'">红色背景</div>
    <div v-color="'blue'">蓝色背景</div>
  </div>
</template>
```

### 练习 2：创建一个自定义指令 `v-draggable`

创建一个自定义指令 `v-draggable`，使元素可以被拖拽。

#### 全局指令

```javascript
Vue.directive('draggable', {
  bind: function (el) {
    el.style.position = 'absolute';
    el.onmousedown = function (e) {
      let offsetX = e.clientX - el.offsetLeft;
      let offsetY = e.clientY - el.offsetTop;
      document.onmousemove = function (e) {
        el.style.left = e.clientX - offsetX + 'px';
        el.style.top = e.clientY - offsetY + 'px';
      };
      document.onmouseup = function () {
        document.onmousemove = null;
        document.onmouseup = null;
      };
    };
  }
});
```

#### 局部指令

```javascript
export default {
  directives: {
    draggable: {
      bind: function (el) {
        el.style.position = 'absolute';
        el.onmousedown = function (e) {
          let offsetX = e.clientX - el.offsetLeft;
          let offsetY = e.clientY - el.offsetTop;
          document.onmousemove = function (e) {
            el.style.left = e.clientX - offsetX + 'px';
            el.style.top = e.clientY - offsetY + 'px';
          };
          document.onmouseup = function () {
            document.onmousemove = null;
            document.onmouseup = null;
          };
        };
      }
    }
  }
};
```

#### 使用指令

```html
<template>
  <div>
    <div v-draggable style="width: 100px; height: 100px; background-color: yellow;">拖拽我</div>
  </div>
</template>
```

## 总结

自定义指令是 Vue.js 中一个非常强大的功能，它允许开发者通过简单的 API 来扩展 Vue 的功能。通过自定义指令，你可以轻松地实现一些复杂的 DOM 操作，而不需要编写大量的重复代码。希望本教程能帮助你更好地理解和使用 Vue.js 的自定义指令。