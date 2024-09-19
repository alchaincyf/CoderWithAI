---
title: 深入理解模块系统：CommonJS 和 ES Modules
date: 2023-10-05
description: 本课程详细讲解了JavaScript中的模块系统，包括CommonJS和ES Modules的实现原理、使用方法以及它们之间的区别。
slug: understanding-module-systems-commonjs-es-modules
tags:
  - JavaScript
  - Node.js
  - 模块系统
category: 前端开发
keywords:
  - CommonJS
  - ES Modules
  - JavaScript模块化
---

# 模块系统（CommonJS 和 ES Modules）

在现代编程中，模块化是一个非常重要的概念。它允许我们将代码分割成多个独立的模块，每个模块负责特定的功能。这不仅使代码更易于维护，还促进了代码的重用。在 Node.js 中，我们主要使用两种模块系统：CommonJS 和 ES Modules。

## 1. CommonJS 模块系统

CommonJS 是 Node.js 最初采用的模块系统。它使用 `require` 和 `module.exports` 来导入和导出模块。

### 1.1 导出模块

在 CommonJS 中，我们使用 `module.exports` 来导出一个模块。例如，我们有一个名为 `math.js` 的文件，其中包含一些数学函数：

```javascript
// math.js
function add(a, b) {
    return a + b;
}

function subtract(a, b) {
    return a - b;
}

module.exports = {
    add,
    subtract
};
```

### 1.2 导入模块

在另一个文件中，我们可以使用 `require` 来导入这个模块：

```javascript
// app.js
const math = require('./math');

console.log(math.add(5, 3)); // 输出: 8
console.log(math.subtract(5, 3)); // 输出: 2
```

### 1.3 实践练习

创建一个名为 `utils.js` 的文件，导出一个函数 `greet`，该函数接受一个名字并返回问候语。然后在 `app.js` 中导入并使用这个函数。

```javascript
// utils.js
function greet(name) {
    return `Hello, ${name}!`;
}

module.exports = greet;
```

```javascript
// app.js
const greet = require('./utils');

console.log(greet('Alice')); // 输出: Hello, Alice!
```

## 2. ES Modules 模块系统

ES Modules 是 ECMAScript 6 (ES6) 引入的标准模块系统。它使用 `import` 和 `export` 来导入和导出模块。

### 2.1 导出模块

在 ES Modules 中，我们可以使用 `export` 关键字来导出模块。例如，我们有一个名为 `math.js` 的文件，其中包含一些数学函数：

```javascript
// math.js
export function add(a, b) {
    return a + b;
}

export function subtract(a, b) {
    return a - b;
}
```

### 2.2 导入模块

在另一个文件中，我们可以使用 `import` 来导入这个模块：

```javascript
// app.js
import { add, subtract } from './math.js';

console.log(add(5, 3)); // 输出: 8
console.log(subtract(5, 3)); // 输出: 2
```

### 2.3 默认导出

ES Modules 还支持默认导出，使用 `export default` 关键字。例如：

```javascript
// utils.js
export default function greet(name) {
    return `Hello, ${name}!`;
}
```

在导入时，我们可以为默认导出的模块指定任意名称：

```javascript
// app.js
import greet from './utils.js';

console.log(greet('Alice')); // 输出: Hello, Alice!
```

### 2.4 实践练习

创建一个名为 `utils.js` 的文件，导出一个默认函数 `greet`，该函数接受一个名字并返回问候语。然后在 `app.js` 中导入并使用这个函数。

```javascript
// utils.js
export default function greet(name) {
    return `Hello, ${name}!`;
}
```

```javascript
// app.js
import greet from './utils.js';

console.log(greet('Alice')); // 输出: Hello, Alice!
```

## 3. CommonJS 与 ES Modules 的对比

| 特性            | CommonJS                  | ES Modules                |
|-----------------|---------------------------|---------------------------|
| 导入方式        | `require`                 | `import`                  |
| 导出方式        | `module.exports`          | `export` 或 `export default` |
| 动态加载        | 支持                      | 部分支持（需要 `import()`） |
| 默认导出        | 不支持                    | 支持                      |
| 文件扩展名      | `.js`                     | `.js` 或 `.mjs`           |

## 4. 实践项目

### 4.1 项目描述

创建一个简单的计算器应用，使用 CommonJS 和 ES Modules 两种模块系统分别实现。

### 4.2 项目结构

```
calculator/
├── commonjs/
│   ├── math.js
│   └── app.js
└── esmodules/
    ├── math.js
    └── app.js
```

### 4.3 实现步骤

1. **CommonJS 实现**

   - `math.js`：
     ```javascript
     function add(a, b) {
         return a + b;
     }

     function subtract(a, b) {
         return a - b;
     }

     module.exports = {
         add,
         subtract
     };
     ```

   - `app.js`：
     ```javascript
     const math = require('./math');

     console.log(math.add(5, 3)); // 输出: 8
     console.log(math.subtract(5, 3)); // 输出: 2
     ```

2. **ES Modules 实现**

   - `math.js`：
     ```javascript
     export function add(a, b) {
         return a + b;
     }

     export function subtract(a, b) {
         return a - b;
     }
     ```

   - `app.js`：
     ```javascript
     import { add, subtract } from './math.js';

     console.log(add(5, 3)); // 输出: 8
     console.log(subtract(5, 3)); // 输出: 2
     ```

### 4.4 运行项目

- 对于 CommonJS 实现，直接运行 `node commonjs/app.js`。
- 对于 ES Modules 实现，确保文件扩展名为 `.mjs` 或使用 `node --experimental-modules esmodules/app.js` 运行。

## 5. 总结

通过本教程，我们学习了 Node.js 中的两种主要模块系统：CommonJS 和 ES Modules。我们了解了它们的语法、使用方法以及它们之间的主要区别。通过实践练习和项目实现，我们进一步加深了对模块系统的理解。希望这些知识能够帮助你在未来的 Node.js 开发中更好地组织和管理代码。