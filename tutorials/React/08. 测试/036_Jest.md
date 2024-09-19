---
title: Jest 单元测试入门教程
date: 2023-10-05
description: 本教程将带你深入了解如何使用 Jest 进行 JavaScript 单元测试，涵盖基础概念、常用功能及高级技巧。
slug: jest-unit-testing-tutorial
tags:
  - Jest
  - 单元测试
  - JavaScript
category: 编程教程
keywords:
  - Jest 单元测试
  - JavaScript 测试
  - 前端测试
---

# Jest 单元测试

## 1. 简介

Jest 是一个由 Facebook 开发的 JavaScript 测试框架，广泛用于 React 应用程序的单元测试。它提供了简单易用的 API，支持快照测试、模拟、代码覆盖率等功能。本教程将带你了解 Jest 的基本概念、安装、配置以及如何编写和运行单元测试。

## 2. 安装 Jest

### 2.1 使用 Create React App

如果你使用的是 Create React App 创建的 React 项目，Jest 已经默认安装并配置好了。你可以直接开始编写测试。

```bash
npx create-react-app my-app
cd my-app
npm test
```

### 2.2 手动安装

如果你没有使用 Create React App，可以通过 npm 手动安装 Jest：

```bash
npm install --save-dev jest
```

然后在 `package.json` 中添加测试脚本：

```json
{
  "scripts": {
    "test": "jest"
  }
}
```

## 3. 编写第一个测试

### 3.1 创建测试文件

Jest 会自动查找以 `.test.js` 或 `.spec.js` 结尾的文件进行测试。我们可以在 `src` 目录下创建一个 `App.test.js` 文件。

```javascript
// src/App.test.js
import React from 'react';
import { render, screen } from '@testing-library/react';
import App from './App';

test('renders learn react link', () => {
  render(<App />);
  const linkElement = screen.getByText(/learn react/i);
  expect(linkElement).toBeInTheDocument();
});
```

### 3.2 运行测试

在终端中运行以下命令来执行测试：

```bash
npm test
```

Jest 会自动运行所有测试文件，并显示测试结果。

## 4. 常用 API

### 4.1 `test` 和 `it`

`test` 和 `it` 是 Jest 中定义测试用例的函数。它们是同义词，可以互换使用。

```javascript
test('adds 1 + 2 to equal 3', () => {
  expect(1 + 2).toBe(3);
});

it('subtracts 2 - 1 to equal 1', () => {
  expect(2 - 1).toBe(1);
});
```

### 4.2 `expect`

`expect` 用于断言某个值是否符合预期。Jest 提供了丰富的匹配器（matchers）来帮助你编写断言。

```javascript
expect(2 + 2).toBe(4);
expect('hello').toBe('hello');
expect([1, 2, 3]).toEqual([1, 2, 3]);
```

### 4.3 `describe`

`describe` 用于将多个相关的测试用例分组。

```javascript
describe('Math operations', () => {
  test('adds 1 + 2 to equal 3', () => {
    expect(1 + 2).toBe(3);
  });

  test('subtracts 2 - 1 to equal 1', () => {
    expect(2 - 1).toBe(1);
  });
});
```

## 5. 模拟和快照测试

### 5.1 模拟函数

Jest 允许你模拟函数，以便在测试中控制其行为。

```javascript
const mockFn = jest.fn();
mockFn('hello');
expect(mockFn).toHaveBeenCalledWith('hello');
```

### 5.2 快照测试

快照测试用于确保组件的输出不会意外更改。Jest 会自动生成快照文件，并在后续测试中进行比较。

```javascript
import React from 'react';
import renderer from 'react-test-renderer';
import App from './App';

test('App snapshot', () => {
  const tree = renderer.create(<App />).toJSON();
  expect(tree).toMatchSnapshot();
});
```

## 6. 实践练习

### 6.1 练习 1：测试组件渲染

编写一个测试，确保 `App` 组件正确渲染了某个文本。

```javascript
test('renders welcome message', () => {
  render(<App />);
  const messageElement = screen.getByText(/welcome to react/i);
  expect(messageElement).toBeInTheDocument();
});
```

### 6.2 练习 2：模拟 API 调用

假设你的组件中有一个 API 调用，编写一个测试来模拟这个调用并验证组件的行为。

```javascript
import React from 'react';
import { render, screen, waitFor } from '@testing-library/react';
import axios from 'axios';
import App from './App';

jest.mock('axios');

test('fetches data and displays it', async () => {
  axios.get.mockResolvedValue({ data: { message: 'Hello, World!' } });
  render(<App />);
  const messageElement = await waitFor(() => screen.getByText(/Hello, World!/i));
  expect(messageElement).toBeInTheDocument();
});
```

## 7. 总结

通过本教程，你已经学会了如何使用 Jest 进行单元测试。Jest 提供了强大的功能和简洁的 API，帮助你确保代码的正确性和稳定性。继续探索 Jest 的更多功能，如代码覆盖率、异步测试等，进一步提升你的测试技能。

## 8. 下一步

- 学习 React Testing Library，它与 Jest 结合使用，提供更接近用户行为的测试方式。
- 探索端到端测试工具，如 Cypress，以确保整个应用的行为符合预期。

希望本教程对你有所帮助，祝你在 React 开发和测试的道路上越走越远！