---
title: Jest 与 TypeScript 集成教程
date: 2023-10-05
description: 本课程详细介绍如何在TypeScript项目中集成和使用Jest进行单元测试，涵盖配置、编写测试用例及常见问题解决。
slug: jest-typescript-integration
tags:
  - Jest
  - TypeScript
  - 单元测试
category: 编程教程
keywords:
  - Jest TypeScript
  - TypeScript 单元测试
  - Jest 配置
---

# Jest 与 TypeScript 教程

## 1. 概述

在本教程中，我们将深入探讨如何在 TypeScript 项目中使用 Jest 进行单元测试。Jest 是一个流行的 JavaScript 测试框架，而 TypeScript 是一种强类型的 JavaScript 超集。结合两者，我们可以编写类型安全且易于维护的测试代码。

## 2. 开发环境搭建

### 2.1 安装 Node.js 和 TypeScript

首先，确保你已经安装了 Node.js 和 TypeScript。如果还没有安装，可以通过以下命令进行安装：

```bash
# 安装 Node.js
curl -fsSL https://deb.nodesource.com/setup_16.x | sudo -E bash -
sudo apt-get install -y nodejs

# 安装 TypeScript
npm install -g typescript
```

### 2.2 初始化 TypeScript 项目

创建一个新的项目目录并初始化 TypeScript 项目：

```bash
mkdir jest-typescript-tutorial
cd jest-typescript-tutorial
npm init -y
tsc --init
```

### 2.3 安装 Jest 和相关依赖

接下来，安装 Jest 和 TypeScript 相关的依赖：

```bash
npm install --save-dev jest ts-jest @types/jest
```

### 2.4 配置 Jest

在项目根目录下创建一个 `jest.config.js` 文件，并添加以下内容：

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

## 3. 编写第一个 TypeScript 程序

### 3.1 创建一个简单的 TypeScript 文件

在 `src` 目录下创建一个 `index.ts` 文件，并编写一个简单的函数：

```typescript
// src/index.ts
export function add(a: number, b: number): number {
  return a + b;
}
```

### 3.2 编译 TypeScript 文件

使用 TypeScript 编译器将 `index.ts` 编译为 JavaScript：

```bash
tsc
```

## 4. 编写单元测试

### 4.1 创建测试文件

在 `src` 目录下创建一个 `index.test.ts` 文件，并编写测试代码：

```typescript
// src/index.test.ts
import { add } from './index';

test('adds 1 + 2 to equal 3', () => {
  expect(add(1, 2)).toBe(3);
});
```

### 4.2 运行测试

使用 Jest 运行测试：

```bash
npx jest
```

你应该会看到类似以下的输出：

```bash
 PASS  src/index.test.ts
  ✓ adds 1 + 2 to equal 3 (2 ms)

Test Suites: 1 passed, 1 total
Tests:       1 passed, 1 total
Snapshots:   0 total
Time:        1.234 s
Ran all test suites.
```

## 5. 实践练习

### 5.1 练习 1：编写更多测试

尝试为 `add` 函数编写更多的测试用例，例如：

- 测试负数相加
- 测试浮点数相加

### 5.2 练习 2：编写新的函数并测试

编写一个新的函数 `multiply`，并为其编写测试用例：

```typescript
// src/index.ts
export function multiply(a: number, b: number): number {
  return a * b;
}
```

```typescript
// src/index.test.ts
import { add, multiply } from './index';

test('multiplies 2 * 3 to equal 6', () => {
  expect(multiply(2, 3)).toBe(6);
});
```

## 6. 测试覆盖率

### 6.1 生成测试覆盖率报告

Jest 可以生成测试覆盖率报告。运行以下命令：

```bash
npx jest --coverage
```

你将看到类似以下的输出：

```bash
 PASS  src/index.test.ts
  ✓ adds 1 + 2 to equal 3 (2 ms)
  ✓ multiplies 2 * 3 to equal 6 (1 ms)

----------|---------|----------|---------|---------|-------------------
File      | % Stmts | % Branch | % Funcs | % Lines | Uncovered Line #s 
----------|---------|----------|---------|---------|-------------------
All files |   100   |   100    |   100   |   100   |                   
 index.ts |   100   |   100    |   100   |   100   |                   
----------|---------|----------|---------|---------|-------------------
```

### 6.2 查看覆盖率报告

生成的覆盖率报告可以在 `coverage` 目录下找到，打开 `index.html` 文件即可查看详细的覆盖率报告。

## 7. 总结

在本教程中，我们学习了如何在 TypeScript 项目中使用 Jest 进行单元测试。我们从环境搭建开始，编写了第一个 TypeScript 程序，并为其编写了测试用例。我们还学习了如何生成和查看测试覆盖率报告。

通过这些步骤，你应该已经掌握了在 TypeScript 项目中使用 Jest 进行单元测试的基本技能。继续探索 TypeScript 和 Jest 的更多高级功能，进一步提升你的测试技能。

## 8. 进一步学习

- **Jest 官方文档**: [https://jestjs.io/docs/getting-started](https://jestjs.io/docs/getting-started)
- **TypeScript 官方文档**: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- **Jest 与 TypeScript 结合的高级用法**: 探索 Jest 的 Mocking、Snapshot Testing 等功能。

希望本教程对你有所帮助，祝你在 TypeScript 和 Jest 的学习旅程中取得成功！