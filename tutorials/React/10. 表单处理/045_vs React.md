---
title: 受控组件 vs 非受控组件：React中的表单处理
date: 2023-10-05
description: 本课程深入探讨React中受控组件与非受控组件的区别，帮助开发者理解如何高效处理表单数据。
slug: controlled-vs-uncontrolled-components
tags:
  - React
  - 表单处理
  - 前端开发
category: 前端开发
keywords:
  - 受控组件
  - 非受控组件
  - React表单
---

# 受控组件 vs 非受控组件

在 React 中，表单元素的处理是一个常见的需求。React 提供了两种处理表单元素的方式：受控组件（Controlled Components）和非受控组件（Uncontrolled Components）。理解这两种方式的区别和适用场景对于编写高效且易于维护的 React 应用至关重要。

## 1. 受控组件（Controlled Components）

### 1.1 理论解释

受控组件是指其值由 React 的 `state` 管理的表单元素。这意味着表单元素的值完全由 React 控制，每当用户输入时，React 会更新 `state`，然后表单元素的值会根据 `state` 进行更新。

### 1.2 代码示例

```jsx
import React, { useState } from 'react';

function ControlledComponent() {
  const [inputValue, setInputValue] = useState('');

  const handleChange = (event) => {
    setInputValue(event.target.value);
  };

  return (
    <div>
      <input
        type="text"
        value={inputValue}
        onChange={handleChange}
      />
      <p>You typed: {inputValue}</p>
    </div>
  );
}

export default ControlledComponent;
```

### 1.3 实践练习

1. 创建一个新的 React 组件，使用受控组件的方式处理一个 `textarea` 元素。
2. 在 `textarea` 中输入内容时，实时显示输入的字符数。

## 2. 非受控组件（Uncontrolled Components）

### 2.1 理论解释

非受控组件是指其值不由 React 的 `state` 管理的表单元素。相反，表单元素的值由 DOM 本身管理。通常，非受控组件使用 `ref` 来获取表单元素的值。

### 2.2 代码示例

```jsx
import React, { useRef } from 'react';

function UncontrolledComponent() {
  const inputRef = useRef(null);

  const handleSubmit = (event) => {
    event.preventDefault();
    alert(`You typed: ${inputRef.current.value}`);
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        type="text"
        ref={inputRef}
      />
      <button type="submit">Submit</button>
    </form>
  );
}

export default UncontrolledComponent;
```

### 2.3 实践练习

1. 创建一个新的 React 组件，使用非受控组件的方式处理一个 `input` 元素。
2. 在表单提交时，显示输入的值。

## 3. 受控组件 vs 非受控组件：选择合适的场景

### 3.1 何时使用受控组件

- **实时数据验证**：受控组件可以方便地进行实时数据验证，因为每次输入都会触发 `onChange` 事件。
- **复杂的表单逻辑**：如果表单逻辑较为复杂，受控组件可以更好地管理状态。
- **动态表单**：需要动态添加或删除表单元素时，受控组件更易于管理。

### 3.2 何时使用非受控组件

- **简单的表单**：对于简单的表单，非受控组件可以减少代码量，简化实现。
- **文件上传**：文件上传通常使用非受控组件，因为文件输入的值是只读的。
- **性能优化**：在某些情况下，非受控组件可以减少不必要的 `state` 更新，从而提高性能。

## 4. 综合练习

### 4.1 任务描述

创建一个包含多个表单元素的注册页面，要求：

1. 使用受控组件处理用户名、密码和电子邮件输入框。
2. 使用非受控组件处理文件上传。
3. 在提交表单时，显示所有输入的值。

### 4.2 代码示例

```jsx
import React, { useState, useRef } from 'react';

function RegistrationForm() {
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [email, setEmail] = useState('');
  const fileInputRef = useRef(null);

  const handleSubmit = (event) => {
    event.preventDefault();
    alert(`
      Username: ${username}
      Password: ${password}
      Email: ${email}
      File: ${fileInputRef.current.files[0] ? fileInputRef.current.files[0].name : 'No file selected'}
    `);
  };

  return (
    <form onSubmit={handleSubmit}>
      <div>
        <label>Username:</label>
        <input
          type="text"
          value={username}
          onChange={(e) => setUsername(e.target.value)}
        />
      </div>
      <div>
        <label>Password:</label>
        <input
          type="password"
          value={password}
          onChange={(e) => setPassword(e.target.value)}
        />
      </div>
      <div>
        <label>Email:</label>
        <input
          type="email"
          value={email}
          onChange={(e) => setEmail(e.target.value)}
        />
      </div>
      <div>
        <label>Upload File:</label>
        <input
          type="file"
          ref={fileInputRef}
        />
      </div>
      <button type="submit">Register</button>
    </form>
  );
}

export default RegistrationForm;
```

## 5. 总结

受控组件和非受控组件各有优缺点，选择哪种方式取决于具体的应用场景。受控组件适用于需要实时数据验证和复杂表单逻辑的场景，而非受控组件则适用于简单的表单和文件上传。通过理解和实践这两种方式，你可以更好地掌握 React 中的表单处理技巧。

希望这篇教程能帮助你更好地理解受控组件和非受控组件的概念，并在实际项目中灵活应用。