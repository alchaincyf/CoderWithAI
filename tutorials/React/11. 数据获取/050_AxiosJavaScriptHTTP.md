---
title: 掌握Axios：现代JavaScript中的HTTP请求
date: 2023-10-05
description: 本课程将深入探讨如何使用Axios在JavaScript中进行HTTP请求，涵盖从基础到高级的用法，包括拦截器、错误处理和并发请求。
slug: mastering-axios-in-javascript
tags:
  - JavaScript
  - Axios
  - HTTP
category: 前端开发
keywords:
  - Axios教程
  - JavaScript HTTP请求
  - Axios拦截器
---

# Axios 教程

## 1. 简介

Axios 是一个基于 Promise 的 HTTP 客户端，用于浏览器和 Node.js。它允许你轻松地发送异步 HTTP 请求到 REST 端点，并处理 JSON 数据。Axios 提供了许多强大的功能，如拦截请求和响应、转换请求和响应数据、取消请求等。

## 2. 安装 Axios

在使用 Axios 之前，你需要先安装它。你可以通过 npm 或 yarn 来安装 Axios。

### 使用 npm:

```bash
npm install axios
```

### 使用 yarn:

```bash
yarn add axios
```

## 3. 基本用法

### 3.1 发送 GET 请求

```javascript
import axios from 'axios';

axios.get('https://api.example.com/data')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error('There was an error!', error);
  });
```

### 3.2 发送 POST 请求

```javascript
import axios from 'axios';

axios.post('https://api.example.com/data', {
    firstName: 'John',
    lastName: 'Doe'
  })
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error('There was an error!', error);
  });
```

### 3.3 发送 PUT 请求

```javascript
import axios from 'axios';

axios.put('https://api.example.com/data/1', {
    firstName: 'John',
    lastName: 'Doe'
  })
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error('There was an error!', error);
  });
```

### 3.4 发送 DELETE 请求

```javascript
import axios from 'axios';

axios.delete('https://api.example.com/data/1')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error('There was an error!', error);
  });
```

## 4. 配置 Axios

你可以通过创建一个 Axios 实例来配置默认的请求参数。

```javascript
import axios from 'axios';

const instance = axios.create({
  baseURL: 'https://api.example.com',
  timeout: 1000,
  headers: {'X-Custom-Header': 'foobar'}
});

instance.get('/data')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error('There was an error!', error);
  });
```

## 5. 拦截器

Axios 允许你拦截请求和响应，以便在它们被处理之前或之后执行某些操作。

### 5.1 请求拦截器

```javascript
axios.interceptors.request.use(config => {
  console.log('Request was sent');
  return config;
}, error => {
  return Promise.reject(error);
});
```

### 5.2 响应拦截器

```javascript
axios.interceptors.response.use(response => {
  console.log('Response was received');
  return response;
}, error => {
  return Promise.reject(error);
});
```

## 6. 取消请求

你可以使用 `CancelToken` 来取消请求。

```javascript
import axios from 'axios';

const source = axios.CancelToken.source();

axios.get('https://api.example.com/data', {
  cancelToken: source.token
}).catch(thrown => {
  if (axios.isCancel(thrown)) {
    console.log('Request canceled', thrown.message);
  } else {
    console.error('There was an error!', thrown);
  }
});

// 取消请求（message 参数是可选的）
source.cancel('Operation canceled by the user.');
```

## 7. 实践练习

### 7.1 创建一个简单的 React 应用

1. 使用 `Create React App` 创建一个新的 React 项目。
2. 安装 Axios。
3. 创建一个组件，使用 Axios 从公共 API 获取数据并显示在页面上。

### 7.2 代码示例

```javascript
import React, { useEffect, useState } from 'react';
import axios from 'axios';

function App() {
  const [data, setData] = useState([]);

  useEffect(() => {
    axios.get('https://api.example.com/data')
      .then(response => {
        setData(response.data);
      })
      .catch(error => {
        console.error('There was an error!', error);
      });
  }, []);

  return (
    <div>
      <h1>Data from API</h1>
      <ul>
        {data.map(item => (
          <li key={item.id}>{item.name}</li>
        ))}
      </ul>
    </div>
  );
}

export default App;
```

## 8. 总结

Axios 是一个功能强大且易于使用的 HTTP 客户端，适用于浏览器和 Node.js。通过本教程，你应该已经掌握了 Axios 的基本用法，包括发送各种类型的请求、配置 Axios、使用拦截器和取消请求。希望你能将这些知识应用到实际项目中，提升你的开发效率。