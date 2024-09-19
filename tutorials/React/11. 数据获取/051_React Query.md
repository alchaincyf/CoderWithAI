---
title: 深入理解React Query：高效数据获取与状态管理
date: 2023-10-05
description: 本课程将深入探讨React Query的使用，教你如何高效地获取和缓存数据，以及如何管理应用状态。
slug: react-query-data-fetching-state-management
tags:
  - React Query
  - 数据获取
  - 状态管理
category: 前端开发
keywords:
  - React Query
  - 数据缓存
  - 状态管理
---

# React Query 教程

## 1. 简介

React Query 是一个强大的库，用于在 React 应用程序中管理和缓存服务器状态。它简化了数据获取、缓存、同步和更新服务器状态的过程。React Query 的核心功能包括数据获取、缓存、分页、无限滚动、错误处理等。

## 2. 安装和环境搭建

首先，确保你已经安装了 Node.js 和 npm。然后，使用 Create React App 创建一个新的 React 项目。

```bash
npx create-react-app react-query-demo
cd react-query-demo
```

接下来，安装 React Query 和 Axios（用于数据获取）。

```bash
npm install @tanstack/react-query axios
```

## 3. 配置 React Query

在你的 React 应用程序中，你需要配置 React Query 的客户端。通常在 `index.js` 或 `App.js` 中进行配置。

```javascript
import React from 'react';
import ReactDOM from 'react-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import App from './App';

const queryClient = new QueryClient();

ReactDOM.render(
  <QueryClientProvider client={queryClient}>
    <App />
  </QueryClientProvider>,
  document.getElementById('root')
);
```

## 4. 基本使用

### 4.1 数据获取

使用 `useQuery` 钩子来获取数据。`useQuery` 接受一个唯一的键（key）和一个返回 Promise 的函数。

```javascript
import React from 'react';
import axios from 'axios';
import { useQuery } from '@tanstack/react-query';

function Posts() {
  const { data, error, isLoading } = useQuery(['posts'], () =>
    axios.get('https://jsonplaceholder.typicode.com/posts').then(res => res.data)
  );

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <ul>
      {data.map(post => (
        <li key={post.id}>{post.title}</li>
      ))}
    </ul>
  );
}

export default Posts;
```

### 4.2 数据缓存

React Query 会自动缓存数据。当再次请求相同的数据时，React Query 会从缓存中返回数据，而不是重新请求服务器。

### 4.3 错误处理

在 `useQuery` 中，你可以通过 `error` 对象来处理错误。

```javascript
if (error) return <div>Error: {error.message}</div>;
```

### 4.4 分页和无限滚动

React Query 支持分页和无限滚动。你可以使用 `useInfiniteQuery` 钩子来实现无限滚动。

```javascript
import React from 'react';
import axios from 'axios';
import { useInfiniteQuery } from '@tanstack/react-query';

function InfinitePosts() {
  const { data, fetchNextPage, hasNextPage, isFetchingNextPage } = useInfiniteQuery(
    ['posts'],
    ({ pageParam = 1 }) =>
      axios.get(`https://jsonplaceholder.typicode.com/posts?_page=${pageParam}&_limit=10`).then(res => res.data),
    {
      getNextPageParam: (lastPage, allPages) => {
        const nextPage = allPages.length + 1;
        return nextPage;
      },
    }
  );

  return (
    <div>
      {data?.pages.map((page, i) => (
        <React.Fragment key={i}>
          {page.map(post => (
            <div key={post.id}>{post.title}</div>
          ))}
        </React.Fragment>
      ))}
      <button onClick={() => fetchNextPage()} disabled={!hasNextPage || isFetchingNextPage}>
        {isFetchingNextPage ? 'Loading more...' : hasNextPage ? 'Load More' : 'Nothing more to load'}
      </button>
    </div>
  );
}

export default InfinitePosts;
```

## 5. 实践练习

### 5.1 练习：创建一个简单的博客应用

1. 使用 React Query 获取博客文章数据。
2. 实现分页功能，每次加载 10 篇文章。
3. 添加错误处理，当请求失败时显示错误信息。

### 5.2 练习：实现无限滚动

1. 使用 `useInfiniteQuery` 钩子实现无限滚动。
2. 当用户滚动到页面底部时，自动加载更多文章。

## 6. 总结

React Query 是一个功能强大的库，简化了 React 应用程序中服务器状态的管理。通过本教程，你学习了如何安装和配置 React Query，以及如何使用 `useQuery` 和 `useInfiniteQuery` 钩子来获取和缓存数据。希望你能通过实践练习进一步掌握 React Query 的使用。

## 7. 下一步

- 深入学习 React Query 的高级功能，如乐观更新、依赖查询等。
- 探索其他状态管理库，如 Redux、MobX 和 Recoil。
- 学习如何使用 React Query 进行表单处理和数据验证。

希望这篇教程对你有所帮助，祝你在 React 开发中取得更多进步！