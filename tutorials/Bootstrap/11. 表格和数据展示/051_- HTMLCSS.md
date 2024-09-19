---
title: 基本表格样式 - HTML与CSS入门
date: 2023-10-05
description: 本课程将教你如何使用HTML和CSS创建和样式化基本表格，包括表格结构、边框、背景颜色、文本对齐等。
slug: basic-table-styling
tags:
  - HTML
  - CSS
  - 表格样式
category: 前端开发
keywords:
  - HTML表格
  - CSS样式
  - 表格设计
---

# 基本表格样式

## 概述

在网页设计中，表格（Table）是一种常见的元素，用于展示结构化的数据。Bootstrap 提供了丰富的样式类，使得我们可以轻松地创建美观且响应式的表格。本教程将详细介绍如何使用 Bootstrap 来创建和定制表格样式。

## 理论解释

### 表格的基本结构

在 HTML 中，表格由 `<table>` 标签定义，内部包含 `<thead>`（表头）、`<tbody>`（表体）和 `<tfoot>`（表尾）等部分。每一行由 `<tr>` 标签定义，单元格由 `<td>`（表体单元格）或 `<th>`（表头单元格）定义。

### Bootstrap 表格类

Bootstrap 提供了多种预定义的表格类，用于快速应用样式。常见的类包括：

- `table`：基本的表格样式。
- `table-striped`：为表格添加条纹样式。
- `table-bordered`：为表格添加边框。
- `table-hover`：为表格添加悬停效果。
- `table-dark`：将表格背景设为深色。
- `table-sm`：使表格更紧凑。

## 代码示例

### 基本表格

```html
<table class="table">
  <thead>
    <tr>
      <th>姓名</th>
      <th>年龄</th>
      <th>城市</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>张三</td>
      <td>28</td>
      <td>北京</td>
    </tr>
    <tr>
      <td>李四</td>
      <td>34</td>
      <td>上海</td>
    </tr>
    <tr>
      <td>王五</td>
      <td>22</td>
      <td>广州</td>
    </tr>
  </tbody>
</table>
```

### 条纹表格

```html
<table class="table table-striped">
  <thead>
    <tr>
      <th>姓名</th>
      <th>年龄</th>
      <th>城市</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>张三</td>
      <td>28</td>
      <td>北京</td>
    </tr>
    <tr>
      <td>李四</td>
      <td>34</td>
      <td>上海</td>
    </tr>
    <tr>
      <td>王五</td>
      <td>22</td>
      <td>广州</td>
    </tr>
  </tbody>
</table>
```

### 带边框的表格

```html
<table class="table table-bordered">
  <thead>
    <tr>
      <th>姓名</th>
      <th>年龄</th>
      <th>城市</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>张三</td>
      <td>28</td>
      <td>北京</td>
    </tr>
    <tr>
      <td>李四</td>
      <td>34</td>
      <td>上海</td>
    </tr>
    <tr>
      <td>王五</td>
      <td>22</td>
      <td>广州</td>
    </tr>
  </tbody>
</table>
```

### 悬停效果表格

```html
<table class="table table-hover">
  <thead>
    <tr>
      <th>姓名</th>
      <th>年龄</th>
      <th>城市</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>张三</td>
      <td>28</td>
      <td>北京</td>
    </tr>
    <tr>
      <td>李四</td>
      <td>34</td>
      <td>上海</td>
    </tr>
    <tr>
      <td>王五</td>
      <td>22</td>
      <td>广州</td>
    </tr>
  </tbody>
</table>
```

### 紧凑表格

```html
<table class="table table-sm">
  <thead>
    <tr>
      <th>姓名</th>
      <th>年龄</th>
      <th>城市</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>张三</td>
      <td>28</td>
      <td>北京</td>
    </tr>
    <tr>
      <td>李四</td>
      <td>34</td>
      <td>上海</td>
    </tr>
    <tr>
      <td>王五</td>
      <td>22</td>
      <td>广州</td>
    </tr>
  </tbody>
</table>
```

## 实践练习

### 练习1：创建一个带条纹和边框的表格

要求：创建一个表格，包含姓名、年龄和城市三列，并为表格添加条纹和边框样式。

```html
<table class="table table-striped table-bordered">
  <thead>
    <tr>
      <th>姓名</th>
      <th>年龄</th>
      <th>城市</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>张三</td>
      <td>28</td>
      <td>北京</td>
    </tr>
    <tr>
      <td>李四</td>
      <td>34</td>
      <td>上海</td>
    </tr>
    <tr>
      <td>王五</td>
      <td>22</td>
      <td>广州</td>
    </tr>
  </tbody>
</table>
```

### 练习2：创建一个深色背景的悬停效果表格

要求：创建一个表格，包含姓名、年龄和城市三列，并为表格添加深色背景和悬停效果。

```html
<table class="table table-dark table-hover">
  <thead>
    <tr>
      <th>姓名</th>
      <th>年龄</th>
      <th>城市</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>张三</td>
      <td>28</td>
      <td>北京</td>
    </tr>
    <tr>
      <td>李四</td>
      <td>34</td>
      <td>上海</td>
    </tr>
    <tr>
      <td>王五</td>
      <td>22</td>
      <td>广州</td>
    </tr>
  </tbody>
</table>
```

## 总结

通过本教程，我们学习了如何使用 Bootstrap 创建和定制表格样式。Bootstrap 提供了丰富的表格类，使得我们可以轻松地实现各种表格效果。希望你能通过实践练习，进一步掌握这些技巧，并在实际项目中灵活应用。