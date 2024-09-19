---
title: 日期选择器开发教程：从基础到高级
date: 2023-10-05
description: 本课程详细讲解如何开发一个功能强大的日期选择器，涵盖从基础的HTML/CSS布局到高级的JavaScript交互实现。
slug: date-picker-development-tutorial
tags:
  - 前端开发
  - JavaScript
  - UI组件
category: 前端开发
keywords:
  - 日期选择器
  - JavaScript日期选择器
  - 前端UI组件
---

# 日期选择器

## 1. 简介

日期选择器（Date Picker）是一种常见的用户界面组件，允许用户从日历中选择日期。在网页开发中，日期选择器通常用于表单中，帮助用户输入日期。Bootstrap 提供了强大的日期选择器组件，可以轻松集成到你的项目中。

## 2. 安装和引入 Bootstrap

在开始使用 Bootstrap 的日期选择器之前，你需要确保已经安装并引入了 Bootstrap。如果你还没有安装 Bootstrap，可以通过以下步骤进行安装：

### 2.1 使用 npm 安装 Bootstrap

```bash
npm install bootstrap
```

### 2.2 引入 Bootstrap CSS 和 JavaScript

在你的 HTML 文件中引入 Bootstrap 的 CSS 和 JavaScript 文件：

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>日期选择器示例</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
</head>
<body>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"></script>
</body>
</html>
```

## 3. 创建日期选择器

Bootstrap 的日期选择器是基于第三方库 `tempus-dominus` 的。你需要引入 `tempus-dominus` 的 CSS 和 JavaScript 文件。

### 3.1 引入 `tempus-dominus`

```html
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tempusdominus-bootstrap-4/5.39.0/css/tempusdominus-bootstrap-4.min.css" />
<script src="https://cdnjs.cloudflare.com/ajax/libs/tempusdominus-bootstrap-4/5.39.0/js/tempusdominus-bootstrap-4.min.js"></script>
```

### 3.2 创建日期选择器

在你的 HTML 文件中创建一个输入框，并使用 `tempus-dominus` 初始化日期选择器：

```html
<div class="container mt-5">
    <div class="form-group">
        <label for="datetimepicker1">选择日期和时间</label>
        <div class="input-group date" id="datetimepicker1" data-target-input="nearest">
            <input type="text" class="form-control datetimepicker-input" data-target="#datetimepicker1"/>
            <div class="input-group-append" data-target="#datetimepicker1" data-toggle="datetimepicker">
                <div class="input-group-text"><i class="fa fa-calendar"></i></div>
            </div>
        </div>
    </div>
</div>

<script>
    $(function () {
        $('#datetimepicker1').datetimepicker();
    });
</script>
```

### 3.3 解释代码

- `input-group`：用于创建一个输入组，包含输入框和日历图标。
- `datetimepicker-input`：用于标识日期选择器的输入框。
- `input-group-append`：用于添加日历图标，并绑定日期选择器的事件。
- `datetimepicker()`：初始化日期选择器。

## 4. 自定义日期选择器

你可以通过传递选项来自定义日期选择器的行为和外观。例如，你可以设置日期格式、禁用某些日期等。

### 4.1 设置日期格式

```javascript
$('#datetimepicker1').datetimepicker({
    format: 'YYYY-MM-DD'
});
```

### 4.2 禁用某些日期

```javascript
$('#datetimepicker1').datetimepicker({
    daysOfWeekDisabled: [0, 6] // 禁用周末
});
```

## 5. 实践练习

### 5.1 练习目标

创建一个包含日期选择器的表单，用户可以选择日期并提交表单。

### 5.2 练习步骤

1. 创建一个新的 HTML 文件，并引入 Bootstrap 和 `tempus-dominus`。
2. 创建一个表单，包含一个日期选择器和一个提交按钮。
3. 使用 JavaScript 初始化日期选择器，并设置日期格式为 `YYYY-MM-DD`。
4. 提交表单时，显示用户选择的日期。

### 5.3 示例代码

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>日期选择器练习</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tempusdominus-bootstrap-4/5.39.0/css/tempusdominus-bootstrap-4.min.css" />
</head>
<body>
    <div class="container mt-5">
        <form id="dateForm">
            <div class="form-group">
                <label for="datetimepicker1">选择日期</label>
                <div class="input-group date" id="datetimepicker1" data-target-input="nearest">
                    <input type="text" class="form-control datetimepicker-input" data-target="#datetimepicker1"/>
                    <div class="input-group-append" data-target="#datetimepicker1" data-toggle="datetimepicker">
                        <div class="input-group-text"><i class="fa fa-calendar"></i></div>
                    </div>
                </div>
            </div>
            <button type="submit" class="btn btn-primary mt-3">提交</button>
        </form>
        <div id="result" class="mt-3"></div>
    </div>

    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/tempusdominus-bootstrap-4/5.39.0/js/tempusdominus-bootstrap-4.min.js"></script>
    <script>
        $(function () {
            $('#datetimepicker1').datetimepicker({
                format: 'YYYY-MM-DD'
            });

            $('#dateForm').on('submit', function(e) {
                e.preventDefault();
                var selectedDate = $('#datetimepicker1').datetimepicker('viewDate').format('YYYY-MM-DD');
                $('#result').text('您选择的日期是：' + selectedDate);
            });
        });
    </script>
</body>
</html>
```

## 6. 总结

通过本教程，你学会了如何在 Bootstrap 中使用日期选择器。你了解了如何安装和引入 Bootstrap，创建日期选择器，以及如何自定义日期选择器的行为和外观。通过实践练习，你进一步巩固了这些知识。希望你能将这些技能应用到实际项目中，提升用户体验。