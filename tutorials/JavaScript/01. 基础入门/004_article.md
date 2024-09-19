---
title: 运算符和表达式详解
date: 2023-10-05
description: 本课程详细讲解编程中的运算符和表达式，包括算术、比较、逻辑运算符及其应用场景。
slug: operators-and-expressions
tags:
  - 编程基础
  - 运算符
  - 表达式
category: 编程基础
keywords:
  - 运算符
  - 表达式
  - 编程基础
---

# 运算符和表达式

## 概述

在编程中，运算符和表达式是构建程序逻辑的基础。运算符用于执行各种操作，如算术运算、比较、逻辑运算等。表达式则是由运算符和操作数组成的代码片段，用于计算一个值。理解运算符和表达式是掌握编程语言的关键一步。

## 运算符

### 1. 算术运算符

算术运算符用于执行基本的数学运算。

- `+`：加法
- `-`：减法
- `*`：乘法
- `/`：除法
- `%`：取模（返回除法的余数）
- `**`：幂运算（ES6新增）

**代码示例：**

```javascript
let a = 10;
let b = 3;

console.log(a + b); // 输出: 13
console.log(a - b); // 输出: 7
console.log(a * b); // 输出: 30
console.log(a / b); // 输出: 3.3333333333333335
console.log(a % b); // 输出: 1
console.log(a ** b); // 输出: 1000
```

### 2. 比较运算符

比较运算符用于比较两个值，并返回一个布尔值（`true` 或 `false`）。

- `==`：等于（类型转换后比较）
- `===`：严格等于（不进行类型转换）
- `!=`：不等于（类型转换后比较）
- `!==`：严格不等于（不进行类型转换）
- `>`：大于
- `<`：小于
- `>=`：大于等于
- `<=`：小于等于

**代码示例：**

```javascript
let x = 5;
let y = '5';

console.log(x == y);  // 输出: true
console.log(x === y); // 输出: false
console.log(x != y);  // 输出: false
console.log(x !== y); // 输出: true
console.log(x > 3);   // 输出: true
console.log(x < 3);   // 输出: false
console.log(x >= 5);  // 输出: true
console.log(x <= 5);  // 输出: true
```

### 3. 逻辑运算符

逻辑运算符用于组合多个条件表达式，并返回一个布尔值。

- `&&`：逻辑与（两个条件都为真时返回真）
- `||`：逻辑或（两个条件中有一个为真时返回真）
- `!`：逻辑非（取反）

**代码示例：**

```javascript
let isSunny = true;
let isWarm = false;

console.log(isSunny && isWarm); // 输出: false
console.log(isSunny || isWarm); // 输出: true
console.log(!isSunny);          // 输出: false
```

### 4. 赋值运算符

赋值运算符用于将值赋给变量。

- `=`：简单赋值
- `+=`：加法赋值
- `-=`：减法赋值
- `*=`：乘法赋值
- `/=`：除法赋值
- `%=`：取模赋值

**代码示例：**

```javascript
let num = 10;

num += 5; // 等同于 num = num + 5
console.log(num); // 输出: 15

num -= 3; // 等同于 num = num - 3
console.log(num); // 输出: 12

num *= 2; // 等同于 num = num * 2
console.log(num); // 输出: 24

num /= 4; // 等同于 num = num / 4
console.log(num); // 输出: 6

num %= 5; // 等同于 num = num % 5
console.log(num); // 输出: 1
```

### 5. 三元运算符

三元运算符是一种简化的条件运算符，用于根据条件选择不同的值。

- `condition ? expr1 : expr2`：如果 `condition` 为真，返回 `expr1`，否则返回 `expr2`。

**代码示例：**

```javascript
let age = 18;
let message = age >= 18 ? '成年' : '未成年';

console.log(message); // 输出: 成年
```

## 表达式

表达式是任何可以计算出一个值的代码片段。它可以是简单的数值、变量，也可以是复杂的运算组合。

**代码示例：**

```javascript
let a = 5;
let b = 10;

let sum = a + b; // sum 是一个表达式，计算结果为 15
let isGreater = a > b; // isGreater 是一个表达式，计算结果为 false
```

## 实践练习

### 练习1：计算圆的面积

编写一个程序，计算并输出圆的面积。圆的面积公式为 `π * r^2`，其中 `r` 是半径。

**提示：** 使用 `Math.PI` 获取 π 的值。

```javascript
let radius = 7;
let area = Math.PI * radius ** 2;

console.log('圆的面积是: ' + area);
```

### 练习2：判断闰年

编写一个程序，判断给定的年份是否是闰年。闰年的条件是：
- 年份能被4整除且不能被100整除，或者能被400整除。

**提示：** 使用逻辑运算符 `&&` 和 `||`。

```javascript
let year = 2024;
let isLeapYear = (year % 4 === 0 && year % 100 !== 0) || (year % 400 === 0);

console.log(year + ' 是闰年吗？ ' + isLeapYear);
```

### 练习3：温度转换

编写一个程序，将摄氏温度转换为华氏温度。转换公式为 `F = C * 9/5 + 32`。

```javascript
let celsius = 25;
let fahrenheit = celsius * 9/5 + 32;

console.log(celsius + ' 摄氏度等于 ' + fahrenheit + ' 华氏度');
```

## 总结

运算符和表达式是编程中的基本构建块。通过掌握各种运算符的使用，你可以构建复杂的逻辑和计算。表达式则是这些运算符和操作数的组合，用于计算出最终的值。通过实践练习，你可以更好地理解和应用这些概念。

继续学习，你将能够使用这些基础知识构建更复杂的程序！