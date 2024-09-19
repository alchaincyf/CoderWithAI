---
title: 跳转语句 (break, continue) 详解
date: 2023-10-05
description: 本课程详细讲解编程中的跳转语句，包括break和continue的使用方法及其在不同编程语言中的应用。
slug: jump-statements-break-continue
tags:
  - 编程基础
  - 控制结构
  - 循环语句
category: 编程基础
keywords:
  - break语句
  - continue语句
  - 跳转语句
---

# 跳转语句 (break, continue)

在编程中，跳转语句用于控制程序的流程。PHP 提供了两种主要的跳转语句：`break` 和 `continue`。这些语句在循环结构中特别有用，可以帮助我们更灵活地控制程序的执行。

## 1. `break` 语句

`break` 语句用于立即终止当前的循环结构（如 `for`、`while`、`do-while` 或 `switch`）。一旦执行到 `break` 语句，程序将跳出当前循环或 `switch` 语句，继续执行后续的代码。

### 1.1 理论解释

`break` 语句通常用于在满足某个条件时提前结束循环。这在处理一些需要提前终止的场景中非常有用，比如在找到特定元素后不再继续遍历数组。

### 1.2 代码示例

```php
<?php
for ($i = 0; $i < 10; $i++) {
    if ($i == 5) {
        break; // 当 $i 等于 5 时，跳出循环
    }
    echo "当前的 i 值是: $i\n";
}
echo "循环结束后的 i 值是: $i\n";
?>
```

### 1.3 实践练习

编写一个程序，使用 `for` 循环遍历一个数组，当找到第一个负数时，使用 `break` 语句跳出循环。

```php
<?php
$numbers = [1, 2, 3, -4, 5, 6];
foreach ($numbers as $number) {
    if ($number < 0) {
        echo "找到负数: $number\n";
        break;
    }
    echo "当前的数字是: $number\n";
}
?>
```

## 2. `continue` 语句

`continue` 语句用于跳过当前循环的剩余部分，直接进入下一次循环。它通常用于在满足某个条件时跳过某些代码的执行，但不会终止整个循环。

### 2.1 理论解释

`continue` 语句可以帮助我们跳过循环中的某些特定情况，而不必完全终止循环。这在处理一些需要忽略某些特定值的场景中非常有用。

### 2.2 代码示例

```php
<?php
for ($i = 0; $i < 10; $i++) {
    if ($i % 2 == 0) {
        continue; // 跳过偶数，继续下一次循环
    }
    echo "当前的 i 值是: $i\n";
}
?>
```

### 2.3 实践练习

编写一个程序，使用 `while` 循环遍历一个数组，当遇到偶数时，使用 `continue` 语句跳过该次循环，只输出奇数。

```php
<?php
$numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
$i = 0;
while ($i < count($numbers)) {
    if ($numbers[$i] % 2 == 0) {
        $i++;
        continue;
    }
    echo "当前的数字是: $numbers[$i]\n";
    $i++;
}
?>
```

## 3. 综合应用

在实际编程中，`break` 和 `continue` 语句通常结合使用，以实现更复杂的逻辑控制。例如，在一个嵌套循环中，可以使用 `break` 跳出内层循环，或者使用 `continue` 跳过内层循环的某些部分。

### 3.1 代码示例

```php
<?php
for ($i = 0; $i < 3; $i++) {
    for ($j = 0; $j < 3; $j++) {
        if ($i == 1 && $j == 1) {
            break; // 当 $i 和 $j 都为 1 时，跳出内层循环
        }
        echo "i = $i, j = $j\n";
    }
}
?>
```

### 3.2 实践练习

编写一个程序，使用嵌套循环生成一个 3x3 的矩阵，当遇到矩阵中的某个特定值时，使用 `continue` 语句跳过该次内层循环的输出。

```php
<?php
for ($i = 0; $i < 3; $i++) {
    for ($j = 0; $j < 3; $j++) {
        if ($i == 1 && $j == 1) {
            continue; // 当 $i 和 $j 都为 1 时，跳过该次内层循环
        }
        echo "i = $i, j = $j\n";
    }
}
?>
```

## 4. 总结

`break` 和 `continue` 语句是控制循环流程的重要工具。`break` 用于完全终止循环，而 `continue` 用于跳过当前循环的剩余部分。通过合理使用这两种语句，可以编写出更加灵活和高效的代码。

希望这篇教程能帮助你更好地理解和应用 `break` 和 `continue` 语句。继续练习和实践，你将能够熟练掌握这些重要的编程概念。