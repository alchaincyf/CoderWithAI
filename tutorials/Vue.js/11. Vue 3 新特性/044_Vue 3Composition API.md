---
title: 深入理解Vue 3的Composition API
date: 2023-10-05
description: 本课程详细介绍了Vue 3中的Composition API，帮助开发者更好地组织和重用代码，提升开发效率。
slug: vue-3-composition-api
tags:
  - Vue.js
  - Composition API
  - 前端开发
category: 前端开发
keywords:
  - Vue 3
  - Composition API
  - 代码组织
---

# Composition API 教程

## 1. 概述

Composition API 是 Vue 3 引入的一种新的 API 风格，旨在解决 Vue 2 中 Options API 在处理复杂逻辑时的一些限制。Composition API 允许开发者将逻辑相关的代码组织在一起，提高了代码的可读性和可维护性。

### 1.1 为什么使用 Composition API？

- **逻辑复用**：Composition API 允许你将逻辑相关的代码封装成可复用的函数，而不是依赖于 mixins 或高阶组件。
- **更好的类型推断**：Composition API 与 TypeScript 结合使用时，提供了更好的类型推断。
- **更好的代码组织**：通过将逻辑相关的代码放在一起，使得代码更易于理解和维护。

## 2. 基本概念

### 2.1 `setup` 函数

`setup` 函数是 Composition API 的入口点。它会在组件创建之前执行，并返回一个对象，该对象中的属性将暴露给模板和其他选项。

```vue
<template>
  <div>{{ message }}</div>
</template>

<script>
import { ref } from 'vue';

export default {
  setup() {
    const message = ref('Hello, Vue 3!');

    return {
      message
    };
  }
};
</script>
```

### 2.2 响应式数据

在 Composition API 中，我们使用 `ref` 和 `reactive` 来创建响应式数据。

- `ref`：用于创建一个响应式的基本类型数据（如字符串、数字等）。
- `reactive`：用于创建一个响应式的对象。

```vue
<template>
  <div>
    <p>{{ count }}</p>
    <button @click="increment">Increment</button>
  </div>
</template>

<script>
import { ref } from 'vue';

export default {
  setup() {
    const count = ref(0);

    function increment() {
      count.value++;
    }

    return {
      count,
      increment
    };
  }
};
</script>
```

### 2.3 计算属性

使用 `computed` 函数可以创建计算属性。

```vue
<template>
  <div>
    <p>{{ fullName }}</p>
  </div>
</template>

<script>
import { ref, computed } from 'vue';

export default {
  setup() {
    const firstName = ref('John');
    const lastName = ref('Doe');

    const fullName = computed(() => `${firstName.value} ${lastName.value}`);

    return {
      fullName
    };
  }
};
</script>
```

### 2.4 侦听器

使用 `watch` 函数可以监听响应式数据的变化。

```vue
<template>
  <div>
    <input v-model="message" placeholder="Type something" />
    <p>{{ message }}</p>
  </div>
</template>

<script>
import { ref, watch } from 'vue';

export default {
  setup() {
    const message = ref('');

    watch(message, (newVal, oldVal) => {
      console.log(`Message changed from ${oldVal} to ${newVal}`);
    });

    return {
      message
    };
  }
};
</script>
```

## 3. 实践练习

### 3.1 创建一个简单的计数器

使用 Composition API 创建一个简单的计数器组件，包含增加和减少按钮。

```vue
<template>
  <div>
    <p>{{ count }}</p>
    <button @click="increment">Increment</button>
    <button @click="decrement">Decrement</button>
  </div>
</template>

<script>
import { ref } from 'vue';

export default {
  setup() {
    const count = ref(0);

    function increment() {
      count.value++;
    }

    function decrement() {
      count.value--;
    }

    return {
      count,
      increment,
      decrement
    };
  }
};
</script>
```

### 3.2 创建一个响应式表单

创建一个包含输入框和提交按钮的表单，使用 Composition API 管理表单数据。

```vue
<template>
  <div>
    <input v-model="name" placeholder="Enter your name" />
    <input v-model="email" placeholder="Enter your email" />
    <button @click="submitForm">Submit</button>
  </div>
</template>

<script>
import { ref } from 'vue';

export default {
  setup() {
    const name = ref('');
    const email = ref('');

    function submitForm() {
      console.log(`Name: ${name.value}, Email: ${email.value}`);
    }

    return {
      name,
      email,
      submitForm
    };
  }
};
</script>
```

## 4. 总结

Composition API 是 Vue 3 中一个强大的工具，它提供了更灵活、更强大的方式来组织和复用代码。通过本教程，你应该已经掌握了 Composition API 的基本概念和使用方法。接下来，你可以尝试在实际项目中应用这些知识，进一步提升你的 Vue 开发技能。

## 5. 进阶学习

- **深入学习 `reactive` 和 `ref` 的区别**：了解何时使用 `reactive` 和 `ref`，以及它们的优缺点。
- **探索 `watchEffect`**：学习如何使用 `watchEffect` 来自动追踪依赖并执行副作用。
- **使用 `provide` 和 `inject`**：了解如何在 Composition API 中使用 `provide` 和 `inject` 进行组件间的数据传递。

通过不断实践和学习，你将能够更好地掌握 Composition API，并在 Vue 3 项目中发挥其强大的功能。