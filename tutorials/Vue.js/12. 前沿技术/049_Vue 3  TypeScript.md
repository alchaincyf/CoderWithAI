---
title: Vue 3 与 TypeScript 开发实战
date: 2023-10-05
description: 本课程深入探讨如何使用Vue 3与TypeScript构建现代化的前端应用，涵盖组件开发、状态管理、类型安全等关键技术。
slug: vue3-typescript-course
tags:
  - Vue 3
  - TypeScript
  - 前端开发
category: 前端开发
keywords:
  - Vue 3
  - TypeScript
  - 前端框架
---

# Vue 3 + TypeScript 教程

## 1. Vue.js 简介和特性

Vue.js 是一个渐进式 JavaScript 框架，用于构建用户界面。Vue 3 是 Vue.js 的最新版本，带来了许多新特性和性能改进。Vue 3 的核心特性包括：

- **响应式系统**：Vue 3 使用 Proxy 对象来实现更高效的响应式系统。
- **Composition API**：提供了一种新的组织代码的方式，使得逻辑复用和代码组织更加灵活。
- **更好的 TypeScript 支持**：Vue 3 对 TypeScript 的支持更加完善，使得类型检查更加严格和准确。

## 2. 开发环境搭建

### 2.1 Vue CLI

Vue CLI 是一个官方提供的脚手架工具，可以帮助你快速搭建 Vue 项目。

```bash
npm install -g @vue/cli
vue create my-vue-app
```

### 2.2 Vite

Vite 是一个新的前端构建工具，由 Vue 的作者尤雨溪开发。它提供了极快的冷启动和热更新。

```bash
npm init vite@latest my-vite-app --template vue-ts
```

## 3. 创建第一个 Vue 应用

### 3.1 使用 Vue CLI 创建项目

```bash
vue create my-first-app
cd my-first-app
npm run serve
```

### 3.2 使用 Vite 创建项目

```bash
npm init vite@latest my-first-app --template vue-ts
cd my-first-app
npm install
npm run dev
```

## 4. Vue 实例和生命周期

### 4.1 Vue 实例

Vue 实例是 Vue 应用的入口点。每个 Vue 应用都从一个 Vue 实例开始。

```typescript
import { createApp } from 'vue';
import App from './App.vue';

const app = createApp(App);
app.mount('#app');
```

### 4.2 生命周期钩子

Vue 实例有一系列的生命周期钩子，允许你在特定阶段执行代码。

```typescript
export default {
  data() {
    return {
      message: 'Hello Vue 3'
    };
  },
  beforeCreate() {
    console.log('beforeCreate');
  },
  created() {
    console.log('created');
  },
  beforeMount() {
    console.log('beforeMount');
  },
  mounted() {
    console.log('mounted');
  }
};
```

## 5. 模板语法和数据绑定

### 5.1 插值

使用双大括号 `{{ }}` 进行文本插值。

```html
<template>
  <div>{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      message: 'Hello Vue 3'
    };
  }
});
</script>
```

### 5.2 指令

Vue 提供了许多内置指令，如 `v-bind`、`v-if`、`v-for` 等。

```html
<template>
  <div v-if="show">This is visible</div>
  <div v-else>This is hidden</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      show: true
    };
  }
});
</script>
```

## 6. 组件化开发

### 6.1 创建组件

组件是 Vue 应用的基本构建块。你可以创建一个简单的组件并将其导入到主应用中。

```typescript
// MyComponent.vue
<template>
  <div>My Component</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  name: 'MyComponent'
});
</script>
```

### 6.2 使用组件

```typescript
// App.vue
<template>
  <MyComponent />
</template>

<script lang="ts">
import { defineComponent } from 'vue';
import MyComponent from './MyComponent.vue';

export default defineComponent({
  components: {
    MyComponent
  }
});
</script>
```

## 7. 组件通信

### 7.1 Props

Props 是父组件向子组件传递数据的方式。

```typescript
// ParentComponent.vue
<template>
  <ChildComponent :message="parentMessage" />
</template>

<script lang="ts">
import { defineComponent } from 'vue';
import ChildComponent from './ChildComponent.vue';

export default defineComponent({
  components: {
    ChildComponent
  },
  data() {
    return {
      parentMessage: 'Hello from parent'
    };
  }
});
</script>
```

```typescript
// ChildComponent.vue
<template>
  <div>{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent, PropType } from 'vue';

export default defineComponent({
  props: {
    message: {
      type: String as PropType<string>,
      required: true
    }
  }
});
</script>
```

### 7.2 Events

子组件可以通过 `$emit` 向父组件发送事件。

```typescript
// ChildComponent.vue
<template>
  <button @click="sendMessage">Send Message</button>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  methods: {
    sendMessage() {
      this.$emit('message-sent', 'Hello from child');
    }
  }
});
</script>
```

```typescript
// ParentComponent.vue
<template>
  <ChildComponent @message-sent="handleMessage" />
</template>

<script lang="ts">
import { defineComponent } from 'vue';
import ChildComponent from './ChildComponent.vue';

export default defineComponent({
  components: {
    ChildComponent
  },
  methods: {
    handleMessage(message: string) {
      console.log(message);
    }
  }
});
</script>
```

## 8. 计算属性和侦听器

### 8.1 计算属性

计算属性是基于依赖进行缓存的属性。

```typescript
<template>
  <div>{{ fullName }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      firstName: 'John',
      lastName: 'Doe'
    };
  },
  computed: {
    fullName(): string {
      return `${this.firstName} ${this.lastName}`;
    }
  }
});
</script>
```

### 8.2 侦听器

侦听器用于观察和响应数据的变化。

```typescript
<template>
  <div>{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      message: 'Hello Vue 3'
    };
  },
  watch: {
    message(newVal: string, oldVal: string) {
      console.log(`Message changed from ${oldVal} to ${newVal}`);
    }
  }
});
</script>
```

## 9. 条件渲染和列表渲染

### 9.1 条件渲染

使用 `v-if`、`v-else` 和 `v-show` 进行条件渲染。

```html
<template>
  <div v-if="show">This is visible</div>
  <div v-else>This is hidden</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      show: true
    };
  }
});
</script>
```

### 9.2 列表渲染

使用 `v-for` 进行列表渲染。

```html
<template>
  <ul>
    <li v-for="item in items" :key="item.id">{{ item.name }}</li>
  </ul>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      items: [
        { id: 1, name: 'Item 1' },
        { id: 2, name: 'Item 2' },
        { id: 3, name: 'Item 3' }
      ]
    };
  }
});
</script>
```

## 10. 表单输入绑定

### 10.1 单向绑定

使用 `v-model` 进行双向数据绑定。

```html
<template>
  <input v-model="message" />
  <div>{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      message: 'Hello Vue 3'
    };
  }
});
</script>
```

### 10.2 多选和单选

```html
<template>
  <input type="checkbox" v-model="checked" />
  <div>{{ checked }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      checked: false
    };
  }
});
</script>
```

## 11. 单文件组件 (.vue 文件)

单文件组件是 Vue 的核心特性之一，允许你将模板、脚本和样式封装在一个文件中。

```html
<template>
  <div class="example">{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      message: 'Hello Vue 3'
    };
  }
});
</script>

<style scoped>
.example {
  color: red;
}
</style>
```

## 12. 组件注册 (全局和局部)

### 12.1 全局注册

全局注册的组件可以在应用的任何地方使用。

```typescript
import { createApp } from 'vue';
import App from './App.vue';
import MyComponent from './MyComponent.vue';

const app = createApp(App);
app.component('MyComponent', MyComponent);
app.mount('#app');
```

### 12.2 局部注册

局部注册的组件只能在注册的组件中使用。

```typescript
<template>
  <MyComponent />
</template>

<script lang="ts">
import { defineComponent } from 'vue';
import MyComponent from './MyComponent.vue';

export default defineComponent({
  components: {
    MyComponent
  }
});
</script>
```

## 13. Scoped CSS

Scoped CSS 允许你为单文件组件中的样式设置作用域，避免样式冲突。

```html
<template>
  <div class="example">{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      message: 'Hello Vue 3'
    };
  }
});
</script>

<style scoped>
.example {
  color: red;
}
</style>
```

## 14. CSS 预处理器 (Sass, Less)

Vue 支持使用 Sass 和 Less 等 CSS 预处理器。

### 14.1 使用 Sass

```html
<template>
  <div class="example">{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      message: 'Hello Vue 3'
    };
  }
});
</script>

<style lang="scss" scoped>
$primary-color: red;

.example {
  color: $primary-color;
}
</style>
```

### 14.2 使用 Less

```html
<template>
  <div class="example">{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      message: 'Hello Vue 3'
    };
  }
});
</script>

<style lang="less" scoped>
@primary-color: red;

.example {
  color: @primary-color;
}
</style>
```

## 15. 动态样式和类绑定

### 15.1 动态类绑定

使用 `:class` 进行动态类绑定。

```html
<template>
  <div :class="{ active: isActive }">{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      message: 'Hello Vue 3',
      isActive: true
    };
  }
});
</script>

<style scoped>
.active {
  color: red;
}
</style>
```

### 15.2 动态样式绑定

使用 `:style` 进行动态样式绑定。

```html
<template>
  <div :style="{ color: textColor }">{{ message }}</div>
</template>

<script lang="ts">
import { defineComponent } from 'vue';

export default defineComponent({
  data() {
    return {
      message: 'Hello Vue 3',
      textColor: 'red'
    };
  }
});
</script>
```

## 16. Vue Router

Vue Router 是 Vue.js 的官方路由管理器。

### 16.1 安装 Vue Router

```bash
npm install vue-router@next
```

### 16.2 配置路由

```typescript
import { createRouter, createWebHistory } from 'vue-router';
import Home from './views/Home.vue';
import About from './views/About.vue';

const routes = [
  { path: '/', component: Home },
  { path: '/about', component: About }
];

const router = createRouter({
  history: createWebHistory(),
  routes
});

export default router;
```

### 16.3 使用路由

```typescript
import { createApp } from 'vue';
import App from './App.vue';
import router from './router';

const app = createApp(App);
app.use(router);
app.mount('#app');
```

## 17. Vuex 状态管理

Vuex 是 Vue.js 的官方状态管理库。

### 17.1 安装 Vuex

```bash
npm install vuex@next
```

### 17.2 配置 Vuex

```typescript
import { createStore } from 'vuex';

const store = createStore({
  state() {
    return {
      count: 0
    };
  },
  mutations: {
    increment(state) {
      state.count++;
    }
  }
});

export default store;
```

### 17.3 使用 Vuex

```typescript
import { createApp } from 'vue';
import App from './App.vue';
import store from './store';

const app = createApp(App);
app.use(store);
app.mount('#app');
```

## 18. 混入 (Mixins)

混入是一种代码复用机制，允许你将组件的选项混入到其他组件中。

```typescript
// myMixin.ts
export default {
  data() {
    return {
      message: 'Hello from mixin'
    };
  },
  methods: {
    greet() {
      console.log(this.message);
    }
  }
};
```

```typescript
// MyComponent.vue
<template>
  <button @click="greet">Greet</button>
</template>

<script lang="ts">
import { defineComponent } from 'vue';
import myMixin from './myMixin';

export default defineComponent({
  mixins: [myMixin]
});
</script>
```