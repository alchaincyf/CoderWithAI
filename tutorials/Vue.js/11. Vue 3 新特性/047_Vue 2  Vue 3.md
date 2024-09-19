---
title: 从 Vue 2 迁移到 Vue 3：全面指南
date: 2023-10-05
description: 本课程详细介绍了如何将现有的 Vue 2 项目迁移到 Vue 3，涵盖了新特性和最佳实践，帮助开发者顺利过渡。
slug: vue2-to-vue3-migration
tags:
  - Vue.js
  - 前端开发
  - 迁移指南
category: 前端开发
keywords:
  - Vue 2
  - Vue 3
  - 迁移
  - 前端框架
  - JavaScript
---

# 从 Vue 2 迁移到 Vue 3

## 1. Vue.js 简介和特性

Vue.js 是一个渐进式 JavaScript 框架，用于构建用户界面。Vue 2 和 Vue 3 都是 Vue.js 的版本，但 Vue 3 在性能、开发体验和功能上都有显著的提升。

### 1.1 Vue 2 的主要特性
- 响应式数据绑定
- 组件化开发
- 模板语法
- 生命周期钩子
- Vue Router 和 Vuex 状态管理

### 1.2 Vue 3 的新特性
- Composition API
- Teleport 组件
- Fragments
- 更好的 TypeScript 支持
- 性能优化

## 2. 开发环境搭建

### 2.1 使用 Vue CLI
Vue CLI 是一个官方提供的脚手架工具，用于快速搭建 Vue 项目。

```bash
npm install -g @vue/cli
vue create my-vue-app
```

### 2.2 使用 Vite
Vite 是一个新的前端构建工具，速度更快，更适合现代开发。

```bash
npm init vite@latest my-vue-app --template vue
```

## 3. 创建第一个 Vue 应用

### 3.1 Vue 2 示例
```html
<div id="app">
  {{ message }}
</div>

<script>
new Vue({
  el: '#app',
  data: {
    message: 'Hello Vue 2!'
  }
})
</script>
```

### 3.2 Vue 3 示例
```html
<div id="app">
  {{ message }}
</div>

<script>
import { createApp } from 'vue'

const app = createApp({
  data() {
    return {
      message: 'Hello Vue 3!'
    }
  }
})

app.mount('#app')
</script>
```

## 4. Vue 实例和生命周期

### 4.1 Vue 2 生命周期钩子
- `beforeCreate`
- `created`
- `beforeMount`
- `mounted`
- `beforeUpdate`
- `updated`
- `beforeDestroy`
- `destroyed`

### 4.2 Vue 3 生命周期钩子
- `beforeCreate` -> `setup()`
- `created` -> `setup()`
- `beforeMount` -> `onBeforeMount`
- `mounted` -> `onMounted`
- `beforeUpdate` -> `onBeforeUpdate`
- `updated` -> `onUpdated`
- `beforeUnmount` -> `onBeforeUnmount`
- `unmounted` -> `onUnmounted`

## 5. 模板语法和数据绑定

### 5.1 Vue 2 数据绑定
```html
<div id="app">
  <p>{{ message }}</p>
  <input v-model="message">
</div>
```

### 5.2 Vue 3 数据绑定
```html
<div id="app">
  <p>{{ message }}</p>
  <input v-model="message">
</div>
```

## 6. 组件化开发

### 6.1 Vue 2 组件示例
```javascript
Vue.component('my-component', {
  template: '<div>{{ message }}</div>',
  data() {
    return {
      message: 'Hello from Vue 2 component!'
    }
  }
})
```

### 6.2 Vue 3 组件示例
```javascript
const MyComponent = {
  template: '<div>{{ message }}</div>',
  setup() {
    return {
      message: 'Hello from Vue 3 component!'
    }
  }
}

createApp({}).component('my-component', MyComponent).mount('#app')
```

## 7. 组件通信

### 7.1 Props 和 Events
- Vue 2 和 Vue 3 的 `props` 和 `events` 使用方式基本相同。

### 7.2 Provide/Inject
- Vue 3 中的 `provide` 和 `inject` 更加灵活。

```javascript
// 父组件
provide: {
  message: 'Hello from parent'
}

// 子组件
inject: ['message']
```

## 8. 计算属性和侦听器

### 8.1 Vue 2 计算属性
```javascript
computed: {
  reversedMessage() {
    return this.message.split('').reverse().join('')
  }
}
```

### 8.2 Vue 3 计算属性
```javascript
import { computed } from 'vue'

setup() {
  const reversedMessage = computed(() => {
    return this.message.split('').reverse().join('')
  })

  return {
    reversedMessage
  }
}
```

## 9. 条件渲染和列表渲染

### 9.1 Vue 2 条件渲染
```html
<div v-if="show">Show this</div>
<div v-else>Show that</div>
```

### 9.2 Vue 3 条件渲染
```html
<div v-if="show">Show this</div>
<div v-else>Show that</div>
```

## 10. 表单输入绑定

### 10.1 Vue 2 表单输入绑定
```html
<input v-model="message">
```

### 10.2 Vue 3 表单输入绑定
```html
<input v-model="message">
```

## 11. 单文件组件 (.vue 文件)

### 11.1 Vue 2 单文件组件
```vue
<template>
  <div>{{ message }}</div>
</template>

<script>
export default {
  data() {
    return {
      message: 'Hello Vue 2!'
    }
  }
}
</script>
```

### 11.2 Vue 3 单文件组件
```vue
<template>
  <div>{{ message }}</div>
</template>

<script>
import { ref } from 'vue'

export default {
  setup() {
    const message = ref('Hello Vue 3!')
    return { message }
  }
}
</script>
```

## 12. 组件注册 (全局和局部)

### 12.1 Vue 2 全局组件
```javascript
Vue.component('my-component', {
  template: '<div>Global Component</div>'
})
```

### 12.2 Vue 3 全局组件
```javascript
const MyComponent = {
  template: '<div>Global Component</div>'
}

createApp({}).component('my-component', MyComponent).mount('#app')
```

## 13. Scoped CSS

### 13.1 Vue 2 Scoped CSS
```vue
<style scoped>
div {
  color: red;
}
</style>
```

### 13.2 Vue 3 Scoped CSS
```vue
<style scoped>
div {
  color: red;
}
</style>
```

## 14. CSS 预处理器 (Sass, Less)

### 14.1 Vue 2 使用 Sass
```bash
npm install sass-loader sass --save-dev
```

### 14.2 Vue 3 使用 Sass
```bash
npm install sass-loader sass --save-dev
```

## 15. 动态样式和类绑定

### 15.1 Vue 2 动态样式
```html
<div :class="{ active: isActive }">Dynamic Class</div>
```

### 15.2 Vue 3 动态样式
```html
<div :class="{ active: isActive }">Dynamic Class</div>
```

## 16. Vue Router

### 16.1 Vue 2 路由配置
```javascript
import Vue from 'vue'
import VueRouter from 'vue-router'

Vue.use(VueRouter)

const routes = [
  { path: '/', component: Home },
  { path: '/about', component: About }
]

const router = new VueRouter({
  routes
})

new Vue({
  router
}).$mount('#app')
```

### 16.2 Vue 3 路由配置
```javascript
import { createApp } from 'vue'
import { createRouter, createWebHistory } from 'vue-router'

const routes = [
  { path: '/', component: Home },
  { path: '/about', component: About }
]

const router = createRouter({
  history: createWebHistory(),
  routes
})

createApp({}).use(router).mount('#app')
```

## 17. Vuex 状态管理

### 17.1 Vue 2 Vuex 配置
```javascript
import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

const store = new Vuex.Store({
  state: {
    count: 0
  },
  mutations: {
    increment(state) {
      state.count++
    }
  }
})

new Vue({
  store
}).$mount('#app')
```

### 17.2 Vue 3 Vuex 配置
```javascript
import { createApp } from 'vue'
import { createStore } from 'vuex'

const store = createStore({
  state: {
    count: 0
  },
  mutations: {
    increment(state) {
      state.count++
    }
  }
})

createApp({}).use(store).mount('#app')
```

## 18. 混入 (Mixins)

### 18.1 Vue 2 混入
```javascript
const myMixin = {
  created() {
    console.log('Mixin created')
  }
}

new Vue({
  mixins: [myMixin]
})
```

### 18.2 Vue 3 混入
```javascript
const myMixin = {
  created() {
    console.log('Mixin created')
  }
}

createApp({
  mixins: [myMixin]
}).mount('#app')
```

## 19. 自定义指令

### 19.1 Vue 2 自定义指令
```javascript
Vue.directive('focus', {
  inserted: function(el) {
    el.focus()
  }
})
```

### 19.2 Vue 3 自定义指令
```javascript
const focus = {
  mounted(el) {
    el.focus()
  }
}

createApp({}).directive('focus', focus).mount('#app')
```

## 20. 插件系统

### 20.1 Vue 2 插件
```javascript
const MyPlugin = {
  install(Vue) {
    Vue.mixin({
      created() {
        console.log('Plugin installed')
      }
    })
  }
}

Vue.use(MyPlugin)
```

### 20.2 Vue 3 插件
```javascript
const MyPlugin = {
  install(app) {
    app.mixin({
      created() {
        console.log('Plugin installed')
      }
    })
  }
}

createApp({}).use(MyPlugin).mount('#app')
```

## 21. 虚拟 DOM 和 diff 算法

### 21.1 Vue 2 虚拟 DOM
Vue 2 使用虚拟 DOM 来提高渲染性能。

### 21.2 Vue 3 虚拟 DOM
Vue 3 对虚拟 DOM 进行了优化，性能更好。

## 22. 异步组件

### 22.1 Vue 2 异步组件
```javascript
const AsyncComponent = () => ({
  component: import('./AsyncComponent.vue'),
  loading: LoadingComponent,
  error: ErrorComponent,
  delay: 200,
  timeout: 3000
})
```

### 22.2 Vue 3 异步组件
```javascript
const AsyncComponent = defineAsyncComponent(() =>
  import('./AsyncComponent.vue')
)
```

## 23. keep-alive 组件缓存

### 23.1 Vue 2 keep-alive
```html
<keep-alive>
  <component :is="currentComponent"></component>
</keep-alive>
```

### 23.2 Vue 3 keep-alive
```html
<keep-alive>
  <component :is="currentComponent"></component>
</keep-alive>
```

## 24. 懒加载和代码分割

### 24.1 Vue 2 懒加载
```javascript
const Home = () => import('./Home.vue')
```

### 24.2 Vue 3 懒加载
```javascript
const Home = defineAsyncComponent(() => import('./Home.vue'))
```

## 25. Vue DevTools

### 25.1 Vue 2 DevTools
Vue 2 使用 Vue DevTools 进行调试。

### 25.2 Vue 3 DevTools
Vue 3 使用 Vue DevTools 进行调试。

## 26. Vue Test Utils

### 26.1 Vue 2 测试
```javascript
import { mount } from '@vue/test-utils'
import MyComponent from './MyComponent.vue'

test('renders correctly', () => {
  const wrapper = mount(MyComponent)
  expect(wrapper.text()).toContain('Hello')
})
```

### 26.2 Vue 3 测试
```javascript
import { mount } from '@vue/test-utils'
import MyComponent from './MyComponent.vue'

test('renders correctly', () => {
  const wrapper = mount(MyComponent)
  expect(wrapper.text()).toContain('Hello')
})
```

## 27. Vue CLI

### 27.1 Vue 2 CLI
```bash
vue create my-project
```

### 27.2 Vue 3 CLI
```bash
vue create my-project
```

## 28. Nuxt.js (Vue.js 服务端渲染框架)

### 28.1 Nuxt.js 简介
Nuxt.js 是一个基于 Vue.js 的服务端渲染框架。

### 28.2 Nuxt.js 安装
```bash
npx create-nuxt-app my-nuxt-app
```

## 29. 项目结构组织

### 29.1 Vue 2 项目结构
```
my-project/
├── public/
├── src/
│   ├── assets/
│   ├── components/
│   ├── views/
│   ├── App.vue
│   ├── main.js
├── package.json
```

### 29.2 Vue 3 项目结构
```
my-project/
├── public/
├── src/
│   ├── assets/
│   ├── components/
│   ├── views/
│   ├── App.vue
│   ├── main.js
├── package.json
```

## 30. 代码风格和规范

### 30.1 ESLint 和 Prettier
```bash
npm install eslint prettier eslint-plugin-vue --save-dev
```

### 30.2 配置文件
```json
{
  "extends": [
    "eslint:recommended",
    "plugin:vue/vue3-recommended"
  ],
  "rules": {
    "vue/no-unused-vars": "error"
  }
}
```

## 31. 错误处理和调试技巧

### 31.1 Vue 2 错误处理
```javascript
Vue.config.errorHandler = function (err, vm, info) {
  console.error(err)
}
```

### 31.2 Vue 3 错误处理
```javascript
app.config.errorHandler = (err, vm, info) => {
  console.error(err)
}
```

## 32. 性能优化策略

### 32.1 Vue 2 性能优化
- 使用 `v-once`
- 懒加载组件

### 32.2 Vue 3 性能优化
- 使用 `v-once`
- 懒加载组件
- 使用 `Suspense`

## 33. TypeScript 与 Vue

### 33.1 Vue 2 + TypeScript
```bash
vue add typescript
```

### 33.2 Vue 3 + TypeScript
```bash
vue add typescript
```

## 34. 渲染函数和 JSX

### 34.1 Vue 2 渲染函数
```javascript
Vue.component('my-component', {
  render(h) {
    return h('div', 'Hello')
  }
})
```

### 34.2