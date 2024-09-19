---
title: 组件通信 (Input 和 Output) 教程
date: 2023-10-05
description: 本课程详细讲解如何在Angular应用中使用Input和Output进行组件间的数据传递和事件通信。
slug: component-communication-input-output
tags:
  - Angular
  - 组件通信
  - 前端开发
category: 前端开发
keywords:
  - Angular组件通信
  - Input属性
  - Output事件
  - 前端开发
---

# 组件通信 (Input 和 Output)

在 Angular 中，组件是构建用户界面的基本单元。组件之间的通信是构建复杂应用的关键。本教程将详细介绍如何使用 `@Input` 和 `@Output` 装饰器在 Angular 组件之间进行数据传递和事件触发。

## 1. 理论解释

### 1.1 `@Input` 装饰器

`@Input` 装饰器用于将父组件的数据传递给子组件。子组件可以通过 `@Input` 装饰器接收父组件传递的数据，并在模板中使用这些数据。

### 1.2 `@Output` 装饰器

`@Output` 装饰器用于将子组件的事件传递给父组件。子组件可以通过 `@Output` 装饰器触发事件，父组件可以监听这些事件并做出相应的处理。

## 2. 代码示例

### 2.1 使用 `@Input` 传递数据

假设我们有一个父组件 `ParentComponent` 和一个子组件 `ChildComponent`。我们希望将父组件的数据传递给子组件。

#### 父组件 (`parent.component.ts`)

```typescript
import { Component } from '@angular/core';

@Component({
  selector: 'app-parent',
  template: `
    <app-child [message]="parentMessage"></app-child>
  `
})
export class ParentComponent {
  parentMessage = 'Hello from Parent!';
}
```

#### 子组件 (`child.component.ts`)

```typescript
import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-child',
  template: `
    <p>{{ message }}</p>
  `
})
export class ChildComponent {
  @Input() message: string;
}
```

### 2.2 使用 `@Output` 触发事件

假设我们希望子组件在用户点击按钮时触发一个事件，父组件监听该事件并做出响应。

#### 子组件 (`child.component.ts`)

```typescript
import { Component, Output, EventEmitter } from '@angular/core';

@Component({
  selector: 'app-child',
  template: `
    <button (click)="sendMessage()">Send Message to Parent</button>
  `
})
export class ChildComponent {
  @Output() messageEvent = new EventEmitter<string>();

  sendMessage() {
    this.messageEvent.emit('Hello from Child!');
  }
}
```

#### 父组件 (`parent.component.ts`)

```typescript
import { Component } from '@angular/core';

@Component({
  selector: 'app-parent',
  template: `
    <app-child (messageEvent)="receiveMessage($event)"></app-child>
    <p>{{ childMessage }}</p>
  `
})
export class ParentComponent {
  childMessage: string;

  receiveMessage(message: string) {
    this.childMessage = message;
  }
}
```

## 3. 实践练习

### 3.1 练习目标

创建一个简单的 Angular 应用，包含一个父组件和一个子组件。父组件传递一个数组给子组件，子组件显示数组中的内容。当用户点击子组件中的某个元素时，子组件触发一个事件，父组件接收该事件并显示被点击的元素。

### 3.2 步骤

1. **创建 Angular 项目**：使用 Angular CLI 创建一个新的 Angular 项目。

   ```bash
   ng new component-communication-practice
   cd component-communication-practice
   ```

2. **生成父组件和子组件**：使用 Angular CLI 生成父组件和子组件。

   ```bash
   ng generate component parent
   ng generate component child
   ```

3. **在父组件中定义数组**：在 `parent.component.ts` 中定义一个数组，并将其传递给子组件。

   ```typescript
   import { Component } from '@angular/core';

   @Component({
     selector: 'app-parent',
     template: `
       <app-child [items]="items" (itemClicked)="onItemClicked($event)"></app-child>
       <p>Clicked Item: {{ clickedItem }}</p>
     `
   })
   export class ParentComponent {
     items = ['Item 1', 'Item 2', 'Item 3'];
     clickedItem: string;

     onItemClicked(item: string) {
       this.clickedItem = item;
     }
   }
   ```

4. **在子组件中接收数组并触发事件**：在 `child.component.ts` 中接收数组，并在用户点击某个元素时触发事件。

   ```typescript
   import { Component, Input, Output, EventEmitter } from '@angular/core';

   @Component({
     selector: 'app-child',
     template: `
       <ul>
         <li *ngFor="let item of items" (click)="clickItem(item)">{{ item }}</li>
       </ul>
     `
   })
   export class ChildComponent {
     @Input() items: string[];
     @Output() itemClicked = new EventEmitter<string>();

     clickItem(item: string) {
       this.itemClicked.emit(item);
     }
   }
   ```

5. **运行应用**：使用 Angular CLI 运行应用，查看结果。

   ```bash
   ng serve
   ```

## 4. 总结

通过本教程，我们学习了如何在 Angular 中使用 `@Input` 和 `@Output` 装饰器进行组件之间的通信。`@Input` 用于从父组件向子组件传递数据，而 `@Output` 用于从子组件向父组件传递事件。掌握这些基本概念和技巧，将帮助你构建更加复杂和交互性强的 Angular 应用。

希望本教程对你有所帮助，继续探索 Angular 的更多功能和特性吧！