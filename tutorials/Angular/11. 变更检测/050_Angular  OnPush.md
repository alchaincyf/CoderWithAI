---
title: 深入理解 Angular 中的 OnPush 策略
date: 2023-10-05
description: 本课程详细讲解 Angular 中的 OnPush 策略，帮助开发者优化应用性能，减少不必要的视图更新。
slug: angular-onpush-strategy
tags:
  - Angular
  - 性能优化
  - 前端开发
category: 前端开发
keywords:
  - Angular OnPush
  - 性能优化
  - Angular 视图更新
---

# OnPush 策略

## 1. 简介

在 Angular 中，变更检测是框架自动更新视图以反映模型变化的过程。默认情况下，Angular 使用 `ChangeDetectionStrategy.Default` 策略，这意味着每当任何组件或服务中的数据发生变化时，Angular 都会检查整个组件树以确保视图与模型同步。然而，这种策略在大型应用中可能会导致性能问题，因为每次变更检测都会遍历整个组件树。

为了优化性能，Angular 提供了 `ChangeDetectionStrategy.OnPush` 策略。使用 `OnPush` 策略的组件只有在以下情况下才会触发变更检测：

1. 输入属性（`@Input`）发生变化。
2. 组件或其子组件触发事件。
3. 手动调用 `ChangeDetectorRef.detectChanges()`。

## 2. 理论解释

### 2.1 默认变更检测策略

默认情况下，Angular 使用 `ChangeDetectionStrategy.Default` 策略。这意味着每当任何数据发生变化时，Angular 都会检查整个组件树。这种策略适用于小型应用，但在大型应用中可能会导致性能问题。

### 2.2 OnPush 变更检测策略

`ChangeDetectionStrategy.OnPush` 策略通过限制变更检测的触发条件来提高性能。使用 `OnPush` 策略的组件只有在以下情况下才会触发变更检测：

- **输入属性变化**：当组件的输入属性发生变化时，Angular 会触发变更检测。
- **事件触发**：当组件或其子组件触发事件时，Angular 会触发变更检测。
- **手动触发**：通过调用 `ChangeDetectorRef.detectChanges()` 手动触发变更检测。

### 2.3 为什么使用 OnPush 策略？

使用 `OnPush` 策略可以显著减少不必要的变更检测，从而提高应用的性能。特别是在大型应用中，减少变更检测的频率可以避免不必要的计算和视图更新。

## 3. 代码示例

### 3.1 创建一个使用 OnPush 策略的组件

首先，我们创建一个简单的组件，并将其变更检测策略设置为 `OnPush`。

```typescript
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';

@Component({
  selector: 'app-on-push-example',
  template: `
    <div>
      <p>Name: {{ name }}</p>
      <p>Age: {{ age }}</p>
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class OnPushExampleComponent {
  @Input() name: string;
  @Input() age: number;
}
```

### 3.2 在父组件中使用 OnPush 组件

接下来，我们在父组件中使用这个 `OnPush` 组件，并观察其行为。

```typescript
import { Component } from '@angular/core';

@Component({
  selector: 'app-parent',
  template: `
    <button (click)="updateName()">Update Name</button>
    <app-on-push-example [name]="name" [age]="age"></app-on-push-example>
  `
})
export class ParentComponent {
  name = 'John';
  age = 30;

  updateName() {
    this.name = 'Jane';
  }
}
```

### 3.3 观察行为

当我们点击“Update Name”按钮时，`name` 属性的值会发生变化。由于 `OnPushExampleComponent` 使用了 `OnPush` 策略，只有当输入属性 `name` 发生变化时，组件才会触发变更检测并更新视图。

## 4. 实践练习

### 4.1 创建一个简单的计数器应用

1. 创建一个父组件 `CounterComponent`，其中包含一个计数器变量 `count`。
2. 创建一个子组件 `CounterDisplayComponent`，使用 `OnPush` 策略，并接收 `count` 作为输入属性。
3. 在 `CounterComponent` 中添加一个按钮，每次点击按钮时增加 `count` 的值。
4. 观察 `CounterDisplayComponent` 的视图更新行为。

### 4.2 代码实现

```typescript
// counter.component.ts
import { Component } from '@angular/core';

@Component({
  selector: 'app-counter',
  template: `
    <button (click)="increment()">Increment</button>
    <app-counter-display [count]="count"></app-counter-display>
  `
})
export class CounterComponent {
  count = 0;

  increment() {
    this.count++;
  }
}

// counter-display.component.ts
import { Component, Input, ChangeDetectionStrategy } from '@angular/core';

@Component({
  selector: 'app-counter-display',
  template: `
    <p>Count: {{ count }}</p>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class CounterDisplayComponent {
  @Input() count: number;
}
```

### 4.3 观察结果

每次点击“Increment”按钮时，`count` 的值会增加。由于 `CounterDisplayComponent` 使用了 `OnPush` 策略，只有当输入属性 `count` 发生变化时，组件才会触发变更检测并更新视图。

## 5. 手动触发变更检测

在某些情况下，你可能需要手动触发变更检测。Angular 提供了 `ChangeDetectorRef` 服务，允许你手动调用 `detectChanges()` 方法。

### 5.1 示例代码

```typescript
import { Component, ChangeDetectorRef } from '@angular/core';

@Component({
  selector: 'app-manual-detection',
  template: `
    <p>Count: {{ count }}</p>
    <button (click)="increment()">Increment</button>
  `
})
export class ManualDetectionComponent {
  count = 0;

  constructor(private cdr: ChangeDetectorRef) {}

  increment() {
    this.count++;
    this.cdr.detectChanges(); // 手动触发变更检测
  }
}
```

### 5.2 观察行为

在这个示例中，每次点击“Increment”按钮时，`count` 的值会增加，并且手动调用 `detectChanges()` 方法会触发变更检测，更新视图。

## 6. 性能优化技巧

### 6.1 使用不可变数据结构

在使用 `OnPush` 策略时，确保输入属性是不可变的。Angular 通过比较输入属性的引用（而不是值）来确定是否需要触发变更检测。因此，使用不可变数据结构（如 `Immutable.js`）可以确保输入属性的引用发生变化，从而触发变更检测。

### 6.2 减少事件触发

尽量减少事件触发的频率，特别是在大型应用中。可以通过合并事件、使用防抖或节流技术来减少事件触发的次数。

### 6.3 使用 `markForCheck()`

在某些情况下，你可能希望在异步操作完成后触发变更检测。可以使用 `ChangeDetectorRef.markForCheck()` 方法，它会在当前或下一个变更检测周期中触发变更检测。

```typescript
import { Component, ChangeDetectionStrategy, ChangeDetectorRef } from '@angular/core';

@Component({
  selector: 'app-async-example',
  template: `
    <p>Data: {{ data }}</p>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class AsyncExampleComponent {
  data: string;

  constructor(private cdr: ChangeDetectorRef) {}

  ngOnInit() {
    setTimeout(() => {
      this.data = 'Async Data';
      this.cdr.markForCheck(); // 标记为需要检查
    }, 1000);
  }
}
```

## 7. 总结

`OnPush` 策略是 Angular 中优化性能的重要工具。通过限制变更检测的触发条件，可以显著减少不必要的计算和视图更新，从而提高应用的性能。在使用 `OnPush` 策略时，确保输入属性是不可变的，并合理使用手动触发变更检测的方法，可以进一步优化应用的性能。

通过本教程的学习，你应该能够理解 `OnPush` 策略的工作原理，并在实际项目中应用它来优化 Angular 应用的性能。