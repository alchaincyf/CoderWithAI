---
title: 深入理解Angular中的路由守卫
date: 2023-10-05
description: 本课程详细讲解Angular中的路由守卫机制，帮助开发者掌握如何通过路由守卫实现权限控制和导航保护。
slug: angular-route-guards
tags:
  - Angular
  - 路由守卫
  - 权限控制
category: 前端开发
keywords:
  - Angular路由守卫
  - 导航保护
  - 权限管理
---

# 路由守卫

## 概述

在 Angular 应用中，路由守卫（Route Guards）是一种强大的工具，用于控制用户是否可以导航到某个路由。它们通常用于权限控制、数据预加载、确认对话框等场景。路由守卫可以阻止或允许用户访问特定的路由，从而增强应用的安全性和用户体验。

## 路由守卫的类型

Angular 提供了几种不同类型的路由守卫：

1. **CanActivate**: 决定是否可以激活某个路由。
2. **CanActivateChild**: 决定是否可以激活某个路由的子路由。
3. **CanDeactivate**: 决定是否可以离开某个路由。
4. **Resolve**: 在路由激活之前获取路由所需的数据。
5. **CanLoad**: 决定是否可以加载某个惰性加载的模块。

## 创建一个简单的路由守卫

### 1. 创建守卫

首先，我们使用 Angular CLI 创建一个新的守卫：

```bash
ng generate guard auth
```

这将在 `src/app` 目录下生成一个名为 `auth.guard.ts` 的文件。

### 2. 实现守卫逻辑

在 `auth.guard.ts` 文件中，我们可以实现 `CanActivate` 接口来控制用户是否可以访问某个路由。例如，我们可以检查用户是否已登录：

```typescript
import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, UrlTree, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { AuthService } from './auth.service';

@Injectable({
  providedIn: 'root'
})
export class AuthGuard implements CanActivate {

  constructor(private authService: AuthService, private router: Router) {}

  canActivate(
    route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot
  ): Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {
    if (this.authService.isLoggedIn()) {
      return true;
    } else {
      this.router.navigate(['/login']);
      return false;
    }
  }
}
```

### 3. 配置路由

在 `app-routing.module.ts` 中，我们可以将这个守卫应用到某个路由上：

```typescript
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { HomeComponent } from './home/home.component';
import { LoginComponent } from './login/login.component';
import { AuthGuard } from './auth.guard';

const routes: Routes = [
  { path: 'home', component: HomeComponent, canActivate: [AuthGuard] },
  { path: 'login', component: LoginComponent },
  { path: '', redirectTo: '/home', pathMatch: 'full' }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
```

### 4. 实践练习

1. **创建一个简单的登录页面**：在 `login` 组件中，实现一个简单的登录表单，用户输入用户名和密码后，调用 `AuthService` 的 `login` 方法。
2. **实现 `AuthService`**：在 `auth.service.ts` 中，实现 `login` 和 `isLoggedIn` 方法。`login` 方法可以简单地将用户信息存储在 `localStorage` 中，`isLoggedIn` 方法检查 `localStorage` 中是否有用户信息。
3. **测试路由守卫**：启动应用，尝试直接访问 `/home` 路由，观察是否被重定向到 `/login` 页面。

## 其他类型的路由守卫

### CanActivateChild

`CanActivateChild` 守卫用于控制子路由的访问权限。它的实现方式与 `CanActivate` 类似：

```typescript
import { Injectable } from '@angular/core';
import { CanActivateChild, ActivatedRouteSnapshot, RouterStateSnapshot, UrlTree, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { AuthService } from './auth.service';

@Injectable({
  providedIn: 'root'
})
export class AuthChildGuard implements CanActivateChild {

  constructor(private authService: AuthService, private router: Router) {}

  canActivateChild(
    childRoute: ActivatedRouteSnapshot,
    state: RouterStateSnapshot
  ): Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {
    if (this.authService.isLoggedIn()) {
      return true;
    } else {
      this.router.navigate(['/login']);
      return false;
    }
  }
}
```

### CanDeactivate

`CanDeactivate` 守卫用于在用户离开某个路由时进行确认。例如，当用户在表单中输入了数据但尚未保存时，可以弹出确认对话框：

```typescript
import { Injectable } from '@angular/core';
import { CanDeactivate, ActivatedRouteSnapshot, RouterStateSnapshot, UrlTree } from '@angular/router';
import { Observable } from 'rxjs';
import { FormComponent } from './form/form.component';

@Injectable({
  providedIn: 'root'
})
export class DeactivateGuard implements CanDeactivate<FormComponent> {

  canDeactivate(
    component: FormComponent,
    currentRoute: ActivatedRouteSnapshot,
    currentState: RouterStateSnapshot,
    nextState?: RouterStateSnapshot
  ): Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {
    if (component.form.dirty) {
      return confirm('You have unsaved changes. Are you sure you want to leave?');
    }
    return true;
  }
}
```

### Resolve

`Resolve` 守卫用于在路由激活之前获取数据。例如，我们可以在用户访问某个路由之前，预先加载一些数据：

```typescript
import { Injectable } from '@angular/core';
import { Resolve, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { Observable } from 'rxjs';
import { DataService } from './data.service';

@Injectable({
  providedIn: 'root'
})
export class DataResolver implements Resolve<any> {

  constructor(private dataService: DataService) {}

  resolve(
    route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot
  ): Observable<any> | Promise<any> | any {
    return this.dataService.fetchData();
  }
}
```

在路由配置中使用 `resolve` 属性：

```typescript
const routes: Routes = [
  { path: 'data', component: DataComponent, resolve: { data: DataResolver } }
];
```

### CanLoad

`CanLoad` 守卫用于控制惰性加载模块的加载。例如，我们可以防止未登录用户加载某些模块：

```typescript
import { Injectable } from '@angular/core';
import { CanLoad, Route, UrlSegment, UrlTree, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { AuthService } from './auth.service';

@Injectable({
  providedIn: 'root'
})
export class LoadGuard implements CanLoad {

  constructor(private authService: AuthService, private router: Router) {}

  canLoad(
    route: Route,
    segments: UrlSegment[]
  ): Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {
    if (this.authService.isLoggedIn()) {
      return true;
    } else {
      this.router.navigate(['/login']);
      return false;
    }
  }
}
```

在路由配置中使用 `canLoad` 属性：

```typescript
const routes: Routes = [
  { path: 'lazy', loadChildren: () => import('./lazy/lazy.module').then(m => m.LazyModule), canLoad: [LoadGuard] }
];
```

## 总结

路由守卫是 Angular 应用中非常重要的功能，它们可以帮助我们控制路由的访问权限、数据预加载、用户确认等。通过本教程，你应该已经掌握了如何创建和使用不同类型的路由守卫。在实际开发中，合理使用路由守卫可以大大提升应用的安全性和用户体验。