import React, { useState } from 'react';
import ChatWindow from './ChatWindow';
import { FaExpand, FaCompress } from 'react-icons/fa';

// AIChatWidget 组件：用于渲染 AI 聊天小部件
function AIChatWidget() {
  // 状态管理
  const [isOpen, setIsOpen] = useState(false);  // 控制聊天窗口是否打开
  const [isFullScreen, setIsFullScreen] = useState(false);  // 控制聊天窗口是否全屏
  const [messages, setMessages] = useState([]);  // 存储聊天消息，初始为空数组

  // 切换聊天窗口的开关状态
  const toggleChat = () => {
    console.log('Toggling chat, current state:', isOpen);  // 记录当前状态
    setIsOpen(!isOpen);  // 切换开关状态
  };

  // 切换全屏模式
  const toggleFullScreen = () => setIsFullScreen(!isFullScreen);

  return (
    // 主容器，根据状态添加相应的 CSS 类
    <div className={`ai-chat-widget ${isOpen ? 'open' : ''} ${isFullScreen ? 'fullscreen' : ''}`}>
      {isOpen && (
        // 如果聊天窗口打开，渲染 ChatWindow 组件
        <ChatWindow 
          messages={messages}  // 传递聊天消息
          isFullScreen={isFullScreen}  // 传递全屏状态
          onToggleFullScreen={toggleFullScreen}  // 传递切换全屏的方法
          onClose={toggleChat}  // 传递关闭聊天窗口的方法
        />
      )}
      {!isOpen && (
        // 如果聊天窗口关闭，显示打开聊天的按钮
        <div className="chat-controls">
          <button className="chat-toggle" onClick={toggleChat}>
            打开 AI 助手
          </button>
          <button onClick={toggleFullScreen} className="fullscreen-toggle" title={isFullScreen ? "退出全屏" : "全屏"}>
            {isFullScreen ? <FaCompress /> : <FaExpand />}
          </button>
        </div>
      )}
    </div>
  );
}

export default AIChatWidget;