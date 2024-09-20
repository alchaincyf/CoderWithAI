import React from 'react';
import AIChatHistory from './AIChatHistory';
import { FaExpand, FaCompress } from 'react-icons/fa';

interface Message {
  content: string;
  // 添加其他必要的属性
}

interface ChatWindowProps {
  messages: Message[];
  isFullScreen: boolean;
  onToggleFullScreen: () => void;
  onClose: () => void;
}

function ChatWindow({ messages, isFullScreen, onToggleFullScreen, onClose }: ChatWindowProps) {
  return (
    <div className={`chat-window ${isFullScreen ? 'fullscreen' : ''}`}>
      <div className="chat-header">
        <h3>AI 助手</h3>
        <div className="chat-controls">
          <button onClick={onToggleFullScreen} className="fullscreen-toggle" title={isFullScreen ? "退出全屏" : "全屏"}>
            {isFullScreen ? <FaCompress /> : <FaExpand />}
          </button>
          <button onClick={onClose} className="close-chat" title="关闭">
            X
          </button>
        </div>
      </div>
      <div className="chat-messages">
        {messages.map((message, index) => (
          <AIChatHistory key={index} message={message.content} />
        ))}
      </div>
      <div className="chat-input">
        {/* 这里添加聊天输入框 */}
      </div>
    </div>
  );
}

export default ChatWindow;