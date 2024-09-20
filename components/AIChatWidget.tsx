'use client'; // 添加这行来标记为客户端组件

import React, { useState, useRef, useEffect } from 'react';
import styles from './AIChatWidget.module.css';
import { usePathname } from 'next/navigation';
import ReactMarkdown from 'react-markdown';
import Image from 'next/image';

const AIChatWidget: React.FC = () => {
  const [isOpen, setIsOpen] = useState(false);
  const [input, setInput] = useState('');
  const [messages, setMessages] = useState<Array<{ role: string; content: string }>>([]);
  const [isTyping, setIsTyping] = useState(false);
  const pathname = usePathname();
  const messagesEndRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (input.trim()) {
      const userMessage = { role: "user", content: input };
      setMessages(prevMessages => [...prevMessages, userMessage]);
      setInput('');
      setIsTyping(true);

      try {
        console.log('Sending request to API...');
        const response = await fetch('https://www.coderwithai.top/api/chat', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({ 
            messages: [...messages, userMessage],
            currentPath: pathname
          }),
        });

        console.log('Response status:', response.status);

        if (!response.ok) {
          const errorText = await response.text();
          console.error('Error response:', errorText);
          throw new Error(`HTTP error! status: ${response.status}`);
        }

        const reader = response.body?.getReader();
        const decoder = new TextDecoder();

        setMessages(prevMessages => [...prevMessages, { role: "assistant", content: "" }]);

        while (true) {
          const { done, value } = await reader!.read();
          if (done) break;
          const chunk = decoder.decode(value);
          console.log('Received chunk:', chunk);
          const lines = chunk.split('\n');
          for (const line of lines) {
            if (line.startsWith('data: ')) {
              const data = line.slice(6);
              if (data === '[DONE]') {
                break;
              }
              try {
                const parsed = JSON.parse(data);
                if (parsed.choices[0].delta.content) {
                  setMessages(prevMessages => {
                    const newMessages = [...prevMessages];
                    const lastMessage = newMessages[newMessages.length - 1];
                    if (lastMessage.role === "assistant") {
                      lastMessage.content += parsed.choices[0].delta.content;
                    }
                    return newMessages;
                  });
                }
              } catch (error) {
                console.error('Error parsing JSON:', error);
              }
            }
          }
        }
      } catch (error) {
        console.error("Detailed error:", error);
        const errorMessage = { role: "assistant", content: `Error: ${error instanceof Error ? error.message : 'An unexpected error occurred'}` };
        setMessages(prevMessages => [...prevMessages, errorMessage]);
      } finally {
        setIsTyping(false);
      }
    }
  };

  const toggleChat = () => {
    setIsOpen(!isOpen);
  };

  return (
    <div className={styles.widgetContainer}>
      {isOpen ? (
        <div className={styles.chatWindow}>
          <div className={styles.chatHeader}>
            <h3>AI Assistant</h3>
            <button onClick={toggleChat} className={styles.closeButton}>×</button>
          </div>
          <div className={styles.chatMessages}>
            {messages.map((msg, index) => (
              <div key={index} className={`${styles.message} ${msg.role === 'user' ? styles.userMessage : styles.aiMessage}`}>
                {msg.role === 'user' ? (
                  msg.content
                ) : (
                  <ReactMarkdown>{msg.content}</ReactMarkdown>
                )}
              </div>
            ))}
            {isTyping && <div className={styles.typingIndicator}>AI is thinking...</div>}
            <div ref={messagesEndRef} />
          </div>
          <form onSubmit={handleSubmit} className={styles.inputForm}>
            <input
              type="text"
              value={input}
              onChange={(e) => setInput(e.target.value)}
              placeholder="Ask me anything..."
              className={styles.inputField}
            />
            <button type="submit" className={styles.sendButton}>Send</button>
          </form>
        </div>
      ) : (
        <button onClick={toggleChat} className={styles.chatButton}>
          <Image src="/chat.png" alt="AI Chat" width={64} height={64} />
        </button>
      )}
    </div>
  );
};

export default AIChatWidget;