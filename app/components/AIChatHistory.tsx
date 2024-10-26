import React from 'react';

interface AIChatHistoryProps {
  message: string;
}

function AIChatHistory({ message }: AIChatHistoryProps) {
  return (
    <div className="ai-message">
      <p>{message}</p>
    </div>
  );
}

export default AIChatHistory;
