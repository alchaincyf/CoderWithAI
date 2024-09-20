import React from 'react';

function AIChatHistory({ message }) {
  return (
    <div className="ai-message">
      <p>{message}</p>
    </div>
  );
}

export default AIChatHistory;