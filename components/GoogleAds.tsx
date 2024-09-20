'use client';

import { useEffect } from 'react';

// 添加这个类型声明
declare global {
  interface Window {
    adsbygoogle: any[];
  }
}

export default function GoogleAds() {
  useEffect(() => {
    try {
      (window.adsbygoogle = window.adsbygoogle || []).push({});
    } catch (err) {
      console.error(err);
    }
  }, []);

  return (
    <ins
      className="adsbygoogle"
      style={{ display: 'block' }}
      data-ad-client="ca-pub-7522094408813551"
      data-ad-slot="YOUR_AD_SLOT"
      data-ad-format="auto"
      data-full-width-responsive="true"
    />
  );
}