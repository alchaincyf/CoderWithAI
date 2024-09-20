'use client';

import { useEffect } from 'react';
import Script from 'next/script';

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
    <>
      <Script
        async
        src="https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js?client=ca-pub-7522094408813551"
        crossOrigin="anonymous"
      />
      <ins
        className="adsbygoogle"
        style={{ display: 'block' }}
        data-ad-format="autorelaxed"
        data-ad-client="ca-pub-7522094408813551"
        data-ad-slot="4428404678"
      />
    </>
  );
}