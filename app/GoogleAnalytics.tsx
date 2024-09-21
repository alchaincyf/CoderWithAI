'use client';

<<<<<<< HEAD
import Script from 'next/script';

const GoogleAnalytics = () => {
  return (
    <>
      <Script
        src={`https://www.googletagmanager.com/gtag/js?id=${process.env.NEXT_PUBLIC_GA_MEASUREMENT_ID}`}
=======
import Script from 'next/script'

export default function GoogleAnalytics() {
  return (
    <>
      <Script
        src="https://www.googletagmanager.com/gtag/js?id=G-62BNSQ9DLM"
>>>>>>> 810985e83d2338326054cbab57f2ff1ee0a62c06
        strategy="afterInteractive"
      />
      <Script id="google-analytics" strategy="afterInteractive">
        {`
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());

<<<<<<< HEAD
          gtag('config', '${process.env.NEXT_PUBLIC_GA_MEASUREMENT_ID}');
        `}
      </Script>
    </>
  );
};

export default GoogleAnalytics;
=======
          gtag('config', 'G-62BNSQ9DLM');
        `}
      </Script>
    </>
  )
}
>>>>>>> 810985e83d2338326054cbab57f2ff1ee0a62c06
