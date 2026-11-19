import type { Metadata, Viewport } from "next";
import Script from "next/script";
import "./globals.css";
import SiteHeader from "@/components/SiteHeader";
import SiteFooter from "@/components/SiteFooter";
import CopyEnhancer from "@/components/CopyEnhancer";
import { SITE } from "@/lib/site";

const GA_ID = process.env.NEXT_PUBLIC_GA_MEASUREMENT_ID || "GT-KDD8ZDP3";

export const metadata: Metadata = {
  metadataBase: new URL(SITE.url),
  title: { default: SITE.title, template: "%s · CoderWithAI" },
  description: SITE.description,
  alternates: { canonical: "/" },
  openGraph: { siteName: SITE.name, locale: "zh_CN", type: "website", title: SITE.title, description: SITE.description, url: SITE.url, images: [{ url: "/og.png", width: 1200, height: 630 }] },
  twitter: { card: "summary_large_image", title: SITE.title, description: SITE.description, images: ["/og.png"] },
  icons: { icon: "/favicon.ico" },
};

export const viewport: Viewport = { themeColor: "#1d36c8", width: "device-width", initialScale: 1 };

export default function RootLayout({ children }: Readonly<{ children: React.ReactNode }>) {
  return (
    <html lang="zh-CN">
      <head><link rel="preload" href="/fonts/cwa-title.woff2" as="font" type="font/woff2" crossOrigin="anonymous" /></head>
      <body>
        <a className="sr" href="#main">跳到正文</a>
        <SiteHeader />
        <main id="main">{children}</main>
        <SiteFooter />
        <CopyEnhancer />
        <Script src={`https://www.googletagmanager.com/gtag/js?id=${GA_ID}`} strategy="afterInteractive" />
        <Script id="ga" strategy="afterInteractive">
          {`window.dataLayer=window.dataLayer||[];function gtag(){dataLayer.push(arguments);}gtag('js',new Date());gtag('config','${GA_ID}');`}
        </Script>
      </body>
    </html>
  );
}
