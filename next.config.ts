import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  poweredByHeader: false,
  images: { unoptimized: true },
  async redirects() {
    // 旧站（2024 版）的 31 门语言教程已下线：/Python/... 这类老链接统一 301 到新的入口，避免死链
    const oldLangs = [
      "Angular","Bootstrap","Cpp","CSharp","CSS","Django","Express.js","Go","Haskell","HTML","Java",
      "JavaScript","Kotlin","MATLAB","MongoDB","Next.js","Node.js","Perl","PHP","PostgreSQL","Python",
      "R","React","Ruby","Rust","Scala","SQL","Swift","TypeScript","Vue.js","C\\+\\+","C%23",
    ];
    return [
      ...oldLangs.flatMap((l) => [
        { source: `/${l}`, destination: "/legacy", permanent: true },
        { source: `/${l}/:path*`, destination: "/legacy", permanent: true },
      ]),
      { source: "/tutorials/:path*", destination: "/legacy", permanent: true },
      { source: "/contact", destination: "/about", permanent: true },
    ];
  },
};

export default nextConfig;
