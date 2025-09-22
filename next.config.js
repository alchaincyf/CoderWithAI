/** @type {import('next').NextConfig} */
const nextConfig = {
  env: {
    DEEPSEEK_API_KEY: process.env.DEEPSEEK_API_KEY,
    NEXT_PUBLIC_GA_MEASUREMENT_ID: process.env.NEXT_PUBLIC_GA_MEASUREMENT_ID,
  },
  compress: false,
  generateEtags: false,
  poweredByHeader: false,
  assetPrefix: '',
  async headers() {
    return [
      {
        source: '/api/:path*',
        headers: [
          { key: 'Access-Control-Allow-Origin', value: '*' },  // 允许所有域名
          { key: 'Access-Control-Allow-Methods', value: 'GET,OPTIONS,PATCH,DELETE,POST,PUT' },
          { key: 'Access-Control-Allow-Headers', value: 'X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version' },
        ],
      },
      {
        source: '/_next/static/:path*',
        headers: [
          { key: 'Cache-Control', value: 'public, max-age=31536000, immutable' },
          { key: 'Content-Type', value: 'application/javascript; charset=utf-8' },
        ],
      },
      {
        source: '/:path*.css',
        headers: [
          { key: 'Content-Type', value: 'text/css; charset=utf-8' },
        ],
      },
      {
        source: '/:path*.woff',
        headers: [
          { key: 'Content-Type', value: 'font/woff' },
        ],
      },
      {
        source: '/:path*.woff2',
        headers: [
          { key: 'Content-Type', value: 'font/woff2' },
        ],
      },
    ];
  },
  images: {
    domains: ['www.codewithai.com'], // 添加你的图片域名
    unoptimized: true,
  },
};

module.exports = nextConfig;