module.exports = {
  build: {
    command: 'npm run build',
    outputDirectory: '.next',
  },
  runtime: 'nodejs18.x',
  headers: [
    {
      source: '/_next/static/(.*)',
      headers: [
        {
          key: 'Cache-Control',
          value: 'public, max-age=31536000, immutable'
        },
        {
          key: 'Content-Type',
          value: 'application/javascript; charset=utf-8'
        }
      ]
    },
    {
      source: '/(.*).css',
      headers: [
        {
          key: 'Content-Type',
          value: 'text/css; charset=utf-8'
        }
      ]
    },
    {
      source: '/(.*).woff',
      headers: [
        {
          key: 'Content-Type',
          value: 'font/woff'
        }
      ]
    },
    {
      source: '/(.*).woff2',
      headers: [
        {
          key: 'Content-Type',
          value: 'font/woff2'
        }
      ]
    }
  ],
  env: {
    DEEPSEEK_API_KEY: process.env.DEEPSEEK_API_KEY,
    NEXT_PUBLIC_GA_MEASUREMENT_ID: process.env.NEXT_PUBLIC_GA_MEASUREMENT_ID,
  }
};