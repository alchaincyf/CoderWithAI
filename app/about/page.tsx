import React from 'react'
import Link from 'next/link'

export default function About() {
  return (
    <div className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-4">About CoderWithAI</h1>
      <p className="mb-4">
        CoderWithAI is a comprehensive online learning platform designed to help individuals master various programming languages. Our goal is to provide high-quality, accessible tutorials for learners of all levels, from beginners to advanced programmers.
      </p>
      <p className="mb-4">
        Our platform features:
      </p>
      <ul className="list-disc list-inside mb-4">
        <li>Tutorials for multiple programming languages</li>
        <li>A user-friendly interface with easy navigation</li>
        <li>Regularly updated content to keep up with the latest programming trends</li>
        <li>Interactive examples and exercises to reinforce learning</li>
      </ul>
      <p className="mb-4">
        We believe in the power of education and the importance of making programming knowledge accessible to everyone. Our team of experienced developers and educators work tirelessly to create content that is both informative and engaging.
      </p>
      <p className="mb-4">
        Whether you&apos;re looking to start a career in software development, enhance your current skills, or simply explore the world of programming, we&apos;re here to support your learning journey.
      </p>
      <Link href="/" className="text-blue-500 hover:underline">
        Return to Home
      </Link>
    </div>
  )
}