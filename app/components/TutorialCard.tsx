import Image from 'next/image';

export default function TutorialCard({ tutorial }) {
  return (
    <div>
      <Image 
        src={tutorial.coverImage}
        alt={tutorial.title}
        width={300}
        height={200}
        layout="responsive"
      />
      <h2>{tutorial.title}</h2>
      {/* 其他内容 */}
    </div>
  );
}