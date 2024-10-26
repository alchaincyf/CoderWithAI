import Image from 'next/image';

interface TutorialCardProps {
  tutorial: {
    title: string;
    description?: string;
    image?: string;
    // 添加其他必要的属性
  };
}

export default function TutorialCard({ tutorial }: TutorialCardProps) {
  return (
    <div>
      {tutorial.image && (
        <Image 
          src={tutorial.image}
          alt={tutorial.title}
          width={300}
          height={200}
        />
      )}
      <h3>{tutorial.title}</h3>
      {tutorial.description && <p>{tutorial.description}</p>}
    </div>
  );
}
