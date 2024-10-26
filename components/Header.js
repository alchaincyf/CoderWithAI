import Link from 'next/link';
import { useRouter } from 'next/router';

const Header = () => {
  const router = useRouter();

  const languages = [
    { name: 'JavaScript', path: '/JavaScript' },
    { name: 'Python', path: '/Python' },
    { name: 'Java', path: '/Java' },
    { name: 'C++', path: '/Cpp' },
    { name: 'PHP', path: '/PHP' },
    { name: 'C#', path: '/CSharp' }
  ];

  return (
    <header>
      <nav>
        <Link href="/">
          CoderWithAI
        </Link>
        <ul>
          <li>
            <Link href="/" className={router.pathname === '/' ? 'active' : ''}>
              Home
            </Link>
          </li>
          {languages.map((lang) => (
            <li key={lang.path}>
              <Link href={lang.path} className={router.pathname.startsWith(lang.path) ? 'active' : ''}>
                {lang.name}
              </Link>
            </li>
          ))}
        </ul>
        <a href="https://github.com/alchaincyf/CoderWithAI" target="_blank" rel="noopener noreferrer">
          GitHub
        </a>
      </nav>
    </header>
  );
};

export default Header;
