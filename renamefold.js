const fs = require('fs');
const path = require('path');

const tutorialsDir = path.join(process.cwd(), 'tutorials');

function padFolderNames(dir) {
  const items = fs.readdirSync(dir, { withFileTypes: true });
  const renamedItems = [];

  items.forEach(item => {
    if (item.isDirectory()) {
      const match = item.name.match(/^(\d+)\./);
      if (match && parseInt(match[1]) < 10) {
        const newName = `0${item.name}`;
        const oldPath = path.join(dir, item.name);
        const newPath = path.join(dir, newName);
        fs.renameSync(oldPath, newPath);
        console.log(`Renamed: ${item.name} -> ${newName}`);
        renamedItems.push({ oldName: item.name, newName: newName });
      }
    }
  });

  // 处理重命名后的文件夹
  renamedItems.forEach(({ newName }) => {
    const newPath = path.join(dir, newName);
    padFolderNames(newPath);
  });

  // 处理未重命名的文件夹
  items.forEach(item => {
    if (item.isDirectory() && !renamedItems.some(ri => ri.oldName === item.name)) {
      padFolderNames(path.join(dir, item.name));
    }
  });
}

// 处理所有语言目录
fs.readdirSync(tutorialsDir, { withFileTypes: true })
  .filter(dirent => dirent.isDirectory())
  .forEach(dirent => {
    console.log(`Processing ${dirent.name} tutorials...`);
    padFolderNames(path.join(tutorialsDir, dirent.name));
  });

console.log('Folder renaming completed for all languages.');