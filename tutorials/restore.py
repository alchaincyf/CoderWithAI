import os
import shutil
import tkinter as tk
from tkinter import filedialog, messagebox
import re

def select_folders():
    root = tk.Tk()
    root.withdraw()
    folders = []
    while True:
        folder = filedialog.askdirectory(title="选择要整理的文件夹", initialdir=os.getcwd())
        if folder:
            folders.append(folder)
            if not messagebox.askyesno("继续选择", "是否继续选择其他文件夹？"):
                break
        else:
            break
    return folders

def find_course_md(folder):
    for file in os.listdir(folder):
        if file.endswith('.md') and '课程' in file:
            return os.path.join(folder, file)
    return None

def parse_md_file(md_file):
    with open(md_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    sections = re.findall(r'## (.*?)\n((?:- .*?\n)*)', content, re.DOTALL)
    return [(title.strip(), len(items.strip().split('\n'))) for title, items in sections]

def organize_files(folder, sections):
    md_files = [f for f in os.listdir(folder) if f.endswith('.md') and f != os.path.basename(find_course_md(folder))]
    md_files.sort()
    
    file_index = 0
    for index, (title, item_count) in enumerate(sections, start=1):
        new_folder = os.path.join(folder, f"{index}. {title}")
        os.makedirs(new_folder, exist_ok=True)
        
        # 如果是最后一个文件夹，多移动一个文件
        if index == len(sections):
            item_count += 1
        
        for i in range(item_count):
            if file_index < len(md_files):
                shutil.move(os.path.join(folder, md_files[file_index]), os.path.join(new_folder, md_files[file_index]))
                file_index += 1

def main():
    folders = select_folders()
    
    if not folders:
        print("未选择任何文件夹，程序退出。")
        return
    
    for folder in folders:
        course_md = find_course_md(folder)
        if course_md:
            sections = parse_md_file(course_md)
            organize_files(folder, sections)
            print(f"文件夹 {folder} 整理完成")
        else:
            print(f"在文件夹 {folder} 中未找到包含'课程'的md文件")

if __name__ == "__main__":
    main()
