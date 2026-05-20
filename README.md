# UF IFAS Statistical and Data Analytics Consulting Unit
### Intern Collaboration Repository

This repository is the shared workspace for the UF IFAS SDACU intern team. 
It is used for collaborating on R/Shiny projects, sharing data visualizations, 
and storing both practice and finalized work.

---

## What We Build
- R Shiny web applications
- Data visualizations and dashboards
- Statistical reports and analyses
- Other tools and methods as needed

---

## Folder Structure

```
UF-IFAS/
├── projects/     # Polished, finalized work ready to share
├── practice/     # Personal folders for practice and rough work
│   ├── MichaelFox/
│   └── YourName/
└── data/         # Shared datasets
```

**projects/** — Put work here when it's clean and presentable. 
This is the showcase folder.

**practice/YourName/** — Your personal sandbox. Rough code, 
experiments, exercises. No judgment here.

**data/** — Shared datasets the team can pull from.

---

## Getting Started

### 1. Install R
Download and install R from https://cran.r-project.org/

### 2. Install Git
Download and install Git from https://git-scm.com/download/win  
Use all default options during installation.

### 3. Clone this repo
Open a terminal and run:
```bash
git clone https://github.com/MichaelFox18/UF-IFAS.git
cd UF-IFAS
```

### 4. Create your personal folder
```bash
mkdir practice\YourName
echo "# YourName practice work" > practice\YourName\README.md
```

### 5. Configure Git with your name
```bash
git config --global user.name "Your Name"
git config --global user.email "youremail@gmail.com"
```

---

## Guidelines

- Anyone can push directly to main — just make sure your code runs before pushing
- Keep personal experiments in `practice/YourName/`
- Move polished work to `projects/` when it's ready
- Never commit sensitive data, passwords, or API keys
- If you break something, don't panic — Git keeps the full history and we can always roll back
