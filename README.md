# Introduction
This repository is for the demographics data project initially funded by CMA.

# Connecting R Studio to GitHub
## Setting up your Fine-Tuned PAT
To connect R Studio projects to repositories on GitHub, there are two simple steps.
First, create a **Fine-Tuned Personal Access Token**. Here is a [step-by-step](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).
Second, in R Studio select New Project > Version Control > Git. When it comes to your credentials, you'll begin with your GitHub user name. Instead of your password, you'll enter your PAT.

## Configuring Git
You’ll need to configure Git. You can do this through RStudio or in your command line interface of choice. In RStudio, open a new Terminal via Tools > Terminal > New Terminal and enter:

git config --global user.name 'Your Name'
git config --global user.email 'your@email.com'

# Data
As of December 2024, this project consists solely of data from Dallas ISD. Due to data protections, these data are located on OneDrive and will be shared with team members. This may affect some R Studio code, as it may require changing the name of the root directory.
