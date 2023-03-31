#!/usr/bin/env python3
import csv
import os
import subprocess
import sys
import argparse

def check_install_package(package, manager, install=False):
  """Installs the specified package using the specified package manager."""
  global username
  # Check if the package is already installed
  try:
    if manager == 'pacman':
        subprocess.run([manager, "-Q", package], check=True)
    elif manager == 'yay':
        subprocess.run(["sudo", "-u", username, manager, "-Q", package], check=True)
    elif manager == 'brew':
        subprocess.run([manager, "list", package], check=True)
    elif manager == 'pip':
        subprocess.run([manager, "show", package], check=True)
    elif manager == 'git':
        package_name = package.split("/")[-1]
        repo_path = os.path.join("/", "home", username, ".siarch", "repos", package_name)
        if not os.path.isdir(repo_path):
            raise subprocess.CalledProcessError(-1, "git")
    print(f"{package} is already installed")
  except subprocess.CalledProcessError:
    # Package is not installed, so install it
    if not install:
        print(f"\033[0;31m Package {package} is not installed.\033[0m")
        return 
    try:
        if manager == 'pacman':
            subprocess.run(["sudo", manager, "-S", "--noconfirm", package], check=True)
        elif manager == 'yay':
            subprocess.run(["sudo", "-u", username, manager, "-S", "--noconfirm", package], check=True)
        elif manager == 'brew':
            subprocess.run([manager, "install", package], check=True)
        elif manager == 'pip':
            subprocess.run([manager, "install", package], check=True)
        elif manager == 'git':
            package_name = package.split("/")[-1]
            repo_path = os.path.join("/", "home", username, ".siarch", "repos", package_name)
            subprocess.run(["git", "clone", package, repo_path])
            print(f"Cloned {package}")
            os.chdir(repo_path)
            subprocess.run(["make"])
            subprocess.run(["sudo","make", "install"])
            print(f"Installed {package}")

        print(f"Installed {package} using {manager}")
    except:
        print(f"Failed in install {package}.")

def check_yay():
    global username

    # Check if yay is installed
    try:
      subprocess.run(["yay", "--version"], check=True)
      print("yay is already installed")
    except:
      # yay is not installed, so install it
      subprocess.run(["sudo","-u", username ,"git", "clone", "https://aur.archlinux.org/yay.git", "/tmp/yay"])
      cwd = os.getcwd()
      os.chdir("/tmp/yay")
      subprocess.run(["sudo","-u", username ,"makepkg", "-si"])
      print("Installed yay")
      os.chdir(cwd)


parser = argparse.ArgumentParser()

# Add the -c and -u arguments
parser.add_argument("-f", "--file", required=True, help="path to the CSV file")
parser.add_argument("-u", "--username", required=True, help="username")
parser.add_argument("-i", "--install", action="store_true", help="install")

# Parse the command-line arguments
args = parser.parse_args()

# Access the values of the arguments
csv_file = args.file
username = args.username
install = args.install

if sys.platform == "linux" or sys.platform == "linux2":
    check_yay()
    subprocess.run(["yay", "-Sy"], check=True)

# Open the CSV file
with open(csv_file, 'r') as csvfile:
  reader = csv.DictReader(csvfile)
  # Read each line of the CSV file
  for row in reader:
    tag = row["tag"]
    package = row["package"]
    description = row["description"]
    print("Checking {}: {}".format(package, description))
    if tag == "":
      # Install using pacman
      check_install_package(package, "pacman", install)
    elif tag == "p" or tag == "P":
      # Install using pip
      check_install_package(package, "pip", install)
    elif tag == "a" or tag == "A": 
      # Install using yay
      check_install_package(package, "yay", install)
    elif tag == "g" or tag == "G":
      check_install_package(package, "git", install)
    elif tag == "b" or tag == "B":
      check_install_package(package, "brew", install)
