#!/bin/bash
git config --global user.email "${GIT_EMAIL}"
git config --global user.name "${GIT_NAME}"
cp .stack-work/install/x86_64-linux/lts-12.0/8.4.3/bin/MyFunActivity MyFunActivity
git add MyFunActivity
git commit -m "add release"
