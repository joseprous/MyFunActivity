#!/bin/bash
echo "begin deploy.sh"
cp .stack-work/install/x86_64-linux/lts-12.0/8.4.3/bin/MyFunActivity MyFunActivity
echo "after copy"
git add MyFunActivity
git commit -m "add release"
echo "end deploy.sh"
