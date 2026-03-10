#!/bin/bash
rm commit.sh
git add -A
git commit -m "Remove temporary commit script"
git push origin refactor-work
