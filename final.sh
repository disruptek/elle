#!/bin/bash
rm cleanup.sh
git add -A
git commit -m "Remove temporary cleanup script"
git push origin refactor-work
