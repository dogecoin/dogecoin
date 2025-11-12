#!/bin/bash

echo "🔄 Updating backup branch..."
echo ""

# First, let's check if there are uncommitted changes
if ! git diff-index --quiet HEAD --; then
    echo "⚠️  You have uncommitted changes. Please commit them in GitHub Desktop first!"
    echo "Then run this script again."
    exit 1
fi

echo "📋 Current branch: $(git branch --show-current)"
echo "📊 Total commits to backup: $(git rev-list --count master..pat-aggregation-prototype)"
echo ""

# Delete old backup and create fresh one
echo "🗑️  Removing old backup branch..."
git branch -D backup-pat-aggregation-prototype 2>/dev/null || echo "No old backup found"

echo "✨ Creating fresh backup from pat-aggregation-prototype..."
git branch backup-pat-aggregation-prototype

echo ""
echo "✅ Backup complete! Your work is safe in 'backup-pat-aggregation-prototype'"
echo ""
echo "Next steps:"
echo "1. Switch to master branch in GitHub Desktop"
echo "2. Pull latest changes from origin/master" 
echo "3. Create a pull request or merge pat-aggregation-prototype"
