#!/bin/bash

echo "🚀 PAT Aggregation Merge Helper"
echo "==============================="
echo ""

# Check current branch
CURRENT_BRANCH=$(git branch --show-current)
echo "📍 Current branch: $CURRENT_BRANCH"

if [ "$CURRENT_BRANCH" != "master" ]; then
    echo ""
    echo "⚠️  You need to be on the master branch to merge!"
    echo ""
    echo "👉 Please use GitHub Desktop to:"
    echo "   1. Switch to 'master' branch"
    echo "   2. Pull latest changes (Fetch origin → Pull)"
    echo "   3. Run this script again"
    exit 1
fi

echo ""
echo "📊 Merge Statistics:"
echo "-------------------"
echo "Commits to merge: $(git rev-list --count master..pat-aggregation-prototype)"
echo "Files changed: $(git diff --stat master..pat-aggregation-prototype | tail -1)"
echo ""

echo "🔍 Checking for potential conflicts..."
git merge-tree $(git merge-base master pat-aggregation-prototype) master pat-aggregation-prototype > /tmp/conflicts.txt 2>&1

if [ -s /tmp/conflicts.txt ]; then
    echo "⚠️  Potential conflicts detected!"
    echo ""
    echo "The merge might have conflicts in these areas:"
    grep -E "<<<<<<< |>>>>>>> " /tmp/conflicts.txt | head -10
    echo ""
    echo "Don't worry! GitHub Desktop will help you resolve these."
else
    echo "✅ No obvious conflicts detected!"
fi

echo ""
echo "📝 Next Steps in GitHub Desktop:"
echo "--------------------------------"
echo "1. Go to Branch menu → 'Merge into Current Branch...'"
echo "2. Select 'pat-aggregation-prototype'"
echo "3. Click 'Merge' button"
echo "4. If conflicts appear:"
echo "   - GitHub Desktop will show conflicted files"
echo "   - Click each file to resolve conflicts"
echo "   - Use the visual conflict editor"
echo "5. After resolving, commit the merge"
echo "6. Push to origin/master"
echo ""
echo "💡 TIP: You can also create a Pull Request on GitHub.com for easier review!"
