#!/bin/bash
# PreToolUse replacement: reads first 30 lines of task_plan.md
# Triggers before agent submits prompt

if [ -f "task_plan.md" ]; then
    echo "" >&2
    echo "=== CURRENT TASK PLAN (first 30 lines) ===" >&2
    head -n 30 "task_plan.md" >&2
    echo "==========================================" >&2
    echo "" >&2
fi

echo "{\"continue\": true}"
