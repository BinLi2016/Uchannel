#!/bin/bash
# Check if all phases in task_plan.md are complete
# Adapted for Cursor Stop hook

PLAN_FILE="task_plan.md"

if [ ! -f "$PLAN_FILE" ]; then
    # Return empty response if no plan file
    echo "{}"
    exit 0
fi

# Count phases by status
TOTAL=$(grep -c "### Phase" "$PLAN_FILE" || true)
COMPLETE=$(grep -cF "**Status:** complete" "$PLAN_FILE" || true)

# Ensure values are numeric
TOTAL=${TOTAL:-0}
COMPLETE=${COMPLETE:-0}

if [ "$TOTAL" -gt 0 ] && [ "$COMPLETE" -lt "$TOTAL" ]; then
    echo "{\"followup_message\": \"[planning-with-files] Task not complete. Check task_plan.md for pending phases ($COMPLETE/$TOTAL complete).\"}"
else
    echo "{}"
fi
