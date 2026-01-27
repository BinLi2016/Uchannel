---
name: planning-with-files
description: Manus-style file-based planning for complex tasks. Creates task_plan.md, findings.md, and progress.md. Use when starting complex multi-step tasks, research projects, or any task requiring >5 tool calls.
---

# Planning with Files (Agent Edition)

Work like a top-tier agent: Use persistent markdown files as your "working memory on disk."

## Quick Start

Before ANY complex task:

1. **Create `task_plan.md`** — Phase tracking and progress
2. **Create `findings.md`** — Research and discoveries
3. **Create `progress.md`** — Session log and test results
4. **Re-read plan before decisions** — Refreshes goals in attention window
5. **Update after each phase** — Mark complete, log errors

## The Core Pattern

```
Context Window = RAM (volatile, limited)
Filesystem = Disk (persistent, unlimited)

→ Anything important gets written to disk.
```

## File Purposes

| File | Purpose | When to Update |
|------|---------|----------------|
| `task_plan.md` | Phases, progress, decisions | After each phase |
| `findings.md` | Research, discoveries | After ANY discovery |
| `progress.md` | Session log, test results | Throughout session |

## Agent Guidelines

### 1. Create Plan First
Never start a complex task without `task_plan.md`. Use the `task_boundary` tool alongside these files for maximum visibility.

### 2. The 2-Action Rule
After every 2 search, view, or browser operations, IMMEDIATELY sync key findings to `findings.md`.

### 3. Read Before Decide
Before major decisions, read `task_plan.md` to ensure alignment with the objective.

### 4. Self-Correction Protocol
Log ALL errors in `progress.md`. If a task fails 3 times, reassess the approach in `task_plan.md` before proceeding.

## When to Use
Use for multi-step tasks, research, or building new components. Skip for simple Q&A.
