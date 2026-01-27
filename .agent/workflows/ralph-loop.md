---
description: How to run the Antigravity Ralph Loop
---

// turbo-all
1. Ensure `ralph_instructions.md` is in the workspace root and `ralph_loop.py` is in the `Python/` directory.
2. Initialize or update `task.md` with the work you want to perform.
3. Switch to "Ralph Mode" by reading `ralph_instructions.md`.
4. The loop proceeds as follows:
   - Agent performs one or more steps in `task.md`.
   - Agent updates `task.md` status.
   - // turbo
     Agent runs `python Python/ralph_loop.py --iterations 20` to check for overall completion (replace 20 with your desired count).
5. If completion is signaled via `<ralph>COMPLETE</ralph>`, the task is finished.
6. If the agent gets stuck, it should summarize state and the loop restarts.
