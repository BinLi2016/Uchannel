# Planning with Files for Cursor

Implementing Manus-style file-based planning in Cursor using hooks and rules.

## Installation Overview

This skill has been adapted for Cursor on Windows using Git Bash.

- **Rules**: Located in `.cursor/rules/planning-with-files.mdc`
- **Hooks**: Configured in `.cursor/hooks.json`
- **Templates**: Stored in `.cursor/skills/planning-with-files/templates/`
- **Scripts**: Located in `.cursor/scripts/` (using Git Bash)

## Requirements

1. **Git for Windows**: Required to run the bash scripts via hooks.
2. **Nightly Build** (Optional but recommended): Required for full "Agent Skill" functionality.
3. **Stable Build**: Works via `.cursor/rules` and `.cursor/hooks.json`.

## How it Works in Cursor

### Hooks

Cursor hooks allow the agent to automatically perform actions during its lifecycle:

1. **beforeSubmitPrompt**: Triggers `read-plan.sh`. This reads the first 30 lines of `task_plan.md` into the agent's attention window before every prompt submission.
2. **afterFileEdit**: Triggers `remind-update-plan.sh`. Reminds the agent to update the task plan if a phase was completed.
3. **stop**: Triggers `check-complete.sh`. Verifies if all phases in `task_plan.md` are marked as complete before finishing.

### Rules

The rule in `.cursor/rules/planning-with-files.mdc` provides the core guidelines that Cursor's AI follows, ensuring it uses the 3-file pattern (`task_plan.md`, `findings.md`, `progress.md`).

## Windows-Specific Configuration

The `hooks.json` is configured to use Git Bash with absolute paths for maximum reliability:

```json
"command": "\"C:\\Program Files\\Git\\bin\\bash.exe\" \"D:\\Goft\\CsharpCode\\RailwayBridgeSource\\.cursor\\scripts\\read-plan.sh\""
```

### Path Handling
- In `hooks.json`, use double backslashes `\\` for Windows paths.
- In bash scripts, use forward slashes `/`.

## JSON Hook Format

All hook scripts are designed to handle Cursor's JSON input/output format:
- **Input**: Received via `stdin` (JSON).
- **Output**: Must be valid JSON (e.g., `{"continue": true}` or `{"followup_message": "..."}`).

## Manual Workflow

If hooks are not firing (beta limitations), use the manual workflow:

1. **Initialize**: Run `.cursor/scripts/init-session.sh` in Git Bash.
2. **Read Plan**: Manually read `task_plan.md` before major decisions.
3. **Update**: Update `task_plan.md` and `progress.md` after every phase.
4. **Check**: Run `.cursor/scripts/check-complete.sh` to verify readiness.

## Troubleshooting

- **Check Hooks Output**: Open the "Hooks" output panel in Cursor to see execution logs and errors.
- **Git Bash Path**: Ensure Git Bash is installed at `C:\Program Files\Git\bin\bash.exe`.
- **Permissions**: If scripts fail to execute, check file permissions or run Cursor as Administrator.
