import { preToolRejectHook, runHook } from '@mizunashi_mana/claude-code-hook-sdk';

void runHook({
    preToolUseHandler: preToolRejectHook({
        bash: {
            preferAnotherTools: [
                {
                    type: 'regex',
                    match: /(^| )playwright( |$)/,
                    preferTool: 'Use ./scripts/playwright.sh instead',
                },
                {
                    type: 'regex',
                    match: /(^| )rm(|dir)( |$)/,
                    preferTool: 'Use safe-file-deletion MCP instead',
                },
                {
                    type: 'regex',
                    match: /&\s*$/,
                    preferTool: 'Use manage-bg MCP instead',
                },
                {
                    type: 'regex',
                    match: /(^| )pkill( |$)/,
                    preferTool: 'Use manage-bg MCP instead',
                },
                {
                    type: 'regex',
                    match: /^devenv shell/,
                    preferTool: '`devenv shell` is not needed. Run command directly.',
                },
            ],
        },
    }),
});
