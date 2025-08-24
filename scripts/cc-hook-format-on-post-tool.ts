import * as fs from 'node:fs/promises';
import { runHook, execFileAsync, postToolUpdateFileHook } from '@mizunashi_mana/claude-code-hook-sdk';

void runHook({
    postToolUseHandler: postToolUpdateFileHook(async (input) => {
        await fs.access(input.filePath);

        let backFile: string | undefined = undefined;
        if (input.type === 'update') {
            backFile = `${input.filePath}.bak`;
            await fs.copyFile(input.filePath, backFile);
        }

        let errorMessage: string | undefined = undefined;

        try {
            await lint(input.filePath);
        }
        catch (error) {
            if (error instanceof Error) {
                errorMessage = error.message;
            }
            else {
                throw error;
            }
        }

        if (backFile !== undefined) {
            let diffLines: string[];
            try {
                const { stdout } = await execFileAsync('diff', [backFile, input.filePath]);
                diffLines = stdout.split('\n');
            }
            catch {
                diffLines = [];
            }

            if (
                diffLines.length > 0
                && !diffLines.some(line => line.startsWith('>'))
                && diffLines.filter(line => line.startsWith('<')).length === input.linesAddition.length
            ) {
                await fs.copyFile(backFile, input.filePath);
            }

            await fs.unlink(backFile);
        }

        if (errorMessage === undefined) {
            console.error(errorMessage);
        }

        return {};
    }),
});

async function lint(filePath: string): Promise<void> {
    await execFileAsync('pre-commit', ['run', '--files', filePath]);
}
