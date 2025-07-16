import express from 'express';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const projectRoot = path.resolve(__dirname, '..');
const listenPort = process.env.PORT ? parseInt(process.env.PORT, 10) : 8000;

const app = express();

app.use('/test-assets', express.static(path.join(projectRoot, 'tests/features/assets')));
app.use('/', express.static(path.join(projectRoot, 'output')));

app.listen(listenPort, () => {
    console.log(`Static server running on http://localhost:${listenPort}`);
});
