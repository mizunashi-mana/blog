import path from 'node:path';
import * as express from 'express';

const projectRoot = path.resolve(import.meta.dirname, '..');
const listenPort = process.env.PORT !== undefined && process.env.PORT !== '' ? parseInt(process.env.PORT, 10) : 8000;

const app = express.default();

app.use('/test-assets', express.static(path.join(projectRoot, 'tests/features/assets')));
app.use('/', express.static(path.join(projectRoot, 'output')));

app.listen(listenPort, () => {
    console.log(`Static server running on http://localhost:${listenPort}`);
});
