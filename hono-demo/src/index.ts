import { Hono } from 'hono'

import names from './names.ts' // Importiere die names-Routen

const app = new Hono();

/* == Einfache Beispiel == */

// Stellt http://localhost:3000/ bereit
// und antwortet mit "Hello Hono!"
app.get('/', (c) => {
  return c.text('Hello Hono!');
});

// Test-Endpoint, der den Status der Anwendung zurückgibt
app.get('/status', (c) => {
  return c.json({ status: 'ok' });
});

// Beispiel-POST-Endpoint, der JSON-Daten entgegennimmt und zurückgibt
app.post('/echo', async c => {
  const body = await c.req.json();
  return c.json(body);
});

// Beispiel-GET-Endpoint, der einen Parameter entgegennimmt
app.get('/greet/:name', (c) => {
  const name = c.req.param('name'); // Parameter aus der URL
  return c.text(`Hello, ${name}!`);
});

app.get('/greetHtml/:name', (c) => {
  const name = c.req.param('name'); // Parameter aus der URL
  // HTML-Antwort zurückgeben
  return c.html(`<h1>Hello, ${name}!</h1>`);
});

/* == Beispiel für CRUD-artige-Operationen == */
// SIEHE names.ts

app.route('/v0/', names); // API-Versionierung

export default app;
