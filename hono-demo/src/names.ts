import { Hono } from 'hono';
const app = new Hono();

/* == Beispiel für CRUD-artige-Operationen == */

// Variablen um Namen zu speichern
let names = ['Alice', 'Bob', 'Charlie'];
// Alle Namen auslesen
app.get('/names', (c) => {
  return c.json(names);
});

// Neuen Namen hinzufügen
app.put('/names/:name', async (c) => {
  const name = c.req.param('name');
  names.push(name);
  console.log(`Name added: ${name}`);
  c.status(201); // Setzt den Status auf "201 Created"
  return c.body(null); // leere Antwort
});
// Namen auslesen
app.get('/names/:name', (c) => {
  const name = c.req.param('name');
  if (names.includes(name)) {
    return c.json({ name: name }); // Status "204 No Content"
  } else {
    return c.notFound(); // Status "404 Not Found"
  }
});
// Namen löschen
app.delete('/names/:name', (c) => {
  const name = c.req.param('name');
  names = names.filter(n => n !== name); // Filtert den Namen aus der Liste raus
  console.log(`Name deleted: ${name}`);
  return c.body(null); // leere Antwort mit Status 200 (OK)
});

// Verbesserte Version mit Query-Parametern
app.get('/names2', async (c) => {
  /* Query-Paramter werden mit ? an die URL angehängt, hier z.B. http://localhost:3000/v1/names?beginsWith=A */
  const beginsWith = c.req.query('beginsWith'); // Abfrage des Query-Parameters "beginsWith"
  if (beginsWith) {
    // Filtert die Namen, die mit dem angegebenen Präfix beginnen
    const filteredNames = names.filter(name => name.startsWith(beginsWith));
    return c.json(filteredNames);
  } else {
    // Wenn kein Query-Parameter angegeben ist, alle Namen zurückgeben
    return c.json(names);
  }
});

export default app;