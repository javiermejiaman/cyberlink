import express from 'express';
import path from 'path';

const app = express();

app.use(express.static(path.join(__dirname, 'public')));

app.get('/', (req, res) => { res.sendFile(path.join(__dirname, 'views/index.html')) });

app.listen({ port: 4000 }, () => {
  console.log(`Server ready at http://localhost:4000`);
});
