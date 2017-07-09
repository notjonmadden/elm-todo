var express = require("express");
var cors = require("cors");
var _ = require("lodash");
var app = express();
var lists = {};
var id = {};
var getId = name => id[name] !== undefined ? id[name] += 1 : (id[name] = 0);

app.use(cors());

app.get("/lists", (req, res) => {
    return res.json(
        Object.keys(lists).map(k => lists[k]).filter(l => !!l));
});

app.post("/lists", (req, res) => {
    var name = req.query.name;
    var list = {
        name: name,
        todos: []
    };

    if (!name || lists[name]) {
        return res.status(400).send();
    }

    lists[name] = list;

    return res.json(list);
});

app.delete("/lists/:name", (req, res) => {
    var name = req.params.name;
    
    if (!name) {
        return res.status(400).send();
    }

    if (!lists[name]) {
        return res.status(404).send();
    }

    delete lists[name];
    return res.status(204).send();
});

app.post("/lists/:name/todos", (req, res) => {
    var list = lists[req.params.name];
    var title = req.query.title;

    if (!list) {
        return res.status(404).send();
    }

    if (!title) {
        return res.status(400).send();
    }

    list.todos.push({
        title: title,
        done: false,
        id: getId(list.name)
    });
    
    return res.json(list.todos[list.todos.length - 1]);
});

app.delete("/lists/:name/todos/:id", (req, res) => {
    var list = lists[req.params.name];
    if (!list) {
        return res.status(404).send();
    }

    if (!req.params.id) {
        console.log("!req.params.id");
        return res.status(400).send();
    }

    list.todos = _.remove(list.todos,
        t => t.id === req.params.id);

    res.status(204).send();
});

app.put("/lists/:name/todos/:id", (req, res) => {
    var list = lists[req.params.name];
    var todo = list ? list.todos[req.params.id] : null;

    if (!list) {
        return res.status(404).send();
    }

    if (!todo) {
        return res.status(404).send();
    }

    if (req.query.title) {
        todo.title = req.query.title.replace(/'/g, '');
    }

    if (req.query.done) {
        todo.done = req.query.done === "true";
    }

    return res.json(todo);
});

app.listen(8080, () => {
    console.log("started");
});