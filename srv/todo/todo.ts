import { map, max, remove } from "lodash";

interface TodoList {
    readonly id: number;
    name: string;
    readonly todos: ReadonlyArray<Todo>;

    addTodo(title: string): Todo;
    removeTodo(todoId: number): Todo;
}

interface Todo {
    readonly id: number;
    title: string;
    done: boolean;
}

interface TodoListFactory {
    create(name: string): TodoList;
}

interface TodoListRepository {
    getAll(): TodoList[];
    get(id: number): TodoList | null;
    add(list: TodoList): void;
    update(list: TodoList): void;
    delete(id: number): TodoList;
}

class IncrementingIdTodoListFactory implements TodoListFactory {
    constructor(private nextId: number = 0) { }

    create(name: string): TodoList {
        const nextId = this.nextId;

        this.nextId += 1;

        return new class implements TodoList {
            readonly id = nextId;
            private _name = name;
            private _todos: Todo[] = [];

            public get name() {
                return this._name;
            }

            public set name(val) {
                if (!/\w+/.test(val)) {
                    throw Error(`invalid TodoList name: ${val}`);
                }

                this._name = name;
            }

            public get todos() {
                return this._todos;
            }

            public addTodo(title: string) {
                const id = max(map(this.todos, t => t.id)) || -1;
                const todo: Todo = {
                    id: id + 1,
                    done: false,
                    title: title
                };

                this._todos.push(todo);

                return todo;
            }

            public removeTodo(id: number) {
                const todo = remove(this._todos, t => t.id === id)[0];

                return todo || null;
            }
        };
    }
}

class InMemoryTodoListRepository {
    
}

class InMemoryTodoRepository {

}