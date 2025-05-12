import fs from 'fs/promises';

class Person {
    constructor(public name: string, public age: number, public student: boolean) {}
}

let Bob = new Person("Bob",21,true)
let Alice = new Person("Alice", 12, false);
let Tom = new Person("Tom", 25, true);
let Tina = new Person("Tina", 68, false);
const people:Person[] = [Bob,Alice,Tom,Tina]


let PersonalHTML = function (per:Person):string {
    let html:string = ` 
    <!DOCTYPE html>
    <html>
        <head></head>
        <body>
            <h1>Name: ${per.name}</h1>
            <h1>Age: ${per.age}</h1>
            <h1>Is a Student: ${per.student}</h1>
        </body>
    </html>`
    return html;
}


for (const individual of people) {
fs.writeFile(`htmlFor${individual.name}.html`, PersonalHTML(individual));
fs.unlink(`htmlFor${individual.name}.html`,);
} // aks what is happening with the files

const delay = (time: number): Promise<void> => {
  return new Promise((resolve) => setTimeout(resolve, time));
};

let squareVoid = function(n:number){
    console.log(n)
    setTimeout(() =>console.log("helloWorld"),n*n)
}

let squarePromiseAsy = async function(n:number): Promise<number>{
    console.log(n)
    await delay(n)
    return n*n
}

let squarePromiseThen = function(n:number): Promise<number>{
    console.log(n)
    let res = delay(1000).then(() => n*n).catch(err => {console.log(err)})
    return res
}


//squareVoid(100)
//console.log(await squarePromiseAsy(1000))
console.log(squarePromiseThen(1000))

