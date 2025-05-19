import fs from 'fs/promises';

class Person {
    constructor(public name: string, public age: number, public student: boolean) {}
}

let Bob = new Person("Bob",21,true)
let Alice = new Person("Alice", 12, false);
let Tom = new Person("Tom", 25, true);
let Tina = new Person("Tina", 68, false);
const people:Person[] = [Bob,Alice,Tom,Tina]


const PersonalHTML = function (per:Person):string {
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
await fs.writeFile(`htmlFor${individual.name}.html`, PersonalHTML(individual));
fs.unlink(`htmlFor${individual.name}.html`,);
} // aks what is happening with the files



const delay = (time: number): Promise<void> => {
  return new Promise((resolve) => setTimeout(resolve, time));
};

const squareVoid = function(n:number){
    console.log(n)
    delay(1000).then(() => console.log(n*n))
}

const squarePromiseAsy = async function(n:number): Promise<number>{
    await delay(1000)
    return n*n
}

const squarePromiseThen = function(n:number): Promise<number>{
    let res = delay(1000).then(() => n*n)
    return res
}

let delayTimes =  async function(n:number, times:number):Promise<number> {
    let j = delay(1000)
    while(times>0){
        times -= 1
        j = j.then(() => delay(1000))
    }
    return j.then(()=>n*n)
}

const squareMultiple= function (nums:number[]):Promise<number>[]{
    let result = nums.map(x => squarePromiseThen(x))
    return result
}

const squareMultipleAsyncAwait = async function (nums: number[]):Promise<number[]>{
    let result = []
    nums.forEach(async (elem) => {
        result.push(await squarePromiseAsy(elem));
    })
    return result
}

//uareVoid(1)
//console.log(await squarePromiseAsy(3))
//console.log(await squarePromiseThen(2))
//console.log(await delayTimes(3,3))
const exArr:number[] = [1,2,3,4]
let arr:any = squareMultiple(exArr)
arr = await Promise.all(arr).catch(error => console.log(`${error} bei dem versuch die Promises zu lösen `))
console.log(arr)

let res = await squareMultipleAsyncAwait(exArr)
console.log(res)
