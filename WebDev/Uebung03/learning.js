

console.log("Hello World")
for (let i = 1; i < 11; i++) {
    console.log("simple for: " + i*i)
}

for (let j = 0; j < 10; j++) {
    let sum = 0;
    for(let i = 0; i < j; i++) {
        sum += 2*i+1;
    }
    console.log("odd loops: "+sum);
}
const a = Array.from({length: 10}, (_, i) => (i+1)**2)
console.log(a)
function squares() {
    let arr = Array.from({length: 10}, (_, i) => i+1)
    let sum = 0;
    for (const x of arr) {
        sum += x;
    }
    console.log(sum)
    sum = 0;
    arr.forEach(x => sum +=x)
    console.log(sum)
    console.log(arr.reduce((anotherSum,x) => anotherSum +x))
    console.log(arr.map(x => x*x))
} 

squares();