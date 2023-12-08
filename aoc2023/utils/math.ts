export function gcd2(a: number, b: number): number {
    if (!b) return b === 0 ? a : NaN;
    return gcd2(b, a % b);
}

export function gcd(array: number[]): number {
    var n = 0;
    for (var i = 0; i < array.length; ++i)
        n = gcd2(array[i], n);
    return n;
}

export function lcm2(a: number, b: number): number {
    return a * b / gcd2(a, b);
}

export function lcm(array: number[]): number {
    var n = 1;
    for (var i = 0; i < array.length; ++i)
        n = lcm2(array[i], n);
    return n;
}
