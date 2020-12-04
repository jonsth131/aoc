module AoC2020.Day01

let input = Utils.readInputAsInts "Day01.txt"

let rec findTwoInArray (input: int []) sum l r =
    let currentSum = input.[l] + input.[r]
    if currentSum = sum then input.[l] * input.[r]
    elif currentSum > sum then findTwoInArray input sum l (r - 1)
    else findTwoInArray input sum (l + 1) r

let rec findThreeInArray (input: int []) sum i l r =
    let currentSum = input.[i] + input.[l] + input.[r]
    if i > input.Length - 2 || l > r
    then findThreeInArray input sum (i + 1) (i + 2) (input.Length - 1)
    elif currentSum = sum
    then input.[i] * input.[l] * input.[r]
    elif currentSum > sum
    then findThreeInArray input sum i l (r - 1)
    else findThreeInArray input sum i (l + 1) r

let findTwo sum =
    findTwoInArray (input |> Array.sort) sum 0 (input.Length - 1)

let findThree sum =
    findThreeInArray (input |> Array.sort) sum 0 1 (input.Length - 1)

let part1 = findTwo 2020

let part2 = findThree 2020
