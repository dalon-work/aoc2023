#let day1_part1() = {
  // We get an empty string at the end for some reason
  let lines = read("data/day1.txt").split("\n").slice(0,-1)

  let sum = 0

  for line in lines {
    let first_digit = line.find(regex("\d"))
    let last_digit = line.rev().find(regex("\d"))

    sum += int( first_digit + last_digit )

  }

  [ Part 1: #sum ]
}

#let day1_part2() = {
  let words = (
    one: "1",
    two: "2",
    three: "3",
    four: "4",
    five: "5",
    six: "6",
    seven: "7",
    eight: "8",
    nine: "9",
  )

  let all_numbers = words

  for (key, value) in words {
    all_numbers.insert(key.rev(), value)
    all_numbers.insert(value, value)
  }

  let key_regex = regex( (words.keys() + ("\d",)).join("|") )
  let rev_key_regex = regex( (words.keys().map(str.rev) + ("\d",)).join("|") )

  // We get an empty string at the end for some reason
  let lines = read("data/day1.txt").split("\n").slice(0,-1)

  let sum = 0

  for line in lines {
    let first_digit = all_numbers.at( line.match(key_regex).text )
    let last_digit = all_numbers.at( line.rev().match(rev_key_regex).text )

    sum += int( first_digit + last_digit )
  }

  [ Part 2: #sum ]

}

= Day 1

#day1_part1()

#day1_part2()


