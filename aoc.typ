#let day1() = {
  // We get an empty string at the end for some reason
  let lines = read("data/day1.txt").split("\n").slice(0,-1)

  let line_part1(line) = {
    let first_digit = line.find(regex("\d"))
    let last_digit = line.rev().find(regex("\d"))
    int( first_digit + last_digit )
  }

  [ Part 1: #lines.map(line_part1).sum() \ ]

  let words = ( one: "1", two: "2", three: "3", four: "4", five: "5", six: "6", seven: "7", eight: "8", nine: "9",)

  let all_numbers = words

  for (key, value) in words {
    all_numbers.insert(key.rev(), value)
    all_numbers.insert(value, value)
  }

  let key_regex = regex( (words.keys() + ("\d",)).join("|") )
  let rev_key_regex = regex( (words.keys().map(str.rev) + ("\d",)).join("|") )

  let line_part2(line) = {
    let first_digit = all_numbers.at( line.match(key_regex).text )
    let last_digit = all_numbers.at( line.rev().match(rev_key_regex).text )
    int( first_digit + last_digit )
  }

  [ Part 2: #lines.map(line_part2).sum() ]
}

= Day 1

#day1()

