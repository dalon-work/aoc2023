#let day4() = {
  [= Day 4]
  let cards = read("data/day4.txt").split("\n").slice(0,-1)

  let card_sum = 0

  for card in cards {
    let points = 0
    let (ws, hs) = card.split(":").at(1).split("|")

    let winning = ws.split()
    let have = hs.split()
    for h in have {
      if winning.contains(h) {
        if points == 0 {
          points = 1
        } else {
          points = points + points
        }
      }
    }
    card_sum += points
  }

  [Part 1: #card_sum \ ]

  let copies = (1,)
  for c in cards.slice(0,-1) {
    copies.push(1)
  }

  for (i,c) in cards.enumerate() {
    let matches = 0
    let (ws, hs) = c.split(":").at(1).split("|")

    let winning = ws.split()
    let have = hs.split()
    for h in have {
      if winning.contains(h) {
        matches += 1
      }
    }

    let j = 0;
    while j < matches {
      copies.at(i+j+1) += copies.at(i)
      j += 1
    }
  }

  [Part 2: #copies.sum()]

}

#let day2() = {
  [= Day 2]
  let games = read("data/day2.txt").split("\n").slice(0,-1)

  let game_regex = regex("(\d*) (red|green|blue)")

  let max_cubes = ( red : 12, green : 13, blue : 14,)

  let game_part1_is_valid(game) = {
    for m in game.matches(game_regex) {
      if int(m.captures.at(0)) > max_cubes.at(m.captures.at(1)) {
        return false
      }
    }
    return true
  }

  [Part 1: #games.filter(game_part1_is_valid).map(game => int(game.split(":").at(0).split(" ").at(1)) ).sum()\ ]

  let game_part2_power(game) = {
    let cubes = (red : (0,), green : (0,), blue : (0,),)

    for m in game.matches(game_regex) {
      cubes.at( m.captures.at(1) ).push( int(m.captures.at(0)) ) 
    }

    return cubes.values().map(a => calc.max(..a) ).product() 
  }

  [Part 2: #games.map(game_part2_power).sum()\ ]
}

#let day1() = {
  [= Day 1]

  // We get an empty string at the end for some reason
  let lines = read("data/day1.txt").split("\n").slice(0,-1)

  let line_part1(line) = {
    let first_digit = line.find(regex("\d"))
    let last_digit = line.rev().find(regex("\d"))
    int( first_digit + last_digit )
  }

  [Part 1: #lines.map(line_part1).sum()\ ]

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

  [Part 2: #lines.map(line_part2).sum()\ ]
}

#day1()
#day2()
#day4()

