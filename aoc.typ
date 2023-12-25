#let day6() = {
  [= Day 6]

  let times1 = (42, 89, 91, 89)
  let dists1 = (308, 1170, 1291, 1467)

  // Part 2
  let times2 = (42899189,)
  let dists2 = (308117012911467,)

  // Example
  //let times = (7, 15, 30)
  //let dists = (8, 40, 200)

  let ways(times, dists) = {
    let ranges = ()
    for (T, D) in times.zip(dists) {
      let zero_n = calc.floor( (T - calc.sqrt(calc.pow(T,2) - 4*D))/2 + 1)
      let zero_p = calc.ceil( (T + calc.sqrt(calc.pow(T,2) - 4*D))/2 - 1 )
      let valid_range = zero_p - zero_n + 1
      ranges.push( valid_range )
    }
    return ranges.product()
  }

  [ Part 1: #ways(times1, dists1) \ ]
  [ Part 2: #ways(times2, dists2) \ ]

}

#let day5() = {
  [= Day 5]

  let sections = read("data/day5.txt").split("\n\n")

  let preprocess(array) = {
    array.split("\n").slice(1).filter( s => s.len() > 0 ).map( s => s.split().map(int)).sorted( key: a => a.at(1) )
  }

  let seed_to_soil = preprocess(sections.at(1))
  let soil_to_fert = preprocess(sections.at(2))
  let fert_to_watr = preprocess(sections.at(3))
  let watr_to_lght = preprocess(sections.at(4))
  let lght_to_temp = preprocess(sections.at(5))
  let temp_to_hmty = preprocess(sections.at(6))
  let hmty_to_locn = preprocess(sections.at(7))

  let dest(src, ranges) = {
    let dst = src
    let i = 0
    for (dst_start, src_start, rlen) in ranges {
      if src_start <= src and src < src_start + rlen {
        return dst_start + (src - src_start)
      }
    }
    return dst
  }

  let seeds = sections.at(0).split().slice(1).map(int)

  let locs = ()

  for seed in seeds {
    let soil = dest(seed, seed_to_soil)
    let fert = dest(soil, soil_to_fert)
    let watr = dest(fert, fert_to_watr)
    let lght = dest(watr, watr_to_lght)
    let temp = dest(lght, lght_to_temp)
    let hmty = dest(temp, temp_to_hmty)
    let locn = dest(hmty, hmty_to_locn)
    locs.push(locn)
  }

  [ Part 1: #calc.min(..locs) \ ]

  let dest_part2(src, ranges) = {
    let dst = ( src, calc.pow(2,62) )
    let i = 0
    for (dst_start, src_start, rlen) in ranges {
      if src_start <= src and src < src_start + rlen {
        dst.at(0) = dst_start + (src - src_start)
        dst.at(1) = (src_start + rlen) - src
        return dst
      }
    }
    return dst
  }

  let seed_range_idx = 0
  let locs = ()

  while seed_range_idx < seeds.len() {
    let test_seed = seeds.at(seed_range_idx)
    let seed_range_end = test_seed + seeds.at(seed_range_idx+1)
    seed_range_idx += 2
    while test_seed < seed_range_end {
      let (soil, soil_range) = dest_part2(test_seed, seed_to_soil)
      let (fert, fert_range) = dest_part2(soil, soil_to_fert)
      let (watr, watr_range) = dest_part2(fert, fert_to_watr)
      let (lght, lght_range) = dest_part2(watr, watr_to_lght)
      let (temp, temp_range) = dest_part2(lght, lght_to_temp)
      let (hmty, hmty_range) = dest_part2(temp, temp_to_hmty)
      let (locn, locn_range) = dest_part2(hmty, hmty_to_locn)
      locs.push(locn)

      test_seed += calc.min(soil_range, fert_range, watr_range, lght_range, temp_range, hmty_range, locn_range)
    }
  }

  [ Part 2: #calc.min(..locs) ]
}

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
#day5()
#day6()

