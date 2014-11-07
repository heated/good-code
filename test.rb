def triangle_number(n)
  n * (n + 1) / 2
end

def sum_array(arr)
  arr.reduce(&:+)
end

def prime?(number)
  number != 1 && !(2...number).any? { |factor| number % factor == 0 }
end

def primes_under(max)
  (1...max).select { |number| prime?(number) }
end

def ordered_word?(word)
  for i in 1...word.size
    return false if word[i - 1] > word[i]
  end
  true
end

def encrypt(str)
  encoded = []
  letter = str[0]
  count = 1
  for i in 1..str.size
    if str[i] == letter
      count += 1
    else
      encoded << [letter, count]
      letter = str[i]
      count = 1
    end
  end
  encoded
end

def one_off_words(str, word_list)
  word_list.select do |word|
    count = 0
    word.size.times do |i|
      count += 1 if str[i] != word[i]
    end
    word.size == other.size && count == 1
  end
end

# Interview Problem Candidate
# given two intervals described each by two numbers on a number line, find the intersect and the section of each interval with no overlap
# let a range be [l, r] for [left, right]

def valid?(range)
  range[0] < range[1]
end

def overlap(range1, range2)
  l1, r1 = range1
  l2, r2 = range2
  
  lmax = [l1, l2].max 
  rmin = [r1, r2].min
  
  both = [lmax, rmin]
  both = [] unless valid?(both)

  { 
    range1: [[l1, lmax], [rmin, r1]].select { |range| valid?(range) },
    range2: [[l2, lmax], [rmin, r2]].select { |range| valid?(range) },
    both: both
  }
end

def fibs(max)
  arr = [0, 1]
  (max - 2).times { arr << arr[-1] + arr[-2] }
  arr.take(max)
end

def fib(n) 
  a, b = 0, 1 
  n.times { a, b = b, a + b }
  a
end

require 'matrix'

def matrix_fibs(n)
  (Matrix[[0, 1], [1, 1]] ** n)[1, 0]
end

# Interview Problem Candidate
def stock_picker2(arr)
  low = arr[0]
  high = low
  value = 0

  (arr.size - 1).times do |i|
    for j in i...arr.size
      if (arr[j] - arr[i]) > value
        low, high = i, j
        value = arr[j] - arr[i]
      end
    end
  end
  
  [low, high]
end

def uniq(arr)
  seen = {}
  arr.each { |el| seen[el] = true }
  seen.keys
end

def stock_picker(arr)
  trade = [0, 0]
  value = 0
  low = 0
  high = 0
  for i in 1 .. arr.size - 1
    if arr[i] > arr[high]
      high = i    
      if arr[high] - arr[low] > value
        value = arr[high] - arr[low]
        trade = [low, high]
      end
    elsif arr[i] < arr[low]
      low = i
      high = i
    end
  end
  trade
end

class Array
  def quick_sort(left, right)
    return if left >= right

    pivot_val = self[right]

    left_p =
    right_p = left

    # partition
    while right_p < right
      if self[right_p] < pivot_val
        swap(left_p, right_p)
        left_p += 1
      end
      right_p += 1
    end

    swap(right, left_p)

    quick_sort(left, left_p - 1)
    quick_sort(left_p + 1, right)
  end

  def sort!
    quick_sort(0, self.size - 1)
  end

  def bubble_sort!
    size.times do |i|
      for j in i + 1 ... size
        swap(i, j) if self[i] > self[j]
      end
    end
  end

  def insertion_sort!(left, right)
    sorted = false
    i = left
    until sorted
      sorted = true
    end
  end

  def swap(i, j)
    temp = self[i]
    self[i] = self[j]
    self[j] = temp
  end
end

# Interview Problem Candidate
def binary_step(arr, target, left, right)
  return if left > right
  half = (left + right) / 2
  case target <=> arr[half]
  when 0
    half
  when -1
    binary_step(arr, target, left, half - 1)
  when 1
    binary_step(arr, target, half + 1, right)
  end
end

def binary_search(arr, target)
  binary_step(arr, target, 0, arr.size - 1)
end

def binary_search2(array, target)
  return nil if array.count == 0

  midpoint = array.size / 2
  case target <=> array[midpoint]
  when -1
    binary_search2(array.take(midpoint), target)
  when 0
    midpoint
  when 1
    subproblem_answer = binary_search2(array.drop(midpoint + 1), target)
    subproblem_answer.nil? ? nil : (midpoint + 1) + subproblem_answer
  end
end

def max_sub_sum(arr)
  max = 0
  max_l = 0
  max_r = 0
  current_max = 0
  left = 0

  arr.size.times do |i|
    current_max += arr[i]

    if current_max < 0
      left = i + 1
      current_max = 0
    elsif current_max > max
      max = current_max
      max_l, max_r = left, i
    end
  end
  
  arr[max_l..max_r]
end

def max_deviation(arr, length)
  deviations = []
  max_idx = -1
  min_idx = -1
  (arr.size - length + 1).times do |i|
    max_idx = small_max(arr, i, i + length) if max_idx < i
    min_idx = small_min(arr, i, i + length) if min_idx < i

    new_index = i + length - 1
    new_el = arr[new_index]

    max_idx = new_index if new_el >= arr[max_idx]
    min_idx = new_index if new_el <= arr[min_idx]

    deviations << (arr[max_idx] - arr[min_idx])
  end
  deviations.max
end

def small_max(arr, left, right)
  idx = 0
  max = arr[left]
  for i in left + 1 ... right
    el = arr[i]
    if el >= max
      max = el
      idx = i
    end
  end
  idx
end

def small_min(arr, left, right)
  idx = 0
  min = arr[left]
  for i in left + 1 ... right
    el = arr[i]
    if el <= min
      min = el
      idx = i
    end
  end
  idx
end

# Hill: find the minimum X such that given an array you can make it strictly ascending by adding or subtracting up to X to each element.

def hill(v)
  flat = []
  v.each_with_index do |el, i|
    flat << el - i
  end

  value = 0
  low = flat[0]
  high = flat[0]

  flat.each_with_index do |el|
    if el < low
      low = el
    elsif el > high
      high = el
      low = el
    end

    new_val = high - low
    value = new_val if new_val > value        
  end

  (value + 1) / 2
end

# Challenge 5: Maximum Difference
# Given an array of integer elements, a subsequence of this array is a set of consecutive elements from the array (i.e: given the array v: [7, 8, -3, 5, -1], a subsequence of v is 8, -3, 5)
# Your task is to
# write a function that finds a left and a right subsequence of the array that satisfy the following conditions
# the two subsequences are unique (they don't have shared elements)
# the difference between the sum of the elements in the right subsequence and the sum of the elements in the left subsequence is maximum
# print the difference to the standard output (stdout)
# Note that your function will receive the following arguments:
# v
# which is the array of integers
# Data constraints
# the array has at least 2 and at most 1,000,000 numbers
# all the elements in the array are integer numbers in the following range: [-1000, 1000]



# given an array and a starting index, return whether or not you can get to a zero by jumping forwards or backwards in the array by the numbers you land on

def can_win(array, index)
  positions_to_try = [index]
  visited = []

  until positions_to_try.empty?
    pos = positions_to_try.shift
    value = array[pos]
    next if value.nil? || visited[pos]
    return true if value == 0

    visited[pos] = true
    positions_to_try << pos + value
    positions_to_try << pos - value
  end
end



# TODO: Given an array of integers, find the two subsequences that are disjoint and contiguous such that the difference of their sums is maximized.

def disjoint_attempt(arr)
  part = [0]
  maxPrev = [0]
  maxLeft = []
  minPrev = [0]
  minLeft = []
  max = 0

  # main idea: subsequence sum is partial sum at end minus partial
  # sum at start. Max score is thus either
  # maxLeft[i] + maxPrev[i] - 2 * part[i] or
  #  - minLeft[i] - minPrev[i] + 2 * part[i]
  # for some i


  # calculate partial sums up to each index in the array
  # calculate max of these seen so far at each index
  arr.each_with_index do |el, i|
    k = i + 1
    part[k] = part[i] + el
    maxPrev[k] = [maxPrev[i], part[k]].max
    minPrev[k] = [minPrev[i], part[k]].min
  end

  maxLeft[arr.size] = part[arr.size]
  minLeft[arr.size] = part[arr.size]

  # calculate max partial sum remaining at each index
  # now that we have enough info, update max score
  arr.size.downto(1) do |i|
    k = i - 1
    maxLeft[k] = [maxLeft[i], part[k]].max
    minLeft[k] = [minLeft[i], part[k]].min
    max = [max, maxLeft[i] + maxPrev[i] - 2 * part[i],
            0 - minLeft[i] - minPrev[i] + 2 * part[i]].max
  end

  # loop doesn't catch this
  max = [max, maxLeft[0] + maxPrev[0] - 2 * part[0],
          0 - minLeft[0] - minPrev[0] + 2 * part[0]].max
  return max
end

def disjoint_control(arr)
  hash = Hash.new { 0 }
  for i in 0...arr.size
    for j in i...arr.size
      hash[[i, j]] = arr[i..j].inject(&:+)
    end
  end

  max = 0
  for i in 0...arr.size
    for j in i - 1...arr.size
      for k in j...arr.size
        left = hash[[i, j]]
        right = hash[[j + 1, k]]
        new_max = (left - right).abs
        max = new_max if new_max > max
      end
    end
  end
  max
end



class DoubleLinkedList
  attr_accessor :value, :parent, :child, :size
  
  def initialize(value = nil, parent = nil, child = nil)
    @value, @parent, @child = value, parent, child
    @child.parent = self if @child
    @parent.child = self if @parent
  end

  def prev
    @parent
  end

  def next
    @child
  end

  def delete
    @parent.child = @child
    @child.parent = @parent
  end

  def unshift(link)
    link.parent = @parent
    link.child = self
  end

  def at(n)
    link = self
    while n > 0
      link = link.child
      return unless link
      n -= 1
    end
    link.value
  end

  def self.from_arr(arr)
    parent = nil
    arr.reverse.each do |el|
      parent = DoubleLinkedList.new(el, nil, parent)
    end

    parent
  end

  def size
    result = 0
    link = self
    while link
      link = link.child
      result += 1
    end
    result
  end

  def to_s
    result = ""
    link = self
    while link
      result << "#{link.value} "
      link = link.child
    end
    result
  end
end

# Interview Problem Candidate
def move_zeros(arr)
  pointer = 0

  arr.each_with_index do |el, i|
    if el != 0
      arr[i] = 0
      arr[pointer] = el
      pointer += 1
    end
  end
end

# Interview Problem Candidate
def index_of(string, substr)
  for i in 0...string.size - substr.size
    same = true
    substr.size do |j|
      same = false if string[i + j] != substr[j]
    end
    return i if same
  end
  nil
end

# time: O(n^2), space: O(n)
def is_shuffle?(str1, str2, str3)
  return false unless str1.size + str2.size == str3.size
  seen = {}
  considering = [[0, 0]]

  str3.each_char do |chr|
    next_gen = []
    considering.each do |pair|
      next if seen[pair]
      seen[pair] = true
      idx1, idx2 = pair

      next_gen << [idx1 + 1, idx2] if str1[idx1] == chr
      next_gen << [idx1, idx2 + 1] if str2[idx2] == chr
    end
    considering = next_gen
  end

  !considering.empty?
end

def sierpinski
  arr = Array.new(64) { false }
  arr[31] = true
  32.times do
    output = ""
    arr.each do |cell|
      output.concat(cell ? "*" : "-")
    end
    puts output

    next_gen = []
    arr.each_index do |i|
      next_gen[i] = arr[i - 1] ^ arr[i + 1]
    end

    arr = next_gen
  end
end

# Interview Problem Candidate
def permutations(arr)
  return [[]] if arr.empty?
  results = []
  arr.each_with_index do |el, i|
    results.concat(
      permutations(arr.take(i) + arr.drop(i + 1)).map { |perm| perm.push(el) }
    )
  end
  results
end

def fizzbuzz
  for i in 1..100
    puts case 15.gcd(i)
    when 1
      i
    when 3
      "Fizz"
    when 5
      "Buzz"
    else
      "FizzBuzz"
    end
  end
end

def next_permutation(n)
  ordering = n.to_s.split("").map(&:to_i)
  k = nil

  for i in 0...ordering.size - 1
    k = i if ordering[i] < ordering[i + 1]
  end

  return unless k
  l = nil
  
  for i in k ... ordering.size
    l = i if ordering[k] < ordering[i]
  end

  ordering[l], ordering[k] = ordering[k], ordering[l]

  new_order = (ordering.take(k + 1) + ordering[k + 1 .. -1].reverse)

  new_order.join.to_i
end

# time to parse some lisp

# input:  string with newlines and tabs
# output: parens around function blocks

# iterate through characters in the string
# there are several states the parser can be in
# the token counting state, where line tokens are counted up and tabs are counted
# the tab counting state, where tabs are being read to determine the structure of the following line
# the ("[ state, where a stack is built up, newlines, tabs, and spaces are ignored, and only one token is being created
# the backslash state, where the following character is ignored

def parse_lisp(str)
	# TODO: handle the tab tree and resulting token structure

	depth_stack = [] # can have (, ", and [
  current_token = nil
  state_stack = []
  state = "token counting"
  tab_count = 0

  str.each_char do |char|
  	current_token << char
  	if state == "tab counting" && char != "\t"
  		# do stuff with tabs and structure
  		tab_count = 0
  	end
  	
  	if char == "\\"
  		state = "backslash"
  	else
		  case state
		  when "token counting"

		  when "tab counting"

		  when "within paren, quote, or bracket"

		  	case char
		  	when ")"
					depth_stack.pop if depth_stack[-1] == '('
				when "]"
					depth_stack.pop if depth_stack[-1] == '['
		  	when '"'
		  		depth_stack.pop if depth_stack[-1] == '"'
				else
					if "([".include?(char)
						depth_stack << char
					else
						
					end
				end
		  when "backslash"
		  	state = state_stack.pop
		  end
		end
	end

  puts pre_parse
end

# p parse_lisp(<<-code)
# def prime? (n)
#   and
#     isnt 1 n
#     none [multiple n _] (range 2 sqrt.n)
# code

# def read (tokens)
#   stack = []
#   current_list = []
#   tokens.each do |token|
#     case token
#     when "("
#       stack.push(current_list)
#       current_list = []
#     when ")" 
#       parent = stack.pop
#       parent.push(current_list)
#       current_list = parent
#     else
#       current_list.push(sym.token)
#     end
#     rev.current_list
#   end
# end


# def think (states memory)
#   (mat-trans:list:mat-mul memory mat-trans:list.states))
# end

# # for each piece of memory, multiply by a bias based on the state in question and the utility to obtain the new state
# def update (memory states utility)

# end

# def stress_test(n)
#   think(mat(n) mat(n, n))
# end

class Node
  attr_accessor :level, :nw, :ne, :sw, :se
  def initialize(nw, ne, sw, se, level = nil)
    @nw, @ne, @sw, @se = nw, ne, sw, se
    @level = level || nw.level + 1
  end

  def centered_subnode
    Node.new(nw.se,
             ne.sw,
             sw.ne,
             se.nw)
  end

  def centered_horizontal(west, east)
    Node.new(west.ne.se,
             east.nw.sw,
             west.se.ne,
             east.sw.nw) 
  end

  def centered_vertical(north, south)
    Node.new(north.sw.se,
             north.se.sw,
             south.nw.ne,
             south.ne.nw)
  end

  def centered_sub_subnode
    Node.new(nw.se.se,
             ne.sw.sw,
             sw.ne.ne,
             se.nw.nw)
  end

  def next_generation
    if level == 2
      # ... do base case through normal simulation ...
    else
      n00 = nw.centered_subnode
      n01 = centered_horizontal(nw, ne)
      n02 = ne.centered_subnode
      n10 = centered_vertical(nw, sw)
      n11 = centered_sub_subnode
      n12 = centered_vertical(ne, se)
      n20 = sw.centered_subnode
      n21 = centered_horizontal(sw, se)
      n22 = se.centered_subnode
      Node.new(Node.new(n00, n01, n10, n11).next_generation,
               Node.new(n01, n02, n11, n12).next_generation,
               Node.new(n10, n11, n20, n21).next_generation,
               Node.new(n11, n12, n21, n22).next_generation)
    end
  end

  def horizontal_forward(west, east)
    Node.new(west.ne,
             east.nw,
             west.se,
             east.sw).next_generation
  end

  def vertical_forward(north, south)
    Node.new(north.sw,
             north.se,
             south.nw,
             south.ne).next_generation
  end

  def centered_forward
    Node.new(nw.se,
             ne.sw,
             sw.ne,
             se.nw).next_generation
  end

  def next_generation
    if (level == 2)
      # ... do base case through normal simulation ...
    else
      n00 = nw.next_generation
      n01 = horizontal_forward(nw, ne)
      n02 = ne.next_generation
      n10 = vertical_forward(nw, sw)
      n11 = centered_forward
      n12 = vertical_forward(ne, se)
      n20 = sw.next_generation
      n21 = horizontal_forward(sw, se)
      n22 = se.next_generation
      Node.new(Node.new(n00, n01, n10, n11).next_generation,
               Node.new(n01, n02, n11, n12).next_generation,
               Node.new(n10, n11, n20, n21).next_generation,
               Node.new(n11, n12, n21, n22).next_generation)
    end
  end
end


def largest_product(grid)
  max = 0

  [[0, 1], [1, 1], [1, 0], [1, -1]].each do |dir|
    x, y = dir
    grid.each_with_index do |row, i|
      row.each_index do |j|
        product = 1
        4.times { |d| product *= get_multiplier(grid, i + d * x, j + d * y) }
        max = product if product > max
      end
    end
  end

  max
end

def get_multiplier(grid, i, j)
  (i < 0 || j < 0 || i >= grid.size || j >= grid[0].size) ? 1 : grid[i][j]
end

# triangle.each_with_index do |row, i|
#   row.each_with_index do |num, j|
#     tails = []
#     tails << triangle[i - 1][j - 1] if i > 0 && j > 0
#     tails << triangle[i - 1][j    ] if i > 0 && j < i
#     num += tails.max if tails.max
#     triangle[i][j] = num
#   end
# end

# puts triangle.last.max

# triangle = []

# 100.times do |i|
#   triangle[i] = []

#   (i + 1).times do |j|
#     triangle[i][j] = j == 0 || j == i ? 1 : triangle[i - 1][j - 1] + triangle[i - 1][j]
#   end
# end

# p triangle

# Project Euler Problem 81
# Find the minimum sum of a path going from the top left corner to the bottom right of a matrix.
#
# def minimum_path_sum(matrix)
#   matrix.each_with_index do |row, i|
#     row.each_index do |j|
#       tails = []
#       tails << matrix[i - 1][j] if i > 0
#       tails << matrix[i][j - 1] if j > 0

#       matrix[i][j] += tails.min if tails.min
#     end
#   end
# end

# minimum_path_sum(matrix)
# puts matrix.last.last

# factorial golf
  # f=lambda{|n|n<1?1:n*f[n-1]}
  # def f(n)n<1?1:n*f(n-1)end
  # f=->n{n<1?1:n*f[n-1]}

  # (= f[if(< _ 1)1(* _ f:1-._)])
  # product:range
  # (mac ? args `(if ,@args))
  # (= 0? zero)
  # (= f[? 0?._ 1(* _ f:1-._)])

# note: If the markov chain is not generated in such a way that it loops, generating new words once you reach the last word may fail.
def markov_chain(str, depth)
  chain = Hash.new { |hash, key| hash[key] = [] }
  parsed_text = str.split
  for pattern_length in 1 .. depth
    (parsed_text.size - pattern_length).times do |i|
      chain[parsed_text[i ... i + pattern_length]] << parsed_text[i + pattern_length]
    end
  end
  chain
end

def generate_markov_string(markov_chain, depth, size)
  words = markov_chain.keys.sample
  all_words = markov_chain.values.flatten
  until words.size >= size
    possible_words = (1..depth).map{ |i| markov_chain[words.last(i)] }.flatten
    words << (possible_words.empty? ? all_words.sample : possible_words.sample)
  end
  words.join(' ')
end

def markov_string(str, size)
  generate_markov_string(markov_chain(str, 3), 3, size)
end

# ont_text = "seems like a lot of them wind up just being first and last half of a message being switched."

# puts markov_string(ont_text, 50)


# games = 

class Card
  values = [2,3,4,5,6,7,8,9,:T,:J,:Q,:K,:A].map(&:to_s)
  @@tiers = {}
  values.each_with_index{ |el, i| @@tiers[el] = i }

  attr_reader :value, :suite
  def initialize(str)
    @value = str[0]
    @suite = str[1]
  end

  def tier
    @@tiers[@value]
  end
end

class Array
  def histogram
    Hash[*self.group_by{ |v| v }.flat_map{ |k, v| [k, v.size] }]
  end

  def sum
    self.inject(&:+)
  end
end

def values(hand)
  hand.map(&:value)
end

def duplicate_value_count(hand)
  values(hand).histogram.select{ |value, count| count > 1 }.size
end

def uniq_value_count(hand)
  values(hand).uniq.size
end

def hand_tier(hand)
  unique          = -> hand { uniq_value_count(hand) == 5 }
  pair            = -> hand { uniq_value_count(hand) == 4 }
  two_pair        = -> hand { uniq_value_count(hand) == 3 && duplicate_value_count(hand) == 2 }
  three_of_a_kind = -> hand { uniq_value_count(hand) == 3 && duplicate_value_count(hand) == 1 }
  straight        = -> hand { 
    tiers = hand.map(&:tier)
    unique[hand] && (tiers.sum - tiers.min * 5 == 10)
  }
  flush           = -> hand { hand.map(&:suite).uniq.size == 1 }
  full_house      = -> hand { uniq_value_count(hand) == 2 && duplicate_value_count(hand) == 2 }
  four_of_a_kind  = -> hand { uniq_value_count(hand) == 2 && duplicate_value_count(hand) == 1 }
  straight_flush  = -> hand { straight[hand] && flush[hand] }

  qualifications = [straight_flush, four_of_a_kind, full_house, flush, straight, three_of_a_kind, two_pair, pair, unique]
  qualifications.size - qualifications.index{ |qualification| qualification[hand] }
end

def pair_encode(hand)
  run_length_values = hand.map(&:tier).histogram.map(&:reverse).sort.reverse.map(&:last)
end

def player_1_wins(hand1, hand2)
  if hand_tier(hand1) != hand_tier(hand2)
    hand_tier(hand1) > hand_tier(hand2)
  else
    (pair_encode(hand1) <=> pair_encode(hand2)) == 1
  end
end

def hand(cards)
  cards.split.map{ |str| Card.new(str) }
end

# puts games.map{ |hands|
#   hand1, hand2 = hands
#   player_1_wins(hand(hand1), hand(hand2)) ? 1 : 0
# }.sum



















# a.edges = [b, d]
# b.edges = [a, c, e]
# ...

# ASSUMED GRAPH FORMAT
# each node has some edges and a value
# the edges are pointers to other nodes, AKA just nodes
# node[:edges]
# node[:value]

# graph is assumed to be connected
class Node
  attr_accessor :value, :children, :distance, :parent
  def initialize(value, children = [])
    @value = value
    @children = children
  end
end

def slow_dijkstras(start, finish)
  visited = {}
  start.distance = start.value
  visited_nodes = [start]
  current_node = nil

  until visited_nodes.index(finish)
    candidate_paths = visited_nodes.map do |node| 
      node.children
          .select{ |node| !visited[node] }
          .map{ |new_node|
            { node: new_node, distance: new_node.value + node.distance }
          }
    end

    new_path = candidate_paths.flatten.min_by{ |path| path[:distance] }

    current_node = new_path[:node]
    current_node.distance = new_path[:distance]

    visited_nodes << current_node
    visited[current_node] = true
  end

  finish.distance
end

# matrix = [[131,673,234,103,18],[201,96,342,965,150],[630,803,746,422,111],[537,699,497,121,956],[805,732,524,37,331]]

def construct_graph(matrix)
  # a node for every index, and an edge for every direction! (except left)
  new_matrix = matrix.map do |row|
    row.map{ |value| Node.new(value) }
  end

  new_matrix.each_with_index do |row, x|
    row.each_with_index do |node, y|
      # complicated children adding logic
      [[-1, 0],[0, 1], [1, 0], [0, -1]].each do |direction| # [0, -1] would be left
        dx, dy = direction
        new_x, new_y = x + dx, y + dy
        if (0...new_matrix.size).member?(new_x) && (0...row.size).member?(new_y)
          node.children << new_matrix[new_x][new_y]
        end
      end
    end
  end

  # construct start and finish nodes via children to / from the first / last elements of each row
  # start = Node.new(0, new_matrix.map(&:first))
  # last = Node.new(0)
  # new_matrix.map(&:last).each{ |node| node.children << last }

  [new_matrix.first.first, new_matrix.last.last]
end

# start, finish = construct_rightwards_graph(matrix)
# puts slow_dijkstras(start, finish)

