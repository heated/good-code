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
  for i in 0..word.size - 2
    return false if word[i] > word[i + 1]
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
    count == 1
  end
end

# given two intervals described each by two numbers on a number line, find the intersect and the section of each interval with no overlap
def overlap(range1, range2)
  results = {
    :range1 => no_overlap(range1, range2),
    :range2 => no_overlap(range2, range1),
    :both => []
  }

  # find intersect
  l1, r1 = range1
  l2, r2 = range2

  if l1 < r2 && l2 < r1
    results[:both] = [[l1, l2].max, [r1, r2].min]
  end
  results
end

def no_overlap(range1, range2)
  l1, r1 = range1
  l2, r2 = range2
  lbr = l1 <= r2 # left before right
  lal = l1 >= l2 # left after left
  rbr = r1 <= r2 # right before right
  ral = r1 >= l2 # right after left
  return [] if lal && rbr # totally covered
  return [[r2, r1]] if lbr && lal
  return [[l1, l2]] if rbr && ral
  return [[l1, l2], [r2, r1]] if !lal && !rbr # range2 in the middle of range1

  range1 # range2 not intersecting range1
end

def fibs(max)
  arr = [0, 1]
  (max - 2).times { arr << arr[-1] + arr[-2] }
  arr.take(max)
end

def fib(n) 
  a, b = 1, 1 
  n.times { a, b = b, a + b }
  a 
end

require 'matrix'

def matrix_fibs(n)
  (Matrix[[0, 1], [1, 1]] ** n)[1, 0]
end

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

def index_of(mainstr, substr)
  for i in 0...mainstr.size - substr.size
    same = true
    for j in 0...substr.size
      same = false if mainstr[i + j] != substr[j]
    end
    return i if same
  end
  -1
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
