even / 2
odd * 3 + 1
3



memory = {}

def collatz (x):
    count = 0
    while x != 1:
        if x in memory:
            count += memory[x]
            return count
        if x % 2 == 0:
            x = x / 2
        else:
            x = (x * 3) + 1
        count += 1
    return count

longest = 0
for x in range(1,10000):
    length = collatz(x)
    memory[x] = length
    if length > longest:
        longest = length
    
