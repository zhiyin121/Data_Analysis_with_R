norms_freqs_noun_address = r"E:\Uni-Stuttgart\2nd\Introduction and Application of Programming in R\Project\combine\norms_freqs_noun.txt"
norms_freqs_noun = open(norms_freqs_noun_address, "r+")


norms_freqs_adj_address = r"E:\Uni-Stuttgart\2nd\Introduction and Application of Programming in R\Project\combine\norms_freqs_adj.txt"
norms_freqs_adj = open(norms_freqs_adj_address, "r+")


'''extrat freqs_noun word list'''
word_list_n = []
for line in norms_freqs_noun.readlines():
    if line == None:
        break
    else:
        word = line.split()[0]
        word_list_n.append(word)
print(word_list_n)


'''extrat freqs_adj word list'''
word_list_a = []
for line in norms_freqs_adj.readlines():
    if line == None:
        break
    else:
        word = line.split()[0]
        word_list_a.append(word)
print(word_list_a)
