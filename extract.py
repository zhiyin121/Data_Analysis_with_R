import re

concrete_address = r"E:\Uni-Stuttgart\2nd\Introduction and Application of Programming in R\Project\Norms\brysbaert-et-al_2014.txt"
concrete = open(concrete_address, 'rb')

norms_address = r"E:\Uni-Stuttgart\2nd\Introduction and Application of Programming in R\Project\Norms\Lynott-norms.txt"
norms = open(norms_address, 'rb')

freqs_address = r"E:\Uni-Stuttgart\2nd\Introduction and Application of Programming in R\Project\Freqs\freqs_lemmas_pos.txt"
freqs = open(freqs_address,'rb')

norms_concerte_address = r"E:\Uni-Stuttgart\2nd\Introduction and Application of Programming in R\Project\combine\norms_concerte.txt"
norms_concerte = open(norms_concerte_address, "r+")

norms_freqs_address = r"E:\Uni-Stuttgart\2nd\Introduction and Application of Programming in R\Project\combine\norms_freqs.txt"
norms_freqs = open(norms_freqs_address, "r+")

norms_freqs_noun_address = r"E:\Uni-Stuttgart\2nd\Introduction and Application of Programming in R\Project\combine\norms_freqs_noun.txt"
norms_freqs_noun = open(norms_freqs_noun_address, "r+")

norms_freqs_adj_address = r"E:\Uni-Stuttgart\2nd\Introduction and Application of Programming in R\Project\combine\norms_freqs_adj.txt"
norms_freqs_adj = open(norms_freqs_adj_address, "r+")


'''extract the word list in norms' file'''
word_list = []
for line in norms.readlines():
    if line == None:
        break
    else:
        word = line.split()[1]
        word_list.append(word)
#print(word_list)


'''extract concrete's lines whose word exit in norms' word list'''
for line in concrete.readlines():
    if line == None:
        break
    else:
        word = line.split(b'\t')[0]
        if word in word_list:
            norms_concerte.write(line.decode('utf-8'))


'''extract freqs's lines whose word exit in norms' word list'''
dict_freqs = {}
for line in freqs.readlines():
    if line == None:
        break
    else:
        read = norms_freqs.readlines()
        word = line.split(b'\t')[0]
        if word in word_list:
            match = {}
            line = line.decode('utf-8')
            pos = str(re.findall(".*\t(.*)\t.*", line)).strip('[\'').strip('\']')
            freqs_num = str(re.findall(".*\t.*\t(.*)", line)).strip('[\'').strip('\']')
            #print(freqs_num)
            line = line.replace(pos, pos[0:1])
            #print(line)
            if pos[0:1] == 'N' or pos[0:1] == 'J':
                match[word.decode('utf-8') +'\t'+ pos[0:1]] = freqs_num
                #print(match)
                for k, v in match.items():
                    if k in dict_freqs.keys():
                        dict_freqs[k] += int(v)
                    else:
                        dict_freqs[k] = int(v)
#print(dict_freqs)

for k, v in dict_freqs.items():
    noun = 'N'
    adj = 'J'
    if noun in k:
        k = k.replace(noun, 'Noun')
        new_line_noun = k + '\t' + str(v) + '\n'
        norms_freqs_noun.write(new_line_noun)
    if adj in k:
        k = k.replace(adj, 'Adjective')
        new_line_adj = k + '\t' + str(v) + '\n'
        norms_freqs_adj.write(new_line_adj)
    new_line = k + '\t' + str(v) + '\n'
    #print(new_line)
    norms_freqs.write(new_line)


                
concrete.close()
norms.close()
freqs.close()
norms_concerte.close()
norms_freqs.close()
