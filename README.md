# pd-0118

log: 

important: on 19:24 1/4/2018, i make a good improvement in mutate function. it is more random. data from before this fix is in data, data2, data3. it's better to use the data after this fix.

the fix is this: if the prob is x. the maximum it can be decreased is x itself.
the maximum it can be increased is 1 - x.
i random up to this number if i want to increase or decrease.


# pd-0118
