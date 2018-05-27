# pd-0118

log: 

important: on 19:24 1/4/2018, i make a good improvement in mutate function. it is more random. data from before this fix is in data, data2, data3. it's better to use the data after this fix.

the fix is this: if the prob is x. the maximum it can be decreased is x itself.
the maximum it can be increased is 1 - x.
i random up to this number if i want to increase or decrease.


27/05/2018: i have made adjustments for the model to run in the lab at school. 

and other retro-update from later models :)

hence, at this point, that is the most updated model

i would update that into this

also, export png instead of pdf, pdf nang bo me

1. create a DELTAS list including all delta so that there is no need to calculate delta in each round of pair match
the function (interact-d au1 au2 rounds delta) -> (interact-d au1 au2)

in which (_ (in-range rounds)) -> (_ (in-list DELTAS))

as a consequence, the function match-population and evolve also lose 2 parameters

the configurations of the simulation are moved to cons.rkt file


2. check if the pic has its title of details -> added

3. continue the simulation from the previous simulation end -> added

because these are already done in the ndg-m-0418

4. i believe that there are other progressive updates from new simulaton settings that could be retro-update into old simulation settings, please check
for example, random-decimal

old: if prob = 0, return, else.. new: if n = 0, return, else..

5. probabily there are other minor updates such as new names, better workflow..

6. i believe the code can be restructured better

7. export pdf instead of png -> yes pdf is better for latex i believe (less weight)

dm pdf nang bo me -> doi lai

8. i think the code for pd and ndg would be diff in structure. for example, in pd, i would like to round the probability to 2 decimals but in ndg maybe just 1, the complexity is already enough.

9. also, pd does not require an automaton structure but ndg does bc too many outcomes

10. add delta to the output file name -> added

11. add location to output file name ->added
