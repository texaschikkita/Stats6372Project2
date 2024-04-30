# Stats6372Project2
Project 2.  Spring 2024.  Caleb Thornsbury, Rafia Mirza, Jessica McPhaul



revised files have been sent to everyone in the team with cleaned code, revised plots, iterations have replaced of 50 code lines to produce the same 23 plots. interactive informative plots have been added for each model and the models have been tidied up.   good work but it just needs to be be tighter and we need to be able to show we undersand what were doing and convery the progression.     please look over everything so you guys can present.   I will do introduction - introduce each of us with a few bylines to make a very quick burb.  I will introduce the dataset and how the study was conducted and why it's relevant.  I will then hand it off to Rafia.  rafia plan on spending 6 talking about the eda the intial steps, the data wrangling, the standardiazation why it was necessary and why after the first attempt at a glm we realized we needed to do further tweaking before we could delve into more advanced models.   Caleb spend 6 minutes finishing up the eda discusion, picking up where rafia left off, going into smote, pca, regularization and initial model selection based on aic/bic/f1 and the 6 relevant metrics  outlined previsously and in the project file.   I have included and sent you guys entirely too many plots to choose from with explanations.   Pick a couple that suit your fancy, and all you have to do is literally read verbatim.   If you have questions hit me up.   Just please go over everything so that the comprehension level is equal amongst the 3 of us.  

then rafia spend about 4 minutes going over your 'advanced' model approach.   I don't want to overstep and if I did my aplogies.   The polynomial model needed a bit a tlc.  I included a quadratic and a cubic model along with the glm you selected as your base model and created comparisons.  I sent you images and talking points.    Don't feel like you have to do what I suggest.  LOL   I just worked hard and want an A. 

caleb then you spend about 4 minutes going over your lda/qda model.  I made a few tweaks, but there are some inconsistencies in the coding and i don't want to make assumptions on what you're trying to do.   The numbers don't look right.  But I could be misinterpereting it.   Note that if we have a hard time making sense of the numbers no way Turner is going to let it fly .Smiling isn't his thing, but let's try to keep his diatribe of harsh critiqueto a minimum.  He's incredible intuitive and can see right through when someone tried to blow past a point that is best left under the radar.  He'll sniff it out and that'll be the thing he get killed on.

I sent you a revised model with plots and talking points.   Use it   don't use it.  You're smart  and I know you're more than capable plus youre mister biotech science guy.   You're younger and smarter than me, just make sure the model makes sense and the numbers are in line with everything else.    Theres something that's just not quite right about it.  I have a guess as to what it is, but I need you to review the EDA and go over it thoroughly so you can see what I'm talking about.   Also, I could be wrong in which case I need your eyes and brain doubly.  

Lastly I'll finish up.  I'll need about 10 minutes to go over everything.  the cleaning, the rfe the initial testing, the params for xgboost, resplitting data for rf, feature selection from rf, resplitting again for test set to compare and test data then again starting with data set resplitting resetting params and creating ensemble model combining extreme grafient with random forest.  I will need to explain the methodolfy behind each model and why ensemble was produced the best results.  Then go over the results the roc curves and metrics for the models I did and how they stack up to the initial model, the LDA/QDA and the polynomial models.   Then I'll wrap it up swing back to you guys see if you have any closing comments or things to add.



Presenting in this fashion is more engaging and shows a collaboration.   Versus just a b c done - this makes it more interesting and we each bring different qualities to the group.   

me 2 min
rafia 5.5 min
caleb 5.5 min
rafia  3.5 min
caleb 3.5 min
me 10  min.  

If you guys need more or less time let me know.  I am going to try to get my final presentation down to 8 minutes so we don't go over.  he wants 30 minutes max.  this time frame gives us 30 minutes exactly. I will cut back and consolidate.  Granted I'm sure whatever part I happen to leave out will be the exact part he critiques and questions.  Thats just how it goes.      However if we can provide a high level and a detailed overview that can be understood by "him" then we've met our objectives.   When I say "him" I mean, I think we should appraoch the presentation as if he is perhaps an investor who funded the research and we are explaining to him the finding and how they can be interpereted in the scope of his objective.   However he's still better at stats than we will probably ever be and we need to drive the project objectives him while still presenting in a way that isn't us just lecturing other staticians. ( i use that term loosely as I am not even remotely close to a statician).  

---

OK then!


I am open to suggestions and if there is any strong opposition to this methodology let me know.   I apologize for being a control freak.  I am trying.   It's a group project and I value you both of you as friends and colleagues.    Rafia thanks so much for being so dang organized.  If you can just be my project manager for everthing we could dominate.    I'll handle the sitting in a tunnel with gremlins doing the tech stuff - you can be the smiling organized face that has slide deck templates ready to go before ive even opened and IDE and you have such a calm and pleasant demeanor when talking.   I'm like the mad scientist that barely knows english, and sadly thats the only language I know.     

If it were up to me I wouldn't present at all.   LOLOL.  I'm so socially awkward and feel like I dont know how to talk without stammering when I present.  It's like all of a sudden I don't know how to read or how pronounce words that have more than 2 syllables.   

Caleb you're confident and you present with an aire of authority which is great.  You're inteligent and genuince and it shows when you present.    We have a great team. Now let's go get an A! 



*****
the revised scripts are knitted to word since yall fancy word, they're also knitted to pdf, the models are saved as rds and rdata and the notebooks have been cached so you dont even have to run it really.  just import from the folder i sent to each of you.  That being said running it won't take but a minutes the models are relatively simple and aren't time / rescource conducive.    There are static plots and interactive depending on how we want to compile the presentation.   Just let me know when that part is ready and I'll drop my slides in.   I'm leaving the slide deck for you guys. Rafia already has 19 different custom designed options ready for us to choose.   Caleb pick one out.  I think they're all great and if Im being honest, I deleted PPT accidentally and haven't even bothered to redownload it.  I was just gonna do it in r slidy presentation but for obvious reasons thats a ABSOLUTELY not from the tem.  Besides Rafia's slide deck is gonna be amazing I already know! I'll grab it from Box when it's ready and we can each drop our slides in. 


------------------

   Let me know if you guys need me to knit anything else.      I am pulling the R code from the files and making one script for the appendix.  I'm gonna streamline it with a note and a link to the full files in case prof wants to see the entirety of the code.  

