# PdD - Read Me

In this folder you find the scripts I wrote for my PhD project. These scripts read out .TextGrid-files, re-organize tables, sort, create and alter files. Most of these scripts are non-modular. I am currently working on modular, beautified versions of the scripts.

This is a list of files in this folder:

**1. Cam_textgrids_auslesenWithConsonant.py:** <p>This script reads in textgrids and gets information out of them.The items annotated in the textgrids are prefixed suffixed words.

**2. CamExp_measure_dur_4.py:** 		 <p><p>This script reads in textgrids and gets information out of them.The items annotated in the textgrids are prefixed suffixed words. The info is gets is different from (1).


**3. checking_items.py:**    <p>This program extracts 10 % of thhe items in each folder of the fertige textgride of the same date and saves them in a new folder (Checking Items). It randomly selects 10 textgriuds and teh matching sound files from each folder (containing 100 items).

**4. copy_and_sort_sound_files_make_textgrids.py:**<p> This script reads out basic textrgrids, i.e. files which contain the transcription of a sound file, identifies which kind of sentence it is, i.e. identifies whether it is one beloning to the experiment, sorts the files accordingly and generated new TextGrid-files with the desired structures.
  
**5. exp_part1_fillers_same_names_01-10-2015.py:** <p> This program takes a list of items which were chosen to be included in a speech production experiment, divides the data in two sets of similar distribution (items belong to different classes), It then embedds one set into accented, the other set into unaccented carrier sentences.There are three types of different unaccented and accented carrier sentences in which the items are embedded. 
Then all sentences imported are put into one list which is given out as a txt file (when the defintion exp_sentence is used). The sentences in this list are formatted in such a way, that one can copy the list into Latex (Beamer).Each carrier sentence(s) will be displayed on one slide. exp_sentences gives out two txt-files, which both use the same sets-the items which are embedded in accented sentences in the one txt-file, are in unaccented position in the other textfile.
Furthermore, the program creates a csv which records which items is in which condition (accented, unaccented) in each experiment.

**6. presentation_maker_2_versions_reading_exp_Cambridge_2016.py:** <p> Same as the program in 5 but here different names are used in the experimental carrier sentences.
  
**7. read_compound_constituent_dur_s:**<p>This script reads in textgrids and gets information out of them.The items annotated in the textgrids are compounds.

**8. read_compound_gem_dur3_n.py:** <p>This script reads in textgrids and gets information out of them.The items annotated in the textgrids are compounds (the items annotated differ between 1 and 2).

**9. transform_ratings_tables_in_un.py:** <p> This script transforms data attained form from lime survey into another formar, i.e. it transposes some rows, deletes some etc. Then it megres all the different tables with a previous table with additional info.
