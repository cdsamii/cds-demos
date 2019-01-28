* This do file goes with the stata-markdown-example.stmd file.
* This will compile the file in Stata.
* To start the entire process, you can create your .stmd file.
* I did this as follows:
* open terminal,
* Then use 
* `cat > stata-markdown-example.stmd`
* `[Type in content for the file.]`
* Press Ctrl-D to exit the vim text editor.

* Now, everything you will do will take place in the .stmd file.
* So a do file like this isn't really necessary, as all it would do 
* would be to run simple commands like this:

cd ~/documents/github/cds-demos/stata-dynamic
copy https://www.stata-journal.com/production/sjlatex/stata.sty stata.sty
markstat using stata-markdown-example.stmd, pdf strict keep(tex)

* These could of course just be run out of the command line
* with no real loss in reproducibility.
