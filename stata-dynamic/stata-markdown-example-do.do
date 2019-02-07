* This do file goes with the stata-markdown-example.stmd file.
* This will compile the file in Stata.
* To start the entire process, you can create your .stmd file.
* I did this as follows:
* open terminal,
* Then use 
* `cat > stata-markdown-example.stmd`
* `[Type in content for the file.]`
* Press Ctrl-D to exit vim.
* Then you can start writing the .stmd file in a text editor.
* All the real action takes place in the .stmd file.
* A .do file like this is just to run the compilation command
* with any options that you prefer:

* Set working director to wherever the .stmd file lives:
cd ~/documents/github/cds-demos/stata-dynamic

* Since I am compiling to PDF via Tex, I need the Tex style file:
copy https://www.stata-journal.com/production/sjlatex/stata.sty stata.sty

* Compile the .stmd file to output format of your choosing 
* here it is a PDF via Tex. I am also asking to keep the
* Tex file:
markstat using stata-markdown-example.stmd, pdf strict keep(tex)

* End.
