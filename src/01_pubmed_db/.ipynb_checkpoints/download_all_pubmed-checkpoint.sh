#  apt-get install curl

#wget ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/README.txt
#curl ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/README.txt -o /scratch/kb7hp/pubmed/README.txt
wget ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/README.txt

# tested and this works like a charm 
wget --recursive --no-parent ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/


#ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline
#https://jasonjwilliamsny.github.io/importingdata-genomics/ p
#https://manpages.debian.org/buster/curl/curl.1.en.html
#https://www.cyberciti.biz/faq/linux-delete-all-files-in-directory-using-command-line/ 
#https://serverfault.com/questions/25199/using-wget-to-recursively-download-whole-ftp-directories