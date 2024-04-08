# CS353-BankProcessing
The program was designed to simulate the processing of bank account transactions and statements. The program will read in two files: "ACCOUNTS.txt", which will contain a list of bank accounts, and "TRANSACTIONS.txt", which lists account transactions. After reading the files in, the application will produce the user an output file "STATEMENT.txt", consisting of the ending statement for each account. The output file will be formatted neatly to ensure readability. (sorta)

## References
[Racket/format](https://docs.racket-lang.org/reference/strings.html#%28mod-path._racket%2Fformat%29)

[Flonum](https://docs.racket-lang.org/reference/flonums.html)

[Regexp-split](https://docs.racket-lang.org/reference/regexp.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-split%29%29)

## Setup
1. Download the repo to your computer using whatever method you prefer. (e.g., curl -LO https://github.com/username/repo.git)
2. Open your IDE of choice (DrRacket)
3. Ensure that Both **ACCOUNT.TXT** and **TRANSACTIONS.txt** are located in the same folder as the program you are going to run.
4. Press Run on the IDE.
5. Return to the folder and open the new STATEMENTS.txt file. ~~(You may need to refresh the folder in order to see the document after pressing run)~~ This was fixed by #:exists 'replace

### To Do: 
1: Fix the rounding issue for Spacely Sprockets
2: Add transaction Line numbers ( #10001 )

### Console Instructions after file is ran
![Console Instructions after user runs the program](/ConsoleOutputForInstructions.png)

### Image of my copy of output file
![Image of the output file created after running the program](/StatementImage.png)
