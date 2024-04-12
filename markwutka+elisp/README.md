# Advent of Code 2023 in Emacs Lisp

Having done AoC in Common Lisp before, I thought it would be interesting to try
Emacs Lisp so that I could get more familiar with it.

I did not include my data files here, but each program expects there to be a data
directory and each day's data file should be name day_N_.txt (e.g. day1.txt).

To run a file, just load it into Emacs and do `M-x eval-buffer` (alt-X or option-X).
You can also go to the Emacs-Lisp menu and select "Evaluate Buffer". The programs write
their result into the message window at the bottom, which is also visible in the *Messages*
buffer.

Each file so far is self-contained, using only functions defined within the file, or that
are part of Emacs Lisp itself. They should all work in a vanilla implementation of Emacs.

## Implementation Notes
When doing AoC in common lisp, I almost always used a function to read in the file
as a list of lines. I made the equivalent Emacs Lisp function here too, and some of the
solutions use it. But, with Emacs Lisp, you generally deal with buffers - files loaded into
memory, and sometimes it's just as easy to edit the buffer and then evaluate it as a
Lisp expression, as I do for day1, or just work with buffer directly because you have to
work with the data in 2 dimensions, as I do in day3.

When I read the file as a list of lines, I generally use a more functional style, and
often just use regular expressions or string-split to parse the file. There are a few
times where I have to update a variable because of the way I am reading a regular expression,
or if I need to iterate through all the entries in a hash table, but in those cases, the
variable manipulation is limited to a local variable that isn't visible outside the function
so it's not terribly non-functional.


