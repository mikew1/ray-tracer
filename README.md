# Simple ray tracer

This is the code from Paul Graham's ANSI Common Lisp, tidied with my comments plus a few tweaks.

In particular, I added the ability to move the light source.

# Output

This is the basic output with the world as per the book, at 4x scale to be more impressive.

![screen shot canvas](/output-png/spheres-4.png?raw=true "")

Emphasis of this code is on simplicity and correctness, not speed.

Note though, [Ray tracing](https://en.wikipedia.org/wiki/Ray_tracing_(graphics)) in general is very amenable to parallelisation because the calculation
of each ray can be considered computationally distinct. This code is many thousands of times slower than it could be done on your GPU.

We can pan the eye position left which is quite trivial:

![screen shot canvas](/output-png/spheres-eye-left-10.png?raw=true "")
![screen shot canvas](/output-png/spheres-eye-left-20.png?raw=true "")
![screen shot canvas](/output-png/spheres-eye-left-30.png?raw=true "")
![screen shot canvas](/output-png/spheres-eye-left-40.png?raw=true "")

But moving the light source is quite nice (this is the bit I added to the code in the book).

Here the light source is moved left by log scale increments:

![screen shot canvas](/output-png/spheres-light-source-left-100.png?raw=true "")
![screen shot canvas](/output-png/spheres-light-source-left-200.png?raw=true "")
![screen shot canvas](/output-png/spheres-light-source-left-400.png?raw=true "")
![screen shot canvas](/output-png/spheres-light-source-left-800.png?raw=true "")

Personal next steps? I plan to check back in on this code after reading 
[7 Concurrency Models](https://pragprog.com/book/pb7con/seven-concurrency-models-in-seven-weeks).

---
#### Running the code

Obviously there's no system definition (`.asd` file) here, all I was doing is evaluating
each source file directly into a REPL. Be warned it does take about a minute on my 2015 macbook
pro to generate an image even at the smallest size. 

There is more info in comments in the source files.
