#lang scribble/manual

@(require scribble/eval (for-label racket "../main.rkt"))

@(define my-eval (make-base-eval))
@(my-eval `(require chirp))


@title{chirp}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

Chirp = CSS & HTML Racket preprocessor.

@section{Installation & updates}

At the command line:
@verbatim{raco pkg install chirp}

After that, you can update the package from the command line:
@verbatim{raco pkg update chirp}


@section{Interface}

@defmodule[chirp]

Hello chirp.


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/chirp"]{http://github.com/mbutterick/chirp}. Suggestions & corrections welcome.

