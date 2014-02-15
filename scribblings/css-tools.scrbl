#lang scribble/manual

@(require scribble/eval (for-label racket "../main.rkt"))

@(define my-eval (make-base-eval))
@(my-eval `(require css-tools))


@title{css-tools}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

A collection of little functions that help make Racket code more readable.

@section{Installation & updates}

At the command line:
@verbatim{raco pkg install css-tools}

After that, you can update the package from the command line:
@verbatim{raco pkg update css-tools}


@section{Interface}

@defmodule[css-tools]

Hello css-tools.


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/css-tools"]{http://github.com/mbutterick/css-tools}. Suggestions & corrections welcome.

