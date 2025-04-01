# Gen AI dev overaching ideas

## Naive Hypothesises

> If it's hard for humans, it's hard for AI

HARD
Taking big code steps and then trying to debug after is difficult.

EASIER 
Small even tiny steps and verify they work.

HARD
Imperitive, side effecting, concurrent programs

EASIER
Simple functions that take values and return values

HARD
Bespoke syntaxes

EASIER
Simple regular syntax



> Gen AI has no problem generating programs that are HARD to reason about

LLMs will acrete complexity just like us. Producing programs that the
HUMANS and thus the LLMs will have a hard time thinking aobut.

Discerning programmers MUST be in the loop. Get the ROOMBA off the cat!

How long until the discerning programmers have lost their discernment?

The problem has always been: how do we produce high quality
maintainable code? 

This hasn't changed.

> Perhaps the Lisp REPL workflow that I have come to love is a solution.

A lifetime of coding has led me to this workflow and it has proven
itself over and over again to be very effective.

Not acting on files and working and only on a scratch pad in memory meaning faster iteration and verification feedback.

aside:
_No file patching needed during development. File patching and running tests suites comsume time and tokens._

> Tiny steps with High quality rich feedback is the recipe for the sauce

This rapid contextual feedback loop is important for LLMs and for programmers. 

The higher the quality of the feedback the higher the potential for
quality for HUMANS and LLMS.


For programmers, there are downsides of a iterating in a REPL on
things and getting the endorphins of evaluating code and realizing
that you had the wrong model of the problem. The probelm is getting
caught in the moment of creation and loosing the big picture. Time for
reflection is needed. This equally true for LLMs.

Hammock time and paper time is needed always.

> The granular steps in LLM repl iteration PRODUCES the LLM context you want. 


> LLM REPL driven workflow with high quality feedback provides
> Reinforcement learning (o1 Deepseek style) the examples it needs.

Perhaps smaller models that are constantly improved on with the
feedback from tiny REPL interaction are the way forward to customize
the model to you and what you are working on.

Also, the hybrid approach of calling non big models via a THINK tool
when a problem occurs.

This seems like a valid path for research.

> The REPL is closed over all Tools

AI can quickly build and dismantle tools as needed.

> Even if the machine doesn't need this workflow, I do.

Even if the machine doesn't need it, I DO. I need to see the steps and
the direction it takes.

I need the collaboration and to be present with the moment to moment ideation.

I need to be able to see the potential problems so that I can stop it
and exercise discernment so that I can keep the program in the space
of programs that can be reasoned about

> I need the collaboration TO be present. 

This is a much more humane workflow.


> The lack of experience in REPL driven development has led to a lack
> of insight into these possibilities.


## Speculation

The next AI PL may be a lisp with Idris or TLA+ qualities. The simple
syntax combined with enforced correctness and high degree of feedback.





