# How Programmers Can Work with GenAI - General Ideas

## Naive Hypotheses

> If it's hard for humans, it's hard for AI

**HARD**  
Taking big code steps and then trying to debug after is difficult.

**EASIER**  
Small, even tiny steps and verify they work.

**HARD**  
Imperative, side-effecting, concurrent programs

**EASIER**  
Simple functions that take values and return values

**HARD**  
Bespoke syntaxes

**EASIER**  
Simple regular syntax

> Gen AI has no problem generating programs that are HARD to reason about

LLMs will accrete complexity just like us. Producing programs that HUMANS and thus the LLMs will have a hard time thinking about.

Discerning programmers MUST be in the loop. Get the ROOMBA off the cat!

How long until the discerning programmers have lost their discernment?

The problem has always been: how do we produce high quality maintainable code? 

This hasn't changed.

> Perhaps the Lisp REPL workflow that I have come to love is a solution.

A lifetime of coding has led me to this workflow and it has proven itself over and over again to be very effective.

Not acting on files and working only on a scratch pad in memory means faster iteration and verification feedback.

**Aside:**  
_No file patching needed during development. File patching and running test suites consume time and tokens._

> Tiny steps with rich feedback is the recipe for the sauce

This rapid contextual feedback loop is important for LLMs AND for programmers. 

The higher the quality of the feedback, the higher the potential for quality for HUMANS and LLMs.

For programmers, there are downsides of iterating in a REPL on things and getting the endorphins of evaluating code and realizing that you had the wrong model of the problem. The problem is getting caught in the moment of creation and losing the big picture. Time for reflection is needed. This is equally true for LLMs.

Hammock time and paper time is needed always.

> The granular steps in LLM REPL iteration PRODUCES the LLM context you want.

> GenAI code is like compiled code, do you want to maintain compiled code?

Generated programs are in dire need of higher level concise maintainable abstractions.

Languages that provide constraints (functional, immutable, restricted concurrency patterns) produce better GenAI programs because bad paths are not as available or idiomatic.

Languages that are constrained and provide higher level abstractions produce/generate more concise, more abstract programs that we can reason about at a higher level.

Clojure has a lot of potential here especially with contracts via Clojure Spec.

* The example of SEXP to HTML AST demonstrates this
  - Try generating this in JavaScript vs Clojure

> LLM REPL driven workflow with high quality feedback provides Reinforcement Learning (o1 Deepseek style) the examples it needs.

Perhaps smaller models that are constantly improved with the feedback from tiny REPL interactions are the way forward to customize the model to you and what you are working on.

Also, the hybrid approach of calling non-big models via a THINK tool when a problem occurs.

This seems like a valid path for research.

> The REPL is closed over all Tools

AI can quickly build and dismantle tools as needed.

> Even if the machine doesn't need this workflow, I do.

Even if the machine doesn't need it, I DO. I need to see the steps and the direction it takes.

I need the collaboration and to be present with the moment-to-moment ideation.

I need to be able to see the potential problems so that I can stop it and exercise discernment so that I can keep the program in the space of programs that can be reasoned about.

> I need the collaboration TO be present.

This is a much more humane workflow.

> Within collaboration we can and should be creating higher quality code

This is the ultimate goal after all.

And now I believe it's possible.

> LLMs are fantastic for quickly building tools tailored to giving feedback for your specific problem

> The lack of experience in REPL driven development has led to a lack of insight into these possibilities.

## Speculation

The next AI PL may be a Lisp with Idris or TLA+ qualities. The simple syntax combined with enforced correctness and high degree of feedback.
