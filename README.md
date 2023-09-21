# Stack Templation

This hosts my personal stack templates and a command line utility to create/modify them.

> And lead us not into templation.
>
> -- The Lord's Prayer

## Installation

    cd templation
    stack install

## Creating a template

You need to be in the top of the stack based project
you wish to turn into a template.

    templation

Redirect the output to a `.hsfiles` in your stack templates
project.  You are done.

## Using the template

    stack install -p author:"Your Name" -p email:your@email.com your-new-project grmble/default

Substitue grmble for your github.
