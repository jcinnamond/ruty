# ruty

ruty is an automated ruby typer for emacs. It is designed for
recording screencasts involving a lot typing of ruby code. Rather than
rely on a bag of meat to type accurately, it allows you to prepare the
content in advance and then let emacs do the typing for you.

ruty tries to look natural by pausing briefly between each character
inserted. The length of the pauses is randomised, with occasional
longer pauses.

In theory ruty can type any content for you, but it has been designed
to recognise ruby code. It looks for some ruby structures -- such as
`class ... end` or `def ... end` -- and behaves a bit like yasnippet
or similar. I.e., after it finished typing the keyword `class` it
'expands' the content to insert `end` on the following line before
continuing with the typing. This was to avoid typing lots of `end`s at
the end of a class definition. That doesn't make for an appealing
screencast.

# Usage

Put ruty.el somewhere into your emacs load path and require it. There
are two main functions:

`ruty/retype-buffer` clears the current buffer and retypes its content.

`ruty/insert-buffer` prompts for the name of another buffer, reads the
content and inserts it at the current point.

You might need to increase the defaults of `max-lisp-eval-depth` and
`max-specpdl-size`. For example, I have the following code in my emacs
config:

    (setq max-lisp-eval-depth 10000)
    (setq max-specpdl-size 10000)

# Licence

Copyright 2016 John Cinnamond.

Distributed under an MIT licence. See the file LICENSE for more
information.
