# Conquest Config Generator

This subdirectory is not part of Conquest proper.
It's the source for a utility meant to work with Conquest's config files.

It can (or will be able to):

 - Parse configs
 - Write configs to files (for now, only as a single file)
 - Manipulate configs. For now, this is extremely limited, but this will change.

# Spec

 - `;` and empty line: ignored, for comments and spacing

 - `(...)`: node, with one line for piece type on it (-1 for none),
 one line with the node type and one line for each id of a linked node.
 The piece type *must* be declared before any node that uses it
 (using nodes ahead of time is fine, as are piece types in a piece action).
 This should not be a problem if you do your piece types first,
 which is what I'd recommend regardless,
 so I'm not going to change it to allow using piece types before they're defined in nodes.

 - `{...}`: pieceType, with a couple lines of int prelude (`maxMoves`, `carryCap`),
 then split up into chunks by letters denoting what the action being defined is
 (the first letter of the name, lowercase, except for combo which is uppercase
 to avoid conflicting with carry). For actions that involve extra ints,
 put the `id` on one line and the int on the next. Node types (for travel)
 should be entered with the type as an ASCII value,
 not as a char because it's simpler this way to implement.
 The default state will be parsed as `false`; to set it to true,
 use a second copy of the letter. e.g. `{...kk \n c...}` will be able to kill anything.

 - `#...`: include the file named after `#`. You would not believe the stupid shit in the implementation that stems from this.

Indentation besides ` ` and `\t` is invalid. 
Trailing spaces may or may not also be invalid, depending on whether or not `atoi` (I know it sucks, but meh) can handle them.

Node ownership is unimplemented but will be part of the preamble portion in one form or another.

# Debugging tactics

 - Try commenting out everything and uncommenting single blocks.
 - Make sure each file includes only entire blocks and is included outside a block. Reasons to break this rule are few and far between.
 - Try removing indentation. Hey, sometimes it helps!