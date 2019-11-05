# For WaveCreator Assignment.

You should modify SinCreator.elm.

It is called from Main.elm, but do not modify Main.elm.  Your submission must compile against the existing modules, with your new SinCreator.elm dropped in.

Other modules can be referred to as examples.

Submit your code on Avenue, with your names and student numbers.

If you want to contribute your changes back to the main ShapeCreator, say so in a comment at the top of your module.

# Installing Dependencies:

elm init
elm install MacCASOutreach/graphicsvg

# Building:

elm make src/Main.elm

## Tasks
- [ ] create steps
- [*] initialize shape box with nothing
- [ ] add interface similar to shapecreator for the shapes
- [*] add highlighting to whatever is selected
- [*] take out second wave
- [ ] sin/cos toggle
- [ ] scale

## Norman's Principles to Change

### Constraints
- take out option to make it into 
- by new design we are constraining what the user can do

### Signifier
- add steps
1. modify wave amplitude, phase, frequency
2. pick animation
3. copy code

### Disoverability
- showing all the shape/anmation options instead of cycling through them with arrow buttons

### Conceptual Model
- model similar to other activities

### Feedback
- add a scale to show what is happeing

### Mapping
- just have one curve?

### Affordance
- the shape generator buttons affords the different animations you can generate

## Design Thinking Concepts
- put focus on the wave part
