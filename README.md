# PR
This is a very simple interpreter for primitive recursiv functions (PR). 

## Usage
Instead of giving the syntax, here is an example:
```
p = Pr{c[0,0], pr[2,1]};
add = Pr{pr[1,1], s . pr[3,3]};
(p . add) (2,2)
```